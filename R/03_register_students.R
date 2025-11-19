################################################################################
# Program: 03_register_students.R
# Purpose: Creates a synthetic student body with personas and attributes, then
#          simulates a course registration period. Students are enrolled one
#          by one, respecting course availability and time conflicts.
# Inputs: ./data/master_schedule.rds
# Outputs: ./data/student_roster.rds
#          ./data/student_enrollment.rds
# Created: 7NOV2025 (IEC)
# Modified: 18NOV2025 (IEC)
################################################################################

# --- 1. Load Libraries and Data ---
library(tidyverse)
library(here)

message("Loading master course schedule from '02_build_semester_schedule.R'...")
master_schedule <- readRDS(here::here("data", "master_schedule.rds"))

# --- 2. Configuration & Simulation Parameters ---
message("Setting simulation parameters...")
set.seed(2025)

N_TOTAL_STUDENTS <- 1850
N_THESIS_SENIORS <- 250
class_year_dist <- c(FirstYear = 0.28, Sophomore = 0.26, Junior = 0.25, Senior = 0.21)

# --- 3. Create Student Roster ---
# (This part is unchanged)
message("\nPart A: Generating the synthetic student roster...")

student_roster <- tibble(
  StudentID = 1:N_TOTAL_STUDENTS,
  ClassYear = sample(names(class_year_dist), size = N_TOTAL_STUDENTS, replace = TRUE, prob = class_year_dist)
)
available_majors <- master_schedule %>% filter(!Department %in% c("FYS", "INDS", "EXDS")) %>% pull(Department) %>% unique()
student_roster <- student_roster %>%
  mutate(Major = if_else(ClassYear == "FirstYear", "Undeclared", sample(available_majors, n(), replace = TRUE)))
personas <- c("STEM Scientist", "Humanities Scholar", "The Artist", "The Athlete", "Well-Rounded")
student_roster <- student_roster %>%
  group_by(Major) %>%
  mutate(Persona = sample(personas, n(), replace = TRUE)) %>% ungroup()
student_roster <- student_roster %>%
  mutate(
    prob_f = case_when(Persona %in% c("Humanities Scholar", "The Artist") ~ 0.60, Persona == "STEM Scientist" ~ 0.40, TRUE ~ 0.52),
    Gender = if_else(runif(n()) < prob_f, "F", "M")
  ) %>% select(-prob_f)
athlete_dist_by_year <- c(FirstYear = 0.30, Sophomore = 0.28, Junior = 0.24, Senior = 0.20)
student_roster <- student_roster %>%
  group_by(ClassYear) %>%
  mutate(
    num_athletes_in_year = round(n() * athlete_dist_by_year[first(ClassYear)]),
    is_athlete = row_number() %in% sample(1:n(), size = first(num_athletes_in_year))
  ) %>% ungroup() %>% select(-num_athletes_in_year)
athlete_genders <- sample(c("F", "M"), size = sum(student_roster$is_athlete), replace = TRUE)
student_roster$Gender[student_roster$is_athlete] <- athlete_genders
senior_ids <- student_roster %>% filter(ClassYear == "Senior") %>% pull(StudentID)
thesis_senior_ids <- sample(senior_ids, N_THESIS_SENIORS)
student_roster <- student_roster %>%
  mutate(is_thesis = if_else(ClassYear == "Senior", StudentID %in% thesis_senior_ids, FALSE))

message("...Done. Saving 'student_roster.rds'.")
saveRDS(student_roster, here::here("data", "student_roster.rds"))

# ==============================================================================
# PART B: THE ENROLLMENT ALGORITHM
# ==============================================================================

message("\nPart B: Running the enrollment simulation...")
courses <- master_schedule %>% mutate(SeatsAvailable = EnrollmentCap)

message("Pre-processing course schedule for fast conflict checking...")
courses_long <- courses %>%
  filter(!is.na(Days) & Days != "N/A") %>%
  select(CourseID_Section, Days, StartTime, EndTime) %>%
  separate_rows(Days, sep = "") %>%
  rename(Day = Days)

check_schedule_conflict <- function(new_course_id, student_schedule) {
  if (is.null(student_schedule) || nrow(student_schedule) == 0) return(FALSE)
  new_course_times <- courses_long %>% filter(CourseID_Section == new_course_id)
  if(nrow(new_course_times) == 0) return(FALSE) # Non-scheduled courses (Ind. Study) can't conflict
  conflict <- student_schedule %>%
    inner_join(new_course_times, by = "Day") %>%
    filter(StartTime.x < EndTime.y & EndTime.x > StartTime.y)
  return(nrow(conflict) > 0)
}

message("PASS 1: Enrolling students via primary 'shopping' period...")
student_ids_ordered_for_registration <- student_roster %>%
  arrange(factor(ClassYear, levels = c("Senior", "Junior", "Sophomore", "FirstYear")), StudentID) %>%
  pull(StudentID)

all_enrollments <- list()
for (id in student_ids_ordered_for_registration) {
  current_student_schedule <- NULL
  student_enrollments <- tibble()
  num_courses_needed <- if_else(id %in% thesis_senior_ids, 3, 4)
  
  potential_courses <- courses %>% filter(SeatsAvailable > 0) %>% sample_n(n())
  
  for (i in 1:nrow(potential_courses)) {
    if (nrow(student_enrollments) >= num_courses_needed) break
    course_to_try <- potential_courses[i, ]
    has_conflict <- check_schedule_conflict(course_to_try$CourseID_Section, current_student_schedule)
    
    if (!has_conflict) {
      student_enrollments <- bind_rows(student_enrollments, tibble(StudentID = id, CourseID_Section = course_to_try$CourseID_Section))
      new_course_times <- courses_long %>% filter(CourseID_Section == course_to_try$CourseID_Section)
      current_student_schedule <- bind_rows(current_student_schedule, new_course_times)
      courses$SeatsAvailable[courses$CourseID_Section == course_to_try$CourseID_Section] <- courses$SeatsAvailable[courses$CourseID_Section == course_to_try$CourseID_Section] - 1
    }
  }
  all_enrollments[[as.character(id)]] <- student_enrollments
}
enrollment_df <- bind_rows(all_enrollments)
message("...Primary enrollment pass complete.")


# --- NEW: PASS 2 - Backfill incomplete schedules ---
message("PASS 2: Backfilling students with incomplete schedules...")

# Identify students who are short on courses
enrollment_counts <- enrollment_df %>% count(StudentID, name = "CoursesTaken")
students_to_backfill <- student_roster %>%
  left_join(enrollment_counts, by = "StudentID") %>%
  replace_na(list(CoursesTaken = 0)) %>%
  mutate(CoursesNeeded = if_else(is_thesis, 3, 4)) %>%
  filter(CoursesTaken < CoursesNeeded)

if (nrow(students_to_backfill) > 0) {
  message(" -> Found ", nrow(students_to_backfill), " students to backfill. Running secondary enrollment...")
  
  for (i in 1:nrow(students_to_backfill)) {
    student_info <- students_to_backfill[i, ]
    id <- student_info$StudentID
    
    # Rebuild this student's current schedule to check against
    enrolled_course_ids <- enrollment_df %>% filter(StudentID == id) %>% pull(CourseID_Section)
    current_student_schedule <- courses_long %>% filter(CourseID_Section %in% enrolled_course_ids)
    
    courses_to_take <- student_info$CoursesNeeded - student_info$CoursesTaken
    
    backfill_enrollments <- tibble()
    
    # Keep trying until their schedule is full
    while(nrow(backfill_enrollments) < courses_to_take) {
      # Find all currently available, non-conflicting courses
      potential_courses <- courses %>% filter(SeatsAvailable > 0)
      
      available_course_ids <- potential_courses$CourseID_Section
      
      # Vectorized conflict check for speed
      conflicts <- sapply(available_course_ids, check_schedule_conflict, student_schedule = current_student_schedule)
      
      valid_courses <- potential_courses %>% filter(!conflicts)
      
      if (nrow(valid_courses) == 0) {
        message(" -> Warning: No non-conflicting courses with open seats found for StudentID ", id, ". Could not backfill completely.")
        break # Break the while loop if truly no options are left
      }
      
      # Pick one valid course and enroll
      course_to_add <- slice_sample(valid_courses, n = 1)
      
      backfill_enrollments <- bind_rows(backfill_enrollments, tibble(StudentID = id, CourseID_Section = course_to_add$CourseID_Section))
      
      # Update schedule and seat counts for the next iteration
      new_course_times <- courses_long %>% filter(CourseID_Section == course_to_add$CourseID_Section)
      current_student_schedule <- bind_rows(current_student_schedule, new_course_times)
      courses$SeatsAvailable[courses$CourseID_Section == course_to_add$CourseID_Section] <- courses$SeatsAvailable[courses$CourseID_Section == course_to_add$CourseID_Section] - 1
    }
    
    # Add the newly backfilled courses to the main enrollment data frame
    enrollment_df <- bind_rows(enrollment_df, backfill_enrollments)
  }
} else {
  message(" -> All students fully enrolled in the first pass. No backfill needed.")
}


# ==============================================================================
# --- POST-PROCESSING: GUARANTEED THESIS ENROLLMENT ---
# ==============================================================================
# (This part is unchanged)
message("\nAssigning guaranteed thesis enrollments for seniors...")
thesis_students <- student_roster %>% filter(is_thesis == TRUE) %>% select(StudentID, Major)
thesis_courses <- master_schedule %>% filter(str_detect(CourseCode, "457")) %>% select(Department, CourseID_Section)
thesis_enrollments_df <- thesis_students %>% left_join(thesis_courses, by = c("Major" = "Department"), relationship = "many-to-many")
successful_assignments <- thesis_enrollments_df %>% filter(!is.na(CourseID_Section)) %>% group_by(StudentID) %>% sample_n(1) %>% ungroup() %>% select(StudentID, CourseID_Section)
failed_assignments <- thesis_students %>% filter(!StudentID %in% successful_assignments$StudentID)

if (nrow(failed_assignments) > 0) {
  message("Warning: ", nrow(failed_assignments), " thesis students found no match in their major. Assigning a random thesis.")
  all_thesis_ids <- thesis_courses %>% pull(CourseID_Section)
  fallback_assignments <- failed_assignments %>% mutate(CourseID_Section = sample(all_thesis_ids, size = n(), replace = TRUE)) %>% select(StudentID, CourseID_Section)
  final_thesis_enrollments <- bind_rows(successful_assignments, fallback_assignments)
} else {
  final_thesis_enrollments <- successful_assignments
}

# Combine all enrollments, including backfilled and thesis
final_enrollment_df <- bind_rows(enrollment_df, final_thesis_enrollments)

# --- Final Sanity Check ---
expected_enrollments <- (N_TOTAL_STUDENTS - N_THESIS_SENIORS) * 4 + (N_THESIS_SENIORS * 3)
total_enrollments <- enrollment_df %>% count(StudentID) %>% nrow()
message(paste("\nSanity Check: Expected Non-Thesis Enrollments:", expected_enrollments, ". Actual Non-Thesis Enrollments:", nrow(enrollment_df)))
message(paste("Sanity Check: Total Final Enrollments (including thesis):", nrow(final_enrollment_df)))

message("...Done. Saving 'student_enrollment.rds'.")
saveRDS(final_enrollment_df, here::here("data", "student_enrollment.rds"))

message("\n--- SCRIPT EXECUTION COMPLETE ---\n")