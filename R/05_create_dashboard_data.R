#####################################################################
# Program: 05_create_dashboard_data.R
# Purpose: This script processes raw simulated data and aggregates it into a
#          single, unified time-series dataset for dashboarding.
# Inputs:  All .rds files from the ./data/ directory.
# Outputs: ./data/dashboard_data.rds
# Created: 7NOV2025 (IEC)
# Modified: 19NOV2025 (IEC) 
#####################################################################

# --- 1. SETUP: LOAD LIBRARIES ---
library(tidyverse)
library(lubridate)
library(here)
library(hms)

SIM_WEEK_START <- ymd("2025-10-20", tz = "America/New_York")
message("Libraries loaded and constants defined.")


# --- 2. DEFINE METADATA FOR LOCATIONS ---
location_coords <- tribble(
  ~Location,                      ~loc_x, ~loc_y,
  "HAMPTON HALL",                 2,      1,
  "PURNELL SCIENCE CENTER",       2,      2,
  "CALDWELL HALL",                3,      3,
  "MERRICK AUDITORIUM",           4,      3,
  "PRESCOTT HALL",                4,      4,
  "THORNTON HALL",                2,      3,
  "CHANDLER ACADEMIC CENTER",     3,      4,
  "STERLING-MEAD PERF. ARTS",     3,      5,
  "WHITMAN THEATER",              1,      4,
  "WHITEHILL ATHLETIC CENTER",    5,      3,
  "Alumni Commons",               5,      4,
  "Gus's",                        3,      2,
  "The Burrow",                   1,      3
)
message("Metadata for location coordinates defined.")


# --- 3. CALCULATE DINING OCCUPANCY ---
message("\n--- Starting Part 3: Calculating Dining Occupancy ---")

dining_swipes <- readRDS(here::here("data", "dining_swipes.rds"))

# Create exit times based on entry (30 minutes for breakfast and lunch, 45 for dinner)
dining_activity <- dining_swipes %>%
  mutate(
    swipe_hour = hour(Timestamp),
    swipe_exit_time = case_when(
      (swipe_hour >= 7 & swipe_hour < 10)  ~ Timestamp + minutes(30),
      (swipe_hour >= 11 & swipe_hour < 14) ~ Timestamp + minutes(30),
      (swipe_hour >= 17 & swipe_hour < 20) ~ Timestamp + minutes(45),
      TRUE                                 ~ Timestamp + minutes(30)
    ),
    interval_start = floor_date(Timestamp, "15 minutes"),
    interval_end = floor_date(swipe_exit_time, "15 minutes"),
    Timestamp_15min = map2(interval_start, interval_end, ~seq(.x, .y, by = "15 min"))
  ) %>%
  unnest(Timestamp_15min) %>%
  select(Timestamp_15min, Location = DiningHall) %>%
  count(Timestamp_15min, Location, name = "Occupancy") %>%
  mutate(ActivityType = "Dining")

message(paste("Dining activity processed. Generated", nrow(dining_activity), "aggregated time-series rows."))


# --- 4. CALCULATE ACADEMIC OCCUPANCY ---
message("\n--- Starting Part 4: Calculating Academic Occupancy ---")

student_enrollment <- readRDS(here::here("data", "student_enrollment.rds"))
master_schedule <- readRDS(here::here("data", "master_schedule.rds"))

academic_base <- student_enrollment %>%
  inner_join(master_schedule, by = "CourseID_Section")

academic_activity <- academic_base %>%
  filter(!is.na(Days) & Days != "N/A" & !is.na(StartTime)) %>%
  mutate(
    Department = case_when(
      Department %in% c("AVC", "AMAV", "AVAS", "AVGS") ~ "ARTS",
      Department %in% c("CHI", "ASCI") ~ "CHIN", Department %in% c("CMS", "CMDN", "CMGS") ~ "CLAS",
      Department %in% c("DANC", "DNMU") ~ "DANC", Department == "ENG" ~ "ENGL",
      Department == "FRE" ~ "FREN", Department == "GER" ~ "GERM", Department == "GRK" ~ "GREK",
      Department == "JPN" ~ "JAPN", Department %in% c("MUS", "DCMU") ~ "MUSI",
      Department %in% c("REL", "ASRE", "CMRE") ~ "RELI", Department == "RFSS" ~ "RHET",
      Department %in% c("RUS", "EURU" ) ~ "RUSS", Department %in% c("HISP", "AFHI", "GSSP", "LSSP") ~ "SPAN",
      Department %in% c("THEA", "ENTH") ~ "THEA", Department %in% c("BIO", "BIEA", "BIES") ~ "BIOL",
      Department == "DCS" ~ "COSC", Department %in% c("EACS", "EAES") ~ "EESC",
      Department %in% c("MATH", "DCMA", "MAPH") ~ "MATH", Department == "NRSC" ~ "NEUR",
      Department %in% c("ANTH", "AMAN") ~ "ANTH", Department %in% c("EDUC", "EDGS") ~ "EDUC",
      Department %in% c("HIST", "ASHI", "CMHI", "EUHI") ~ "HIST", Department == "PLTC" ~ "POLS",
      Department %in% c("PSYC", "NSPY") ~ "PSYC", Department %in% c("SOC", "PTSO") ~ "SOCI",
      Department %in% c("AFR", "AFES") ~ "AFAM", Department == "EUS" ~ "EUST",
      Department %in% c("GSS", "AVMU", "GSPT") ~ "WGSS", TRUE ~ Department
    )
  ) %>%
  separate_rows(Days, sep = "") %>%
  filter(Days %in% c("M", "T", "W", "R", "F", "h")) %>%
  mutate(
    DayOfWeek_Num = case_when(
      Days == "M" ~ 0, Days == "T" ~ 1, Days == "W" ~ 2,
      Days %in% c("R", "h") ~ 3, Days == "F" ~ 4
    ),
    ClassDate = SIM_WEEK_START + days(DayOfWeek_Num)
  ) %>%
  # THE DEFINITIVE FIX: Use hm() to parse the "HH:MM" character string
  # and add the resulting period object to the date.
  mutate(
    start_datetime = ClassDate + hm(StartTime),
    end_datetime = ClassDate + hm(EndTime)
  ) %>%
  filter(!is.na(start_datetime), !is.na(end_datetime)) %>%
  mutate(
    interval_start = floor_date(start_datetime, "15 minutes"),
    interval_end = floor_date(end_datetime - seconds(1), "15 minutes"),
    Timestamp_15min = map2(interval_start, interval_end, ~seq(.x, .y, by = "15 min"))
  ) %>%
  unnest(Timestamp_15min) %>%
  group_by(Timestamp_15min, Location = BuildingName, Department) %>%
  summarise(Occupancy = n(), .groups = "drop") %>%
  mutate(ActivityType = "In Class")

message(paste("Academic activity processed. Generated", nrow(academic_activity), "aggregated time-series rows."))


# --- 5: ENRICH AND FINALIZE DATA ---
message("\n--- Starting Part 5: Enriching and Finalizing Data ---")

academic_activity_final <- academic_activity %>%
  mutate(
    Department = case_when(
      Department == "ARTS" ~ "Art & Art History", Department == "ASIA" ~ "Asian Studies",
      Department == "CHIN" ~ "Chinese", Department == "CLAS" ~ "Classics & Medieval Studies",
      Department == "DANC" ~ "Dance", Department == "ENGL" ~ "English", Department == "FREN" ~ "French",
      Department == "GERM" ~ "German", Department == "GREK" ~ "Greek", Department == "JAPN" ~ "Japanese",
      Department == "LATN" ~ "Latin", Department == "MUSI" ~ "Music", Department == "PHIL" ~ "Philosophy",
      Department == "RELI" ~ "Religion", Department == "RHET" ~ "Rhetoric", Department == "RUSS" ~ "Russian",
      Department == "SPAN" ~ "Spanish", Department == "THEA" ~ "Theater", Department == "ASTR" ~ "Astronomy",
      Department == "BIOL" ~ "Biology", Department == "CHEM" ~ "Chemistry", Department == "COSC" ~ "Computer Science",
      Department == "EESC" ~ "Earth & Environmental Science", Department == "MATH" ~ "Mathematics",
      Department == "NEUR" ~ "Neuroscience", Department == "PHYS" ~ "Physics", Department == "ANTH" ~ "Anthropology",
      Department == "ECON" ~ "Economics", Department == "EDUC" ~ "Education", Department == "HIST" ~ "History",
      Department == "POLS" ~ "Political Science", Department == "PSYC" ~ "Psychology",
      Department == "SOCI" ~ "Sociology", Department == "AFAM" ~ "African American Studies",
      Department == "EUST" ~ "European Studies", Department == "FYS" ~ "First Year Seminar",
      Department == "INDS" ~ "Interdisciplinary Studies", Department == "WGSS" ~ "Women's, Gender & Sexuality Studies",
      Department == "ENVR" ~ "Environmental Studies", TRUE ~ Department
    )
  )

academic_activity_final <- academic_activity_final %>%
  left_join(location_coords, by = "Location")

dining_activity_final <- dining_activity %>%
  left_join(location_coords, by = "Location")

message("Department names recoded and XY coordinates joined.")


# --- 6: COMBINE AND SAVE ---
message("\n--- Starting Part 6: Combining and Saving Final Dataset ---")

dashboard_data <- bind_rows(dining_activity_final, academic_activity_final) %>%
  arrange(Timestamp_15min, Location) %>%
  select(Timestamp_15min, Location, loc_x, loc_y, ActivityType, Department, Occupancy)

output_path <- here::here("data", "dashboard_data.rds")
saveRDS(dashboard_data, output_path)

message("Unified dataset created successfully.")
message(paste("Total rows in final dataset:", nrow(dashboard_data)))
message(paste("File saved to:", output_path))
message("\n--- Script complete. ---")