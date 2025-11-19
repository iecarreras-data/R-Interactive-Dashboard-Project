################################################################################
# Program: 01_create_catalog.R
# Purpose: Read a raw text file of the university course catalog, parse it using
#          regex, and perform extensive cleaning and validation to produce a
#          final, tidy master data frame of all possible courses.
# Inputs: ./data-raw/catalog_text.txt
# Outputs: ./data/course_catalog.rds
# Created: 24OCT2025 (IEC)
# Modified: 18NOV2025 (IEC)
################################################################################

# --- 1. Load Libraries ---
# All required packages should be listed at the top.
library(tidyverse)
library(here) # For building file paths relative to the project root

# --- 2. The "Campus Blueprint" ---
# This section contains all of our expert knowledge about the campus.

# --- 2a. Department Codes ---
department_codes <- c(
  "AFR", "AFEN", "AFHI", "AFES", "AFSO", "AFGS", "AMST", "AMAN", "AMEN", "AMHI",
  "AMPT", "AMAV", "AMGS", "ANTH", "ANLS", "ANLL", "ANGS", "AVC", "AVEN", "AVCM",
  "AVAS", "AMAV", "AVGS", "AVMU", "ASIA", "ASJA", "ASCI", "AVAS", "ASRE", "ASHI",
  "ASPT", "ASPY", "BIO", "BIEA", "BIES", "BIPL", "BIMA", "BIOC", "BICH", "CHEM",
  "CHI", "CMS", "CMHI", "CMEN", "CMGS", "CMDN", "CMRE", "DCS", "DCNS", "DCMA",
  "DCMU", "DCGE", "EACS", "EAES", "EAPH", "ECON", "ECMA", "DCEC", "EDUC", "EDES",
  "EDGS", "ENG", "ENTH", "ENGS", "ENVR", "ENES", "ESER", "EUS", "EURU", "EUHI",
  "EUSO", "EUPT", "EXDS", "FYS", "FRE", "GSS", "GSTH", "GSPT", "GSSP", "GSSO",
  "GSHS", "GSPL", "GSPY", "GER", "RUSS", "GRK", "LATN", "HISP", "HSLL", "HSLS",
  "HIST", "HILS", "HILL", "INDS", "JPN", "LALS", "LLPT", "LSSP", "MATH", "MAPH",
  "MUS", "DNMU", "NRSC", "NSPY", "PHIL", "PLRE", "PHYS", "ASTR", "PLTC", "PTSO",
  "PSYC", "PYRL", "REL", "RFSS", "SOC", "THEA", "DANC"
)

# --- 3. Parsing and Initial Cleaning ---
cat("STEP 3: Parsing raw catalog text file...\n")

# Use here::here() to build a path from the project root.
# This makes the code portable and work on any machine.
# Assumes "catalog_text.txt" is in a "data-raw" folder.
text_file_path <- here::here("data-raw", "catalog_text.txt")

# Read the entire file into one long string
full_text_blob <- paste(readLines(text_file_path, warn = FALSE), collapse = " ") %>%
  str_replace_all("\\s+", " ") # Consolidate all whitespace

# Find the true start of the course listings
start_location <- str_locate(full_text_blob, "Course Offerings\\\\")[1, "end"]
courses_text_blob <- str_sub(full_text_blob, start_location)

# Define the master regex to capture each course block
department_pattern_part <- paste(department_codes, collapse = "|")
master_pattern <- sprintf(
  "((?:%s)\\s\\d{3}[A-Z]?)\\s(.*?)\\\\(.*?)Instructor Permission Required: (?:Yes|No)\\\\",
  department_pattern_part
)

# Extract all matching blocks
all_matches <- str_match_all(courses_text_blob, master_pattern)[[1]]

# Convert the matrix to a clean data frame
parsed_catalog_df <- as_tibble(all_matches[, 2:3], .name_repair = "minimal")
colnames(parsed_catalog_df) <- c("CourseCode", "CourseTitle")

# Final cleanup
parsed_catalog_df <- parsed_catalog_df %>%
  mutate(
    CourseCode = str_trim(CourseCode),
    CourseTitle = str_trim(CourseTitle),
    Department = str_extract(CourseCode, "^[A-Z/]+"),
    CourseLevel = as.numeric(str_extract(CourseCode, "\\d{3}"))
  ) %>%
  filter(!is.na(Department)) %>%
  distinct(CourseCode, .keep_all = TRUE) # Ensure every course is unique

cat(" -> Successfully parsed", nrow(parsed_catalog_df), "unique courses from the catalog.\n")


# --- 4. Manual Data Correction ---
# This block applies manual fixes for known parsing errors or data issues.
cat("STEP 4: Applying manual data corrections...\n")

# --- 4a. Fix incorrect course titles from parsing errors ---
corrected_titles_df <- parsed_catalog_df %>%
  mutate(
    CourseTitle = case_when(
      CourseCode == "PHYS 107" ~ "Introductory Physics of Living Systems I/Lab",
      CourseCode == "PHYS 108" ~ "Introductory Physics of Living Systems II/Lab",
      # ... (add all other title recodes here) ...
      CourseCode == "GSS 363" ~ "Women and the Women’s Movement in Africa",
      TRUE ~ CourseTitle # This is crucial: keeps the original title if no match
    )
  )
cat(" -> Fixed course titles based on manual recode list.\n")

# --- 4b. Fix incorrect department codes ---
engs_codes_to_fix <- c("ENGS 121G", "ENGS 395D", "ENGS 395Q")
corrected_departments_df <- corrected_titles_df %>%
  mutate(
    Department = if_else(CourseCode %in% engs_codes_to_fix, "ENG", Department),
    CourseCode = if_else(CourseCode %in% engs_codes_to_fix, str_replace(CourseCode, "ENGS", "ENG"), CourseCode)
  )
cat(" -> Corrected", length(engs_codes_to_fix), "ENGS courses to ENG department.\n")


# --- 5. Advanced Data Deduplication & Cleaning ---
# This section resolves cross-listed courses to ensure a single, unique entry
# for each distinct course offering.
cat("STEP 5: Performing advanced deduplication for cross-listed courses...\n")

# --- 5a. Explicit Deletion of erroneous or unwanted courses ---
codes_to_delete <- c("FYS 445", "GSS 400", "CHEM 107", "BIES 336", "BIO 195", "PLTC 263", "AVC 212A")
cleaned_df <- corrected_departments_df %>%
  filter(!CourseCode %in% codes_to_delete, CourseLevel != 458)

# --- 5b. Isolate courses that do not need deduplication ---
independent_work_df <- cleaned_df %>% filter(CourseLevel %in% c(360, 457))
deduplication_candidates_df <- cleaned_df %>% filter(!CourseLevel %in% c(360, 457))

# --- 5c. Identify courses with duplicate titles (candidates for deduplication) ---
courses_with_counts_df <- deduplication_candidates_df %>% add_count(CourseTitle, name = "title_count")
unique_courses_df <- courses_with_counts_df %>% filter(title_count == 1)
courses_to_deduplicate_df <- courses_with_counts_df %>% filter(title_count > 1)
cat(" -> Found", nrow(unique_courses_df), "courses with unique titles.\n")
cat(" -> Found", nrow(courses_to_deduplicate_df), "cross-listed instances to resolve.\n")

# --- 5d. Apply 'keeper' list strategy to resolve cross-listings ---
# This vector defines which course code to keep when a title is duplicated.
keeper_codes_vector <- c(
  "AFR 227", "HIST 264", "HISP 390", "REL 258", "HIST 112", "GRK 301", "PHIL 271", "AVC 271",
  # ... (add all other keeper codes here) ...
  "PLTC 363", "PSYC 363", "SOC 340"
)

# Filter the duplicate pool to keep only the designated primary course code.
deduplicated_courses_df <- courses_to_deduplicate_df %>%
  filter(CourseCode %in% keeper_codes_vector)
cat(" -> Resolved cross-listings down to", nrow(deduplicated_courses_df), "unique courses.\n")

# --- 5e. Recombine into a final, fully cleaned catalog ---
fully_cleaned_catalog_df <- bind_rows(
  unique_courses_df,
  deduplicated_courses_df,
  independent_work_df
) %>%
  select(-title_count) # Remove the temporary count column

# --- 6. Add Manually Missed Courses ---
cat("STEP 6: Manually adding known missing courses to the catalog...\n")

missing_courses <- tribble(
  ~CourseCode, ~CourseTitle, ~Department, ~CourseLevel,
  "ECON 101", "Principles of Microeconomics: Prices and Markets", "ECON", 101,
  "NRSC 119", "Drugs: The Damage Done and Designing Better Ones", "NRSC", 119,
  "SOC 101", "Principles of Sociology", "SOC", 101,
  "FRE 101", "Elementary French I", "FRE", 101,
  "GER 101", "Introduction to German Language and Culture I", "GER", 101
)

final_catalog_df <- bind_rows(fully_cleaned_catalog_df, missing_courses) %>%
  distinct(CourseCode, .keep_all = TRUE)

cat(" -> Final master catalog contains", nrow(final_catalog_df), "total unique courses.\n")

# --- 7. Save Final Output ---
# Save the final, clean data frame as an .rds file.
# This format is ideal for passing R objects between scripts as it preserves
# data types (e.g., numeric, character) perfectly.
cat("STEP 7: Saving the final master catalog to the 'data' folder...\n")
saveRDS(
  final_catalog_df,
  file = here::here("data", "course_catalog.rds")
)

cat("\nSUCCESS: Master course catalog created at 'data/course_catalog.rds'.\n")