# Student Activity Dashboard

## Project Overview

This dashboard is a demonstration modeled on a real-world data product I developed during my tenure at Harvard University. The original project solved the critical challenge of synthesizing disparate data streams, such as building access swipes and event check-ins, to provide leadership with actionable intelligence on student engagement patterns.

The end product is a powerful tool that transforms raw, disconnected data into an intuitive platform for strategic decision-making, showcasing my ability to deliver end-to-end data solutions.

![Dashboard Preview](https://raw.githubusercontent.com/iecarreras-data/Student-Activity-Dashboard-Project/main/dashboard_preview.png)

### Core Competencies Showcased

*   **Conceptualize and Build a Data Product:** Taking a strategic goal from an initial idea to a functional, interactive tool.
*   **Data Simulation:** Creating realistic, high-fidelity synthetic datasets that mimic real-world complexity while ensuring complete privacy and ethical data handling.
*   **Data Engineering:** Building a reproducible, multi-step data processing pipeline with proper timezone handling and data integrity checks.
*   **Develop Dynamic Visualizations:** Building user-friendly interfaces in R that empower non-technical stakeholders to explore complex data independently.

---

### Tech Stack & Repository Structure

*   **Language:** R
*   **Key R Packages:** tidyverse, here, lubridate, hms, plotly, htmltools, rmarkdown

The repository is structured as a self-contained RStudio Project to ensure full reproducibility.

.
├── R/                                   
│   ├── 01_create_catalog.R             
│   ├── 02_build_semester_schedule.R    
│   ├── 03_register_students.R          
│   ├── 04_simulate_dining.R            
│   ├── 05_create_dashboard_data.R      
│   └── 06_build_dashboard.Rmd          
├── data/                                
├── data-raw/                            
├── output/                              
├── .gitignore                           
├── Student-Activity-Dashboard-Project.Rproj 
└── README.md                            

---

### How to Reproduce This Project

Follow these steps to run the entire data pipeline and generate the final dashboard.

#### 1. Setup & Prerequisites

1.  **Clone the Repository:** Clone this repository to your local machine using git clone.

2.  **Open the Project:** Open RStudio by double-clicking the Student-Activity-Dashboard-Project.Rproj file. This is crucial as it sets the correct working directory.

3.  **Install Packages:** Open the R console in RStudio and install all necessary packages.

#### 2. Run the Data Generation Pipeline

The scripts in the R folder must be run in numerical order. Each script's output serves as the next script's input.

##### Running the Scripts

Execute each script in order:

source(here::here("R", "01_create_catalog.R"))
source(here::here("R", "02_build_semester_schedule.R"))
source(here::here("R", "03_register_students.R"))
source(here::here("R", "04_simulate_dining.R"))
source(here::here("R", "05_create_dashboard_data.R"))

Then render the final dashboard:

rmarkdown::render(
  here::here("R", "06_build_dashboard.Rmd"), 
  output_file = here::here("output", "student-activity-dashboard.html")
)

Alternatively, use the helper script to generate both versions at once:

source(here::here("R", "render_both_versions.R"))

##### Pipeline Details

1.  **01_create_catalog.R**
    *   **Purpose:** Parses the raw course catalog text file and creates a clean, structured master list of all courses
    *   **Input:** data-raw/catalog_text.txt
    *   **Output:** data/course_catalog.rds

2.  **02_build_semester_schedule.R**
    *   **Purpose:** Curates which courses to offer this semester, creates multiple sections for high-demand courses, and assigns classrooms and time slots using a capacity-aware scheduling algorithm
    *   **Input:** data/course_catalog.rds
    *   **Output:** data/master_schedule.rds

3.  **03_register_students.R**
    *   **Purpose:** Simulates a student body with diverse academic personas and generates realistic course enrollments with conflict-checking logic
    *   **Input:** data/master_schedule.rds
    *   **Output:** data/student_roster.rds, data/student_enrollment.rds

4.  **04_simulate_dining.R**
    *   **Purpose:** Simulates student dining hall usage patterns throughout the week based on their academic schedules, accounting for meal time windows and capacity constraints
    *   **Input:** data/student_roster.rds, data/student_enrollment.rds, data/master_schedule.rds
    *   **Output:** data/dining_swipes.rds

5.  **05_create_dashboard_data.R**
    *   **Purpose:** Aggregates all academic and dining data into a single, unified time-series dataset with 15-minute interval resolution, properly handling timezones and building names
    *   **Inputs:** data/student_enrollment.rds, data/master_schedule.rds, data/dining_swipes.rds
    *   **Output:** data/dashboard_data.rds

6.  **06_build_dashboard.Rmd**
    *   **Purpose:** Renders an interactive HTML dashboard with animated visualizations showing campus activity patterns over time
    *   **Input:** data/dashboard_data.rds
    *   **Output:** output/student-activity-dashboard.html

After running all scripts, open output/student-activity-dashboard.html in your web browser to view the final, interactive dashboard.

---

### Key Technical Solutions

This project demonstrates advanced data engineering practices:

- **Timezone Integrity:** Proper handling of America/New_York timezone throughout the pipeline to ensure accurate temporal analysis
- **Conflict-Free Scheduling:** Multi-pass algorithm that respects room capacities, building locations, and time slot availability
- **Realistic Data Simulation:** Probabilistic models that create believable patterns in course selection and dining behavior
- **Scalable Architecture:** Modular pipeline design where each script has a single, well-defined responsibility

---

### Contact & Attribution

**Author:** Ismael E. Carreras  
**GitHub:** @iecarreras-data  
**LinkedIn:** Ismael E. Carreras

This project is based on real-world experience but uses entirely synthetic data for demonstration purposes.
