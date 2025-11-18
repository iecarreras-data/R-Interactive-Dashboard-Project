# Student Activity Dashboard

## Project Overview

This dashboard is a demonstration modeled on a real-world data product I developed during my tenure at Harvard University. The original project solved the critical challenge of synthesizing disparate data streams, such as building access swipes and event check-ins, to provide leadership with actionable intelligence on student engagement patterns.

The end product is a powerful tool that transforms raw, disconnected data into an intuitive platform for strategic decision-making, showcasing my ability to deliver end-to-end data solutions.

![Dashboard Preview](https://raw.githubusercontent.com/iecarreras-data/Student-Activity-Dashboard-Project/main/dashboard_preview.png)
*(Note: I've added a sample image link. To make this work, you would add a screenshot named `dashboard_preview.png` to the root of your repository.)*

### Core Competencies Showcased

*   **Conceptualize and Build a Data Product:** Taking a strategic goal from an initial idea to a functional, interactive tool.
*   **Data Simulation:** Creating realistic, high-fidelity synthetic datasets that mimic real-world complexity while ensuring complete privacy and ethical data handling.
*   **Data Engineering:** Building a reproducible, multi-step data processing pipeline.
*   **Develop Dynamic Visualizations:** Building user-friendly interfaces in R (`flexdashboard`, `plotly`, `crosstalk`) that empower non-technical stakeholders to explore complex data independently.

---

### Tech Stack & Repository Structure

*   **Language:** R
*   **Key R Packages:** `tidyverse`, `here`, `lubridate`, `flexdashboard`, `plotly`

The repository is structured as a self-contained RStudio Project to ensure full reproducibility.

```
.
├── R/                                   # Contains all R source scripts for the pipeline
├── data/                                # Stores generated data files (e.g., .rds) - git-ignored
├── data-raw/                            # Contains the raw, original input files
├── output/                              # Stores the final HTML dashboard - git-ignored
├── .gitignore                           # Specifies which files and folders for Git to ignore
├── Student-Activity-Dashboard-Project.Rproj # The RStudio Project file
└── README.md                            # You are here!
```

---

### How to Reproduce This Project

Follow these steps to run the entire data pipeline and generate the final dashboard.

#### 1. Setup & Prerequisites

1.  **Clone the Repository:** Clone this repository to your local machine.
    ```bash
    git clone https://github.com/iecarreras-data/Student-Activity-Dashboard-Project.git
    ```
2.  **Open the Project:** Open RStudio by double-clicking the `Student-Activity-Dashboard-Project.Rproj` file. This is crucial as it sets the correct working directory.
3.  **Install Packages:** Open the R console in RStudio and run the following command to install all necessary packages.
    ```r
    install.packages(c("tidyverse", "here", "lubridate", "flexdashboard", "plotly"))
    ```

#### 2. Run the Data Generation Pipeline

The scripts in the `R/` folder are designed to be run in numerical order. Each script's output is the next script's input.

1.  **`R/01_create_catalog.R`**
    *   **Purpose:** Parses the raw `catalog_text.txt` file and creates a clean, master list of all possible courses.
    *   **Input:** `data-raw/catalog_text.txt`
    *   **Output:** `data/course_catalog.rds`

2.  **`R/02_register_students.R`**
    *   **Purpose:** Simulates the student body and a course registration period, assigning students to courses from the catalog.
    *   **Input:** `data/course_catalog.rds`
    *   **Output:** `data/student_registrations.rds`

3.  **`R/03_simulate_dining.R`**
    *   **Purpose:** Simulates student dining hall usage patterns throughout the week based on their academic schedules.
    *   **Input:** `data/student_registrations.rds`
    *   **Output:** `data/dining_activity.rds`

4.  **`R/04_create_dashboard_data.R`**
    *   **Purpose:** Aggregates all academic and dining data into a single, analysis-ready dataset, timed to 15-minute intervals.
    *   **Inputs:** `data/student_registrations.rds`, `data/dining_activity.rds`
    *   **Output:** `data/dashboard_data.rds`

5.  **`R/05_build_dashboard.R`**
    *   **Purpose:** Reads the final aggregated data and uses `flexdashboard` to render the interactive HTML dashboard.
    *   **Input:** `data/dashboard_data.rds`
    *   **Output:** `output/student_activity_dashboard.html`

After running all five scripts, you can open `output/student_activity_dashboard.html` in your web browser to view the final, interactive dashboard.
