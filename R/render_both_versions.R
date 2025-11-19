################################################################################
# Program: render_both_versions.R
# Purpose: Renders the dashboard in two formats:
#          1. Self-contained HTML (for easy distribution/download)
#          2. HTML with libs folder (optimized for web hosting)
# Created: 19NOV2025 (IEC)
################################################################################

library(rmarkdown)
library(here)

message("=== Rendering Dashboard - Two Versions ===\n")

# --- Version 1: Self-Contained (for repository users) ---
message("Rendering self-contained version...")
rmarkdown::render(
  here::here("R", "06_build_dashboard.Rmd"), 
  output_file = here::here("output", "student-activity-dashboard.html"),
  output_options = list(self_contained = TRUE)
)
message("✓ Self-contained version saved: output/student-activity-dashboard.html\n")

# --- Version 2: With Libs Folder (for GitHub Pages) ---
message("Rendering web-optimized version with libs folder...")
rmarkdown::render(
  here::here("R", "06_build_dashboard.Rmd"), 
  output_file = here::here("output", "student-activity-dashboard-web.html"),
  output_options = list(
    self_contained = FALSE, 
    lib_dir = here::here("output", "libs")
  )
)
message("✓ Web version saved: output/student-activity-dashboard-web.html")
message("✓ Dependencies saved: output/libs/\n")

message("=== Both versions rendered successfully! ===")
message("\nFor repository: Use student-activity-dashboard.html")
message("For GitHub Pages: Upload student-activity-dashboard-web.html + libs/ folder")