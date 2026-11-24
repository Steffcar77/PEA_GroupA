# Run once before class.
# Open the workshop's RStudio project before running this script.

# Check the working folder before installing anything.
required_files <- c(
  "Dataset_V3.xlsx",
  "teaching_panel_2020_2023.csv"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files)) {
  stop(
    "Files missing from the working folder: ",
    paste(missing_files, collapse = ", "),
    "\nOpen the workshop's RStudio project and check that ",
    "the scripts, workbook and CSV files are together."
  )
}

# Install missing packages; retain packages already installed.
needed <- c("sfa", "readxl", "sandwich", "lmtest")

missing_packages <- needed[
  !vapply(needed, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

library(sfa)

cat("Working folder:", getwd(), "\n")
cat("sfa version:", as.character(packageVersion("sfa")), "\n")
cat("Setup complete. Open 00_first_steps.R next.\n")