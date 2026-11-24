# 07_sample_checks.R
# Group A, PEA project 4. Documents the actual estimation sample, the missing
# values of the key variables and the adjunct-faculty coding problem in 2023.
#
# Run through run_all_g4.R (working directory = PEA_GroupA). It reads the files
# produced by the instructor kit in output/template_run/ and the raw workbook
# in template/ (read only). Tables are written to output/tables/.

run_dir <- file.path("output", "template_run")
tab_dir <- file.path("output", "tables")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

input_files <- file.path(run_dir, c("teaching_panel_2020_2023.csv",
                                    "project_panel_2020_2023.csv",
                                    "results_group_04_sample.csv"))
if (any(!file.exists(input_files))) {
  stop("Missing kit outputs: ", paste(input_files[!file.exists(input_files)], collapse = ", "),
       "\nRun run_all_g4.R from the PEA_GroupA folder first.")
}
panel <- read.csv(input_files[1], check.names = FALSE)
project <- read.csv(input_files[2], check.names = FALSE)
sample_06 <- read.csv(input_files[3], check.names = FALSE)

# Specification of 06 for group 4.
outcome <- "log_publications"
exposure <- "lag_log_teaching_load"
controls <- c("lag_log_faculty", "lag_lcurrent")
fields <- c("medicine_share18", "engphys_share18", "ssh_share18")

# TRUE where every listed variable is observed and finite.
all_finite <- function(d, vars) {
  keep <- rep(TRUE, nrow(d))
  for (v in vars) keep <- keep & is.finite(d[[v]])
  keep
}


# 1. SAMPLE FLOW: WHICH UNIVERSITIES AND YEARS SURVIVE EACH RESTRICTION -------

steps <- list(
  "1. Teaching panel 2020-2023" = rep(TRUE, nrow(project)),
  "2. Lagged teaching load observed (no 2019 data)" = all_finite(project, exposure),
  "3. Outcome and lagged controls observed" =
    all_finite(project, c(exposure, outcome, controls)),
  "4. 2018 field shares observed (estimation sample of 06)" =
    all_finite(project, c(exposure, outcome, controls, fields))
)

flow_rows <- list()
previous_ids <- unique(project$id)
previous_n <- nrow(project)
for (step in names(steps)) {
  keep <- steps[[step]]
  ids <- unique(project$id[keep])
  years <- sort(unique(project$year[keep]))
  dropped <- setdiff(previous_ids, ids)
  flow_rows[[step]] <- data.frame(
    step = step,
    rows = sum(keep),
    rows_dropped = previous_n - sum(keep),
    universities = length(ids),
    years = paste(min(years), max(years), sep = "-"),
    universities_dropped = if (length(dropped)) paste(dropped, collapse = "; ") else ""
  )
  previous_ids <- ids
  previous_n <- sum(keep)
}
flow <- do.call(rbind, flow_rows)
rownames(flow) <- NULL
print(flow)

# The last step must reproduce the sample written by 06 exactly.
final <- steps[[length(steps)]]
stopifnot(
  nrow(sample_06) == 180L,
  length(unique(sample_06$id)) == 60L,
  setequal(paste(project$id[final], project$year[final]),
           paste(sample_06$id, sample_06$year))
)
write.csv(flow, file.path(tab_dir, "table_sample_flow.csv"), row.names = FALSE)


# 2. CHECK THAT THE EXPOSURE IS log(students / faculty) OF THE PREVIOUS YEAR --

previous_row <- match(paste(sample_06$id, sample_06$year - 1L),
                      paste(panel$id, panel$year))
stopifnot(!anyNA(previous_row))
ratio_previous <- panel$students[previous_row] / panel$faculty[previous_row]
stopifnot(isTRUE(all.equal(sample_06$lag_log_teaching_load, log(ratio_previous))))
cat("Exposure check passed: lag_log_teaching_load = log(students/faculty) in t-1.\n")


# 3. MISSING VALUES AND ZEROS OF THE KEY VARIABLES, BY YEAR -------------------

# Blank and N.A. cells are kept as missing; they are never recoded as zero.
key_vars <- c("faculty", "fellows", "students", "graduates",
              "current_management", "publications", fields)
missing_rows <- list()
for (v in key_vars) {
  for (yr in sort(unique(panel$year))) {
    x <- panel[[v]][panel$year == yr]
    missing_rows[[paste(v, yr)]] <- data.frame(
      variable = v, year = yr, universities = length(x),
      missing = sum(is.na(x)), zeros = sum(x == 0, na.rm = TRUE)
    )
  }
}
missing_table <- do.call(rbind, missing_rows)
rownames(missing_table) <- NULL
print(missing_table[missing_table$missing > 0 | missing_table$zeros > 0, ])
write.csv(missing_table, file.path(tab_dir, "table_missing_values.csv"), row.names = FALSE)


# 4. ADJUNCT FACULTY: 2023 IS A MISSING YEAR, NOT A YEAR OF ZEROS -------------

workbook <- as.data.frame(readxl::read_excel(file.path("template", "Dataset_V3.xlsx"),
                                             sheet = "Dataset 20-23", skip = 1))
names(workbook) <- trimws(names(workbook))
workbook <- workbook[!is.na(workbook$UNIVERSITY_ID), ]

adjunct_rows <- list()
for (yr in sort(unique(workbook$YEAR))) {
  x <- workbook[["ADJUNCT FACULTY"]][workbook$YEAR == yr]
  adjunct_rows[[as.character(yr)]] <- data.frame(
    year = yr, universities = length(x), missing = sum(is.na(x)),
    zeros = sum(x == 0, na.rm = TRUE),
    median_positive = median(x[!is.na(x) & x > 0]),
    treatment = if (yr == 2023) "treated as missing (all values are 0)" else "used as recorded"
  )
}
adjunct <- do.call(rbind, adjunct_rows)
rownames(adjunct) <- NULL
print(adjunct)

# Stop if the workbook no longer shows the documented pattern.
stopifnot(
  all(adjunct$zeros[adjunct$year == 2023] == adjunct$universities[adjunct$year == 2023]),
  all(adjunct$zeros[adjunct$year < 2023] == 0L)
)
write.csv(adjunct, file.path(tab_dir, "table_adjunct_check.csv"), row.names = FALSE)


# 5. DESCRIPTIVE TABLE OF THE ESTIMATION SAMPLE --------------------------------

# 60 universities. The outcome is observed in t = 2021-2023; the exposure and
# the controls are measured in t-1 = 2020-2022. Costs are nominal euros here.
sample_ids <- unique(sample_06$id)
panel$students_per_faculty <- panel$students / panel$faculty
panel$current_management_m <- panel$current_management / 1e6

describe <- list(
  c("Scopus publications (t)", "publications", "2021-2023"),
  c("Enrolled students (t-1)", "students", "2020-2022"),
  c("Faculty, internal teaching staff (t-1)", "faculty", "2020-2022"),
  c("Students per faculty member (t-1)", "students_per_faculty", "2020-2022"),
  c("Research fellows (t-1)", "fellows", "2020-2022"),
  c("Current-management costs, million EUR nominal (t-1)", "current_management_m", "2020-2022")
)
descriptive_rows <- list()
for (item in describe) {
  yrs <- if (item[3] == "2021-2023") 2021:2023 else 2020:2022
  x <- panel[[item[2]]][panel$id %in% sample_ids & panel$year %in% yrs]
  q <- quantile(x, c(0, 0.25, 0.5, 0.75, 1))
  descriptive_rows[[item[2]]] <- data.frame(
    variable = item[1], years = item[3], n = length(x),
    mean = mean(x), sd = sd(x), min = q[[1]], p25 = q[[2]],
    median = q[[3]], p75 = q[[4]], max = q[[5]]
  )
}
descriptives <- do.call(rbind, descriptive_rows)
rownames(descriptives) <- NULL
numeric_cols <- c("mean", "sd", "min", "p25", "median", "p75", "max")
descriptives[numeric_cols] <- lapply(descriptives[numeric_cols], round, digits = 2)
print(descriptives)
write.csv(descriptives, file.path(tab_dir, "table_sample_descriptives.csv"), row.names = FALSE)

cat("07 completed: tables written to output/tables/.\n")
