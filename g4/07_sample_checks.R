# 07_sample_checks.R
# Group A, PEA project 4. Documents the actual estimation sample, the missing
# values of the key variables, the adjunct-faculty coding problem in 2023 and
# data notes to verify at the source.
#
# Group copy of the project panel: the instructor's project_panel_2020_2023.csv
# with ADJUNCT FACULTY 2023 set to NA (all 61 values are 0 in the workbook, while
# no value is 0 in 2020-2022). Only those 61 cells change; the sample flow below
# uses this copy. No estimate uses adjunct faculty, so no estimate changes.
#
# Run through run_all_g4.R (working directory = PEA_GroupA). It reads the files
# produced by the instructor kit in output/template_run/ and the raw workbook
# in template/ (read only). Outputs:
#   output/g4_project_panel_2020_2023.csv (group copy of the project panel);
#   output/tables/table_sample_flow.csv, table_missing_values.csv,
#   table_adjunct_check.csv, table_sample_descriptives.csv, table_07_data_notes.csv.

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
project_kit <- read.csv(input_files[2], check.names = FALSE)
sample_06 <- read.csv(input_files[3], check.names = FALSE)


# 0. GROUP COPY OF THE PROJECT PANEL: ADJUNCT FACULTY 2023 -> NA ---------------

project <- project_kit
is_2023 <- project$year == 2023
stopifnot("adjunct" %in% names(project), sum(is_2023) == 61L,
          all(project$adjunct[is_2023] == 0), all(project$adjunct[!is_2023] > 0))
project$adjunct[is_2023] <- NA_real_

# Check: exactly the 61 adjunct cells of 2023 differ; everything else is identical.
changed <- matrix(FALSE, nrow(project), ncol(project), dimnames = list(NULL, names(project)))
for (v in names(project)) {
  a <- project_kit[[v]]
  b <- project[[v]]
  changed[, v] <- (is.na(a) != is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
}
stopifnot(sum(changed) == 61L, all(changed[is_2023, "adjunct"]),
          sum(changed[, "adjunct"]) == 61L)
write.csv(project, file.path("output", "g4_project_panel_2020_2023.csv"), row.names = FALSE)
cat("Group project panel written: adjunct 2023 set to NA (61 cells changed, nothing else).\n")

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
    treatment = if (yr == 2023) {
      "all values are 0: set to NA in output/g4_project_panel_2020_2023.csv"
    } else "used as recorded"
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


# 6. DATA NOTES TO VERIFY AT THE SOURCE (numbers computed from the data) ---------

pct <- function(x) sprintf("%+.1f%%", 100 * x)
eur_k <- function(x) format(round(x / 1000), big.mark = ",", trim = TRUE)

# Year-on-year changes within each university, 2020-2023.
p <- panel[order(panel$id, panel$year), ]
prev <- match(paste(p$id, p$year - 1L), paste(p$id, p$year))
p$faculty_change <- p$faculty / p$faculty[prev] - 1
p$cm_change <- p$current_management / p$current_management[prev] - 1

# (a) Adjunct faculty 2023.
adj_note <- sprintf(paste(
  "ADJUNCT FACULTY is 0 for %d of %d universities in 2023 and never 0 in 2020-2022 (%d zeros).",
  "Treated as missing: set to NA in output/g4_project_panel_2020_2023.csv. It enters no estimate:",
  "no model uses adjunct faculty and the teaching load uses 2020-2022 only. Verify the 2023 values at the source."),
  adjunct$zeros[adjunct$year == 2023], adjunct$universities[adjunct$year == 2023],
  sum(adjunct$zeros[adjunct$year < 2023]))

# (b) Perugia Stranieri: faculty jump 2021 -> 2022.
ps <- p[p$id == "Perugia Stranieri", ]
stopifnot(nrow(ps) == 4L)
fc <- p[is.finite(p$faculty_change), ]
fc <- fc[order(-abs(fc$faculty_change)), ]
ps_rank <- which(fc$id == "Perugia Stranieri" & fc$year == 2022)
ps_note <- sprintf(paste(
  "Perugia Stranieri: faculty %d -> %d between 2021 and 2022 (%s), students %d -> %d.",
  "Rank %d of %d year-on-year faculty changes in the panel by absolute size (next: %s %d, %s).",
  "It produces the minimum student-faculty ratio (%.2f in 2022). Plausible for a very small university,",
  "but a large within-university change in the exposure: verify the 2022 faculty count at the source."),
  ps$faculty[ps$year == 2021], ps$faculty[ps$year == 2022],
  pct(ps$faculty_change[ps$year == 2022]),
  ps$students[ps$year == 2021], ps$students[ps$year == 2022],
  ps_rank, nrow(fc), fc$id[2], fc$year[2], pct(fc$faculty_change[2]),
  ps$students[ps$year == 2022] / ps$faculty[ps$year == 2022])

# (c) Molise: low current-management costs; cost components; 2022 jump.
cost_items <- c("OPERATING COSTS", "OPERATING COSTS: PERSONNEL",
                "OPERATING COSTS: CURRENT MANAGEMENT",
                "OPERATING COSTS: DEPRECIATION AND WRITE-DOWNS", "OPERATING COSTS: OTHER")
stopifnot(all(cost_items %in% names(workbook)))
wb <- workbook
for (v in cost_items) wb[[v]] <- suppressWarnings(as.numeric(wb[[v]]))
component_gap <- max(abs(wb[[cost_items[1]]] - rowSums(wb[cost_items[-1]])) / wb[[cost_items[1]]],
                     na.rm = TRUE)
wb$cm_share <- wb[[cost_items[3]]] / wb[[cost_items[1]]]
w20 <- wb[wb$YEAR == 2020, ]
w20 <- w20[order(w20$cm_share), ]
mo20 <- w20[w20$UNIVERSITY_ID == "Molise", ]
lag_years <- p[p$year %in% 2020:2022, ]
lag_years$cm_per_faculty <- lag_years$current_management / lag_years$faculty
lag_years <- lag_years[order(lag_years$cm_per_faculty), ]
cm <- p[is.finite(p$cm_change), ]
cm <- cm[order(-cm$cm_change), ]
mo_rank <- which(cm$id == "Molise" & cm$year == 2022)
mo <- p[p$id == "Molise", ]
mo_note <- sprintf(paste(
  "Molise: lowest share of current-management costs in operating costs in 2020 (%.1f%%; next %s %.1f%%;",
  "%.1f / %.1f million EUR) and lowest current-management cost per faculty member in 2020-2022",
  "(%s, %d: %s thousand EUR; median %s thousand). The cost components (personnel, current management,",
  "depreciation, other) add up to operating costs for every university-year (max relative gap %.1e),",
  "so this does not look like a coding error. Current-management costs jump %s in 2022",
  "(%.1f -> %.1f million EUR), rank %d of %d year-on-year changes ordered from the largest increase (larger: %s). Verify the 2022 jump at the source."),
  100 * mo20$cm_share, w20$UNIVERSITY_ID[2], 100 * w20$cm_share[2],
  mo20[[cost_items[3]]] / 1e6, mo20[[cost_items[1]]] / 1e6,
  lag_years$id[1], lag_years$year[1], eur_k(lag_years$cm_per_faculty[1]),
  eur_k(median(lag_years$cm_per_faculty)), component_gap,
  pct(mo$cm_change[mo$year == 2022]),
  mo$current_management[mo$year == 2021] / 1e6, mo$current_management[mo$year == 2022] / 1e6,
  mo_rank, nrow(cm),
  paste(sprintf("%s %d %s", cm$id[seq_len(mo_rank - 1)], cm$year[seq_len(mo_rank - 1)],
                pct(cm$cm_change[seq_len(mo_rank - 1)])), collapse = "; "))
stopifnot(w20$UNIVERSITY_ID[1] == "Molise", lag_years$id[1] == "Molise")

data_notes <- data.frame(
  item = c("Adjunct faculty 2023", "Perugia Stranieri faculty 2022", "Molise current-management costs"),
  status = c("treated as missing; no estimate affected", "to verify at the source",
             "components consistent; 2022 jump to verify at the source"),
  note = c(adj_note, ps_note, mo_note)
)
print(data_notes)
write.csv(data_notes, file.path(tab_dir, "table_07_data_notes.csv"), row.names = FALSE)

cat("07 completed: tables written to output/tables/.\n")
