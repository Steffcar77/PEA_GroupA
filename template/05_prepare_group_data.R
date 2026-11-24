# 05_prepare_group_data.R
# Prepare the extra variables used by the ten group projects.
#
# Before running: run 01_prepare_data.R, or use its prepared teaching CSV.
# Keep this script, Dataset_V3.xlsx and teaching_panel_2020_2023.csv together.
#
# Outputs in the same working folder (existing files will be replaced):
#   project_panel_2020_2023.csv : project variables and one-year lags
#   patents_audit_2020_2025.csv : patent records with missing/duplicate flags
#
# These CSVs are inputs to 06_group_project.R. Their names are kept fixed;
# estimated-model results in later scripts use the results_ prefix.
# The source workbook and the original teaching CSV are not overwritten.


# 1. READ THE WORKBOOK AND EXISTING TEACHING PANEL ---------------------------

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Install readxl first, or run 00_setup.R.")
}

input <- "Dataset_V3.xlsx"
if (!file.exists(input) || !file.exists("teaching_panel_2020_2023.csv")) {
  stop("Keep Dataset_V3.xlsx and teaching_panel_2020_2023.csv in the working folder. Run 01_prepare_data.R first if needed.")
}

d <- read.csv("teaching_panel_2020_2023.csv", check.names = FALSE)
stopifnot(!anyDuplicated(d[c("id", "year")]))
n_input <- nrow(d)

# Skip the first category-label row, as in 01. Remove trailing spaces
# from column names and discard any source rows without a university ID.
m <- as.data.frame(
  readxl::read_excel(input, sheet = "Dataset 20-23", skip = 1)
)
names(m) <- trimws(names(m))
m <- m[!is.na(m$UNIVERSITY_ID), ]


# 2. ADD THE RESOURCE AND DOCTORAL VARIABLES FOR THE PROJECTS ----------------

# Short names on the left will be used in the project scripts.
# Keep these raw values as well as the transformed measures created below.
cols <- c(
  id = "UNIVERSITY_ID",
  year = "YEAR",
  fixed_term = "FIXED-TERM RESEARCHERS",
  adjunct = "ADJUNCT FACULTY",
  scholarship_pct = "PERCENTAGE OF DOCTORAL CANDIDATES RECEIVING A SCHOLARSHIP",
  phds_awarded = "PHDS AWARDED",
  eu_revenues = "REVENUES FROM THE EUROPEAN UNION AND REST OF THE WORLD",
  business_revenues = "REVENUES FROM ENTERPRISES",
  extraordinary_funding = "EXTRAORDINARY FUNDING PLANS"
)

extra <- m[unname(cols)]
names(extra) <- names(cols)
stopifnot(!anyDuplicated(extra[c("id", "year")]))

# Match BOTH university and year. A left join retains every observation
# in the teaching panel even if an added resource variable is missing.
d <- merge(d, extra, by = c("id", "year"), all.x = TRUE, sort = TRUE)
d <- d[order(d$id, d$year), ]
rownames(d) <- NULL
stopifnot(nrow(d) == n_input, !anyDuplicated(d[c("id", "year")]))

# All 2023 adjunct counts in this workbook are zero and need verification.
# Preserve them and flag the year; do not impute replacements. The common
# project specifications do not use adjunct counts as an explanatory variable.
d$adjunct_2023_needs_check <- d$year == 2023


# 3. CONSTRUCT LOGS, COMPOSITION MEASURES AND REVENUE INTENSITIES --------------

# Faculty is the denominator of several project measures. It must be positive.
stopifnot(all(is.finite(d$faculty)), all(d$faculty > 0))

# Use faculty as a size control for projects studying fellows or staff mix.
# These logs retain the definitions in the earlier project templates.
d$log_faculty <- log(d$faculty)
d$log_phd <- log(d$phd)

# Doctoral completions are an outcome for the scholarship project.
# Preserve zero completions in phds_awarded, but leave their logarithm missing.
# Do not silently change the estimand by replacing log(y) with log(1+y).
d$log_phds_awarded <- ifelse(
  d$phds_awarded > 0,
  log(pmax(d$phds_awarded, 1)),
  NA_real_
)
d$log_fellows <- log(d$fellows)

# Fixed-term researchers relative to faculty is a composition proxy.
# Dividing the fraction by 0.10 makes one unit a 10-percentage-point change.
# The scholarship source column is already expressed in percentage units:
# dividing by 10 likewise makes one unit a 10-percentage-point change.
d$fixed_term_share10 <- (d$fixed_term / d$faculty) / 0.10
d$scholarship_share10 <- d$scholarship_pct / 10

# Students per faculty member proxy teaching pressure; this does not directly
# measure teaching hours. Nonacademic staffing also serves multiple missions.
d$log_teaching_load <- log(d$students / d$faculty)
d$log_support <- log(d$nonacademic)

# Express annual receipts/funding per faculty member in thousands of euros.
# Keep these variables in levels, so an observed zero remains zero.
# The eu_revenues name follows the existing template, but its source covers
# BOTH the European Union and the rest of the world, not EU grants alone.
d$eu_per_faculty_1000 <- d$eu_revenues / d$faculty / 1000
d$business_per_faculty_1000 <- d$business_revenues / d$faculty / 1000
d$extraordinary_per_faculty_1000 <- d$extraordinary_funding / d$faculty / 1000


# 4. MATCH EACH UNIVERSITY TO ITS PREVIOUS CALENDAR YEAR --------------------

# For an outcome observed in year t, the starter projects use selected
# resources recorded in t-1. Lagged inputs are not necessarily exogenous:
# earlier performance can influence staffing, receipts and funding.
# An accounting receipt date is also not necessarily a grant-award date.
lagvars <- c(
  "log_faculty", "log_phd", "lcurrent", "log_fellows",
  "fixed_term_share10", "scholarship_share10", "log_teaching_load",
  "log_support", "eu_per_faculty_1000", "business_per_faculty_1000",
  "extraordinary_per_faculty_1000"
)

# Construct university-year keys and look up the SAME university at year-1.
# Do not merely shift the entire column by one row: that can cross a university
# boundary or treat a two-year gap as a one-year lag in an unbalanced panel.
key <- paste(d$id, d$year)
previous <- match(paste(d$id, d$year - 1L), key)

for (variable in lagvars) {
  d[[paste0("lag_", variable)]] <- d[[variable]][previous]
}

# Check that each successful match refers to the intended university/year.
matched <- !is.na(previous)
stopifnot(
  all(d$id[previous[matched]] == d$id[matched]),
  all(d$year[previous[matched]] == d$year[matched] - 1L),
  nrow(d) == 244L,
  sum(!is.na(d$lag_log_faculty)) == 183L
)

# There is no 2019 resource observation, so 2020 lags are missing. Keep all
# 244 original rows here. A lagged-resource model can use at most 183 rows
# (61 universities x 3 years), before additional missing-value/field restrictions.
# A field-adjusted project may therefore use fewer rows than this checkpoint.
write.csv(d, "project_panel_2020_2023.csv", row.names = FALSE, na = "")


# 5. PRESERVE AND AUDIT THE PATIRIS RECORDS ----------------------------------

# This is an audit dataset, not a cleaned patent-count panel. Keep unresolved
# records so that the patent project can examine their measurement problems.
p <- as.data.frame(readxl::read_excel(input, sheet = "PATIRIS"))
p <- p[!is.na(p$USTAT), c("USTAT", "PATIRIS NAME", "Year", "Total")]
names(p) <- c("ustat_name", "patiris_name", "year", "total_raw")
p$year <- as.integer(p$year)

# Retain total_raw exactly as imported. Nonnumeric placeholders such as N.A.
# become missing numeric totals; they must not be recoded to zero patents.
# Any genuinely observed numeric zero remains zero after this conversion.
p$patents <- suppressWarnings(as.numeric(p$total_raw))
p$missing_total <- is.na(p$patents)

# Flag BOTH rows in a duplicated institution-name/year pair. Dropping just
# the second row would assume the first record was the correct one.
# Leave resolution of the duplicate to the measurement audit.
p$duplicate_name_year <- duplicated(p[c("ustat_name", "year")]) |
  duplicated(p[c("ustat_name", "year")], fromLast = TRUE)


# 6. ATTACH UNIVERSITY IDs USING THE SUPPLIED CONCORDANCE --------------------

# In the Mapping sheet, columns 3 and 6 supply the source names and IDs.
# Skip the two introductory rows and read without treating the next row as headers.
mp <- as.data.frame(
  readxl::read_excel(input, sheet = "Mapping", col_names = FALSE, skip = 2)
)
valid <- !is.na(mp[[3]]) & !is.na(mp[[6]])

# Normalize spelling conventions for exact matching, as in 01. This helper
# removes accents, capitalization and punctuation; it does not guess matches.
norm_name <- function(x) {
  gsub("[^a-z0-9]", "", tolower(iconv(x, to = "ASCII//TRANSLIT")))
}
map <- setNames(mp[[6]][valid], norm_name(mp[[3]][valid]))
p$id <- unname(map[norm_name(p$ustat_name)])

# Retain the calendar-period labels used by the patent project. They are
# descriptive groups, not proof of when a particular patent or institution
# became treated. The application needs verified rules, effective dates,
# relevant ownership definitions and compatible patent observation dates.
p$reform_period <- ifelse(
  p$year < 2023, "before",
  ifelse(p$year == 2023, "transition", "after")
)

# Keep 2025 in this audit export to preserve the source records. A substantive
# trend analysis should exclude it until coverage completeness is established.
# Missing totals, duplicated keys and unmatched IDs remain visible in the file.
write.csv(p, "patents_audit_2020_2025.csv", row.names = FALSE, na = "")

cat("Project panel:", nrow(d), "rows; lagged-resource sample:",
    sum(!is.na(d$lag_log_faculty)), "rows.\n")
cat("PATIRIS:", nrow(p), "rows; observed totals:", sum(!p$missing_total),
    "; duplicate-key rows:", sum(p$duplicate_name_year), "\n")
cat("Unmatched PATIRIS university IDs:", sum(is.na(p$id)), "\n")
