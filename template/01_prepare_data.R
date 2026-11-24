# 01_prepare_data.R
# Rebuild the three prepared datasets from Dataset_V3.xlsx.
#
# Before running:
#   Open the workshop's RStudio project. Keep this script, the workbook
#   and the prepared CSVs in that same working folder.
#
# Outputs (existing files with these names will be replaced):
#   teaching_panel_2020_2023.csv : resources matched to research outputs
#   scopus_panel_2018_2025.csv  : longer publication/citation series
#   incites_panel_2020_2025.csv : category-normalized citation indicators
#
# The source Excel workbook is read only; it is never overwritten.


# 1. CHECK THE PACKAGE AND WORKBOOK -----------------------------------------

# readxl imports Excel sheets. Other operations below use base R.
if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Install readxl first, or run 00_setup.R.")
}

input <- "Dataset_V3.xlsx"

if (!file.exists(input)) {
  stop(
    "Dataset_V3.xlsx is missing from the working folder. ",
    "Open the workshop project first."
  )
}

# This import helper avoids repeating the same Excel-reading commands.
# trimws() removes accidental spaces at the ends of column names.
read_tab <- function(sheet, skip = 0L) {
  a <- as.data.frame(
    readxl::read_excel(input, sheet = sheet, skip = skip)
  )
  names(a) <- trimws(names(a))
  a
}

# The main sheet begins with a category-label row. Skip it so that the
# next row supplies the actual column names. Other sheets need no skip.
main <- read_tab("Dataset 20-23", skip = 1)
scopus <- read_tab("Scopus")
v19 <- read_tab("VQR 15-19")
v11 <- read_tab("VQR 11-14")

# A university-year pair must identify a single row in each source panel.
# Duplicate keys would multiply observations when the panels are merged.
stopifnot(
  !anyDuplicated(main[c("UNIVERSITY_ID", "YEAR")]),
  !anyDuplicated(scopus[c("UNIVERSITY_ID", "YEAR")])
)


# 2. SELECT AND RENAME THE RESOURCE VARIABLES -------------------------------

# Left: short names used in the teaching scripts.
# Right: the corresponding column names in the source workbook.
mapping <- c(
  id = "UNIVERSITY_ID",
  year = "YEAR",
  faculty = "INTERNAL TEACHING STAFF",
  fellows = "RESEARCH FELLOWS",
  nonacademic = "NON-ACADEMIC STAFF",
  current_management = "OPERATING COSTS: CURRENT MANAGEMENT",
  operating_costs = "OPERATING COSTS",
  graduates = "NUMBER OF GRADUATES",
  phd = "STUDENTS ENROLLED IN PHD PROGRAMMES",
  students = "NUMBER OF ENROLLED STUDENTS",
  performance_funding = "PERFORMANCE-BASED FUNDING QUOTA",
  ffo = "FFO (DM)"
)

d <- main[unname(mapping)]
names(d) <- names(mapping)

# Select annual Scopus outputs and use the same university/year key names.
s <- scopus[c(
  "UNIVERSITY_ID", "YEAR", "NUMBER OF PUBLICATIONS", "CITATIONS"
)]
names(s) <- c("id", "year", "publications", "citations")


# 3. PREPARE UNIVERSITY-LEVEL VQR AND EXPOSURE MEASURES ----------------------

# These VQR measures describe an assessment window, not an annual series.
# They will be attached to each annual row using the university ID.
v <- v19[c("UNIVERSITY_ID", "R1", "IRFS", "IRAS2")]
names(v) <- c("id", "r1_1519", "irfs_1519", "iras2_1519")

# Retain the earlier-window measures and the existing normalization of
# IRAS1 by the expected-products share. This is not a funding shock.
old <- data.frame(
  id = v11$UNIVERSITY_ID,
  q_1114 = as.numeric(v11$IRAS1) /
    as.numeric(v11[["EXPECTED PRODUCTS OVER THE UNIVERSITY TOTAL"]]),
  irfs_1114 = as.numeric(v11$IRFS)
)

# all.x = TRUE retains every university in v even if an older score is missing.
v <- merge(v, old, by = "id", all.x = TRUE)

# R1 measures quality relative to the national reference. Center at 1
# and divide by 0.10: one dose unit represents a 0.10 increase in R1.
# This rescaling does not make the score an exogenous treatment.
v$dose_r1 <- (v$r1_1519 - 1) / 0.10

# A second exposure describes baseline dependence on performance funding.
# Freeze the share at its 2020 value rather than updating it after 2022.
# One dose unit represents 10 percentage points of baseline funding share.
base <- d[d$year == 2020, c("id", "performance_funding", "ffo")]
base$dose_dependence <- (base$performance_funding / base$ffo) / 0.10

v <- merge(
  v, base[c("id", "dose_dependence")],
  by = "id", all.x = TRUE
)

# There must be one set of assessment/exposure measures per university.
stopifnot(!anyDuplicated(v$id))


# 4. CONSTRUCT FIELD-COMPOSITION CONTROLS FROM 2018 -------------------------

# Scopus subject categories overlap: one publication can have several
# subject assignments. These are shares of assignments, not mutually
# exclusive shares of publications.
#
# Freeze composition in 2018 so these controls do not change after the
# funding event used in the later exposure exercise.
f <- scopus[scopus$YEAR == 2018, ]

# In this workbook, columns 8 onward contain the subject counts.
# Check that layout before applying this script to a different workbook.
total <- rowSums(f[8:ncol(f)])

# A university with no subject assignments has an undefined composition.
# Its shares remain missing; setting them to zero would change their meaning.
denom <- ifelse(total > 0, total, NA_real_)

field <- data.frame(
  id = f$UNIVERSITY_ID,
  medicine_share18 = f$Medicine / denom,
  engphys_share18 = rowSums(f[c(
    "Engineering", "Computer Science", "Materials Science",
    "Physics and Astronomy"
  )]) / denom,
  ssh_share18 = rowSums(f[c(
    "Arts and Humanities", "Business, Management and Accounting",
    "Economics, Econometrics and Finance", "Psychology", "Social Sciences"
  )]) / denom
)


# 5. BUILD THE LONGER SCOPUS OUTPUT PANEL -----------------------------------

# Keep all Scopus university-year observations, including years for which
# the main resource sheet supplies no inputs. Missing inputs are not filled in.
long <- merge(s, v, by = "id", all.x = TRUE)
long <- merge(long, field, by = "id", all.x = TRUE)
long <- long[order(long$id, long$year), ]

long$log_publications <- log(long$publications)

# A zero citation count has no finite logarithm. Preserve such values as
# missing in the log variable rather than silently adding one to all counts.
long$log_citations <- ifelse(
  long$citations > 0,
  log(pmax(long$citations, 1)),
  NA_real_
)

# The exploratory funding-timing exercise uses 2022 as the first post year.
# This is distinct from the end date of a VQR assessment window.
# Multiplying the frozen dose by post22 creates a dose-by-post regressor.
long$post22 <- as.integer(long$year >= 2022)
long$r1_post22 <- long$dose_r1 * long$post22
long$dependence_post22 <- long$dose_dependence * long$post22


# 6. MATCH OUTPUTS TO OBSERVED RESOURCES AND TRANSFORM INPUTS ----------------

# Match on BOTH university and year. This inner join retains matched rows;
# it does not attach a university's outputs from one year to another year.
panel <- merge(d, long, by = c("id", "year"))
panel <- panel[order(panel$id, panel$year), ]
rownames(panel) <- NULL

# The teaching labor input combines internal faculty and research fellows.
# It is not simply the faculty count used in the first descriptive plot.
panel$labor <- panel$faculty + panel$fellows

# Verify the expected balanced resource-output panel before taking logs.
stopifnot(
  nrow(panel) == 244L,
  length(unique(panel$id)) == 61L,
  all(table(panel$id) == 4L),
  all(panel$labor > 0),
  all(panel$current_management > 0),
  !anyNA(panel[c("publications", "labor", "current_management", "r1_1519")])
)

# Logs supply the inputs used in the log-linear production equations.
# Current-management spending is expressed in euro millions, at current prices.
panel$llabor <- log(panel$labor)
panel$lcurrent <- log(panel$current_management / 1e6)
panel$lgrads <- log(panel$graduates)
panel$lphd <- log(panel$phd)
panel$lnonacademic <- log(panel$nonacademic)
panel$performance_share <- panel$performance_funding / panel$ffo
panel$lffo <- log(panel$ffo)

# Explicit year indicators leave 2020 as the reference category.
for (yr in 2021:2023) {
  panel[[paste0("y", yr)]] <- as.integer(panel$year == yr)
}


# 7. MATCH INCITES USING THE WORKBOOK'S CONCORDANCE --------------------------

# The Mapping sheet supplies the name-to-ID concordance. Skip its first
# two rows; columns 3 and 6 contain the names and university IDs used here.
# Import without headers because these are positional concordance columns.
mp <- as.data.frame(
  readxl::read_excel(input, sheet = "Mapping", col_names = FALSE, skip = 2)
)
valid <- !is.na(mp[[3]]) & !is.na(mp[[6]])

# Normalize accents, capitalization and punctuation for exact matching.
# This does not perform fuzzy matching or guess unmatched institutions.
norm_name <- function(x) {
  gsub("[^a-z0-9]", "", tolower(iconv(x, to = "ASCII//TRANSLIT")))
}

key <- setNames(mp[[6]][valid], norm_name(mp[[3]][valid]))
inc <- read_tab("Dati InCites")
inc$id <- unname(key[norm_name(inc$USTAT)])

# Stop if an institution is unmatched or a university-year key is duplicated.
stopifnot(
  !anyNA(inc$id),
  !anyDuplicated(inc[c("id", "Year")])
)

inc <- data.frame(
  id = inc$id,
  year = inc$Year,
  cnci = inc[["Category Normalized Citation Impact"]],
  wos_documents = inc[["Web of Science Documents"]]
)

# Attach the same university-level scores and construct the same interaction.
# A normalized citation indicator still requires scrutiny of citation windows.
inc <- merge(inc, v, by = "id", all.x = TRUE)
inc$log_cnci <- log(inc$cnci)
inc$post22 <- as.integer(inc$year >= 2022)
inc$r1_post22 <- inc$dose_r1 * inc$post22

# Add available InCites outcomes without dropping resource-panel observations.
panel <- merge(
  panel, inc[c("id", "year", "cnci", "wos_documents")],
  by = c("id", "year"), all.x = TRUE
)
panel <- panel[order(panel$id, panel$year), ]
rownames(panel) <- NULL

# Every exported dataset must have unique university-year keys.
for (obj in c("panel", "long", "inc")) {
  stopifnot(!anyDuplicated(get(obj)[c("id", "year")]))
}


# 8. WRITE PREPARED DATASETS TO THE WORKING FOLDER ---------------------------

# These filenames are the inputs expected by the later teaching scripts.
# Missing values are written as blank CSV cells, not replaced with zero.
write.csv(
  panel, "teaching_panel_2020_2023.csv",
  row.names = FALSE, na = ""
)
write.csv(
  long, "scopus_panel_2018_2025.csv",
  row.names = FALSE, na = ""
)
write.csv(
  inc, "incites_panel_2020_2025.csv",
  row.names = FALSE, na = ""
)

cat("Prepared 244 input-output rows; 61 universities; 2020-2023.\n")
cat("Scopus: 536 rows; 67 universities; 2018-2025.\n")
cat("Siena Stranieri has zero 2018 subject assignments; its field shares remain missing.\n")
