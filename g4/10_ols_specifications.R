# 10_ols_specifications.R
# Group A, PEA project 4. Six main OLS specifications and one robustness column:
# outcome log_publications (t), exposure lag_log_teaching_load = log(S/F) in t-1.
#
#   (1) year effects                         (2) = (1) + lag_lcurrent
#   (3) year effects + 2018 field shares     (4) = (3) + lag_lcurrent
#   (5) university and year effects          (6) = (5) + lag_lcurrent  -> must equal 06
#   (7) = (6) with log(regular students / faculty) in t-1 as the load (robustness)
#
# Interpretation of the exposure coefficient. Every column controls for
# lag_log_faculty. Since log(S/F) = log S - log F, holding log F fixed the
# coefficient on log(S/F) equals the coefficient on log S: it is the association
# of MORE STUDENTS AT FIXED FACULTY, i.e. the chosen intervention.
# Without lag_lcurrent it targets the total effect; with lag_lcurrent it targets
# the effect at fixed resources, because current costs can be a mediator
# (more students -> more fees -> more spending -> research).
# These target labels require a causal ordering not established by the data:
# exposure, faculty and costs are all measured in t-1; costs may also precede demand.
# These are associations: the lag does not make the exposure randomly assigned.
#
# Standard errors as in 06: vcovCL(cluster = university, type = "HC1"),
# t inference with G - 1 degrees of freedom.
#
# Run through run_all_g4.R (working directory = PEA_GroupA).
# Outputs: output/tables/table_10_ols_long.csv, output/tables/table_10_ols_wide.csv

library(sandwich)
library(lmtest)

run_dir <- file.path("output", "template_run")
tab_dir <- file.path("output", "tables")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

a <- read.csv(file.path(run_dir, "results_group_04_sample.csv"), check.names = FALSE)
a <- a[order(a$id, a$year), ]
rownames(a) <- NULL
G <- length(unique(a$id))
stopifnot(nrow(a) == 180L, G == 60L)

exposure <- "lag_log_teaching_load"
fields <- c("medicine_share18", "engphys_share18", "ssh_share18")

# Robustness load measure for column (7): log(regular students / faculty) in t-1.
# "NUMBER OF REGULAR STUDENTS" is read from the workbook (template/, read only).
workbook <- as.data.frame(readxl::read_excel(file.path("template", "Dataset_V3.xlsx"),
                                             sheet = "Dataset 20-23", skip = 1))
names(workbook) <- trimws(names(workbook))
workbook <- workbook[!is.na(workbook$UNIVERSITY_ID), ]
panel <- read.csv(file.path(run_dir, "teaching_panel_2020_2023.csv"), check.names = FALSE)
key_prev <- paste(a$id, a$year - 1L)
regular_prev <- workbook[["NUMBER OF REGULAR STUDENTS"]][
  match(key_prev, paste(workbook$UNIVERSITY_ID, workbook$YEAR))]
faculty_prev <- panel$faculty[match(key_prev, paste(panel$id, panel$year))]
stopifnot(all(is.finite(regular_prev)), all(regular_prev > 0), all(is.finite(faculty_prev)))
a$lag_log_regular_load <- log(regular_prev / faculty_prev)


# 1. SPECIFICATIONS -------------------------------------------------------------

specs <- list(
  "(1)" = list(fields = FALSE, university = FALSE, costs = FALSE, load = exposure),
  "(2)" = list(fields = FALSE, university = FALSE, costs = TRUE,  load = exposure),
  "(3)" = list(fields = TRUE,  university = FALSE, costs = FALSE, load = exposure),
  "(4)" = list(fields = TRUE,  university = FALSE, costs = TRUE,  load = exposure),
  "(5)" = list(fields = FALSE, university = TRUE,  costs = FALSE, load = exposure),
  "(6)" = list(fields = FALSE, university = TRUE,  costs = TRUE,  load = exposure),
  "(7)" = list(fields = FALSE, university = TRUE,  costs = TRUE,  load = "lag_log_regular_load")
)
# In (5)-(7) the time-invariant 2018 field shares are absorbed by university effects.
# (7) = (6) with log(regular students / faculty) as the load measure (robustness).
load_labels <- c(lag_log_teaching_load = "enrolled students / faculty",
                 lag_log_regular_load = "regular students / faculty")

fit_column <- function(s) {
  rhs <- c(s$load, "lag_log_faculty", if (s$costs) "lag_lcurrent",
           if (s$fields) fields, if (s$university) "factor(id)", "factor(year)")
  fit <- lm(reformulate(rhs, response = "log_publications"), data = a, na.action = na.fail)
  V <- sandwich::vcovCL(fit, cluster = a$id, type = "HC1")
  list(fit = fit, V = V, tests = lmtest::coeftest(fit, vcov. = V, df = G - 1))
}
fits <- lapply(specs, fit_column)


# 2. LONG TABLE -----------------------------------------------------------------

critical <- qt(0.975, df = G - 1)

# Within R-squared (university-effect columns only): share of the variation of the
# outcome around each university's mean explained by the other regressors.
within_r2 <- function(fit, s) {
  if (!s$university) return(NA_real_)
  y_within <- a$log_publications - ave(a$log_publications, a$id)
  1 - sum(resid(fit)^2) / sum(y_within^2)
}

long_rows <- list()
for (column in names(specs)) {
  s <- specs[[column]]
  tests <- fits[[column]]$tests
  for (term in intersect(c(s$load, "lag_log_faculty", "lag_lcurrent"), rownames(tests))) {
    b <- tests[term, "Estimate"]
    se <- tests[term, "Std. Error"]
    long_rows[[paste(column, term)]] <- data.frame(
      column = column, term = if (term == s$load) "load" else term,
      load_measure = load_labels[[s$load]], estimate = b, cluster_se = se,
      lo = b - critical * se, hi = b + critical * se, p = tests[term, "Pr(>|t|)"],
      field_shares = s$fields, university_effects = s$university, cost_control = s$costs,
      target = if (s$costs) "fixed resources" else "total",
      n = nobs(fits[[column]]$fit), universities = G,
      r_squared = summary(fits[[column]]$fit)$r.squared,
      r_squared_within = within_r2(fits[[column]]$fit, s)
    )
  }
}
long <- do.call(rbind, long_rows)
rownames(long) <- NULL


# 3. CHECK: COLUMN (6) REPRODUCES THE COEFFICIENT OF 06 -------------------------

ref <- read.csv(file.path(run_dir, "results_group_04_mean_coefficients.csv"))
col6 <- long[long$column == "(6)" & long$term == "load", ]
if (!isTRUE(all.equal(col6$estimate, ref$estimate, tolerance = 1e-8)) ||
    !isTRUE(all.equal(col6$cluster_se, ref$cluster_se, tolerance = 1e-8))) {
  stop("Column (6) does not reproduce 06: estimate ", col6$estimate, " vs ", ref$estimate,
       "; SE ", col6$cluster_se, " vs ", ref$cluster_se)
}
cat("Check passed: column (6) reproduces the coefficient and cluster SE of 06.\n")

write.csv(long, file.path(tab_dir, "table_10_ols_long.csv"), row.names = FALSE)


# 4. WIDE TABLE (REPORT FORMAT) -------------------------------------------------

cell <- function(column, term) {
  x <- long[long$column == column & long$term == term, ]
  if (!nrow(x)) return(c("", ""))
  c(sprintf("%.3f", x$estimate), sprintf("(%.3f)", x$cluster_se))
}
yes_no <- function(x) ifelse(x, "yes", "no")
wide <- data.frame(row = c("log(teaching load), t-1", "", "log faculty, t-1", "",
                           "log current costs, t-1", "", "95% CI, teaching load",
                           "Load measure", "Year effects", "2018 field shares",
                           "University effects",
                           "Target", "N", "Universities", "R-squared",
                           "Within R-squared"))
for (column in names(specs)) {
  s <- specs[[column]]
  x <- long[long$column == column & long$term == "load", ]
  wide[[column]] <- c(cell(column, "load"), cell(column, "lag_log_faculty"),
                      cell(column, "lag_lcurrent"),
                      sprintf("[%.2f, %.2f]", x$lo, x$hi), x$load_measure,
                      "yes", if (s$university) "absorbed" else yes_no(s$fields),
                      yes_no(s$university), x$target, x$n, x$universities,
                      sprintf("%.3f", x$r_squared),
                      if (s$university) sprintf("%.3f", x$r_squared_within) else "")
}
print(wide, right = FALSE)
write.csv(wide, file.path(tab_dir, "table_10_ols_wide.csv"), row.names = FALSE)

cat("Cluster-robust SEs in parentheses (university clusters, HC1, df = G - 1).\n")
cat("10 completed: table_10_ols_long.csv and table_10_ols_wide.csv written.\n")
