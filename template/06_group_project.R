# 06_group_project.R
# Run one of the ten group projects using the CSVs prepared in 01 and 05.
# Keep this script, the CSVs and all results in the same workshop folder.
#
# In the RStudio Console, choose a project BEFORE sourcing this script:
#   GROUP <- 1L
#   RUN_FRONTIER <- FALSE
#   source("06_group_project.R")
#
# To switch projects, assign a new GROUP value in the Console and source again.
# Groups 1-8 can optionally set RUN_FRONTIER <- TRUE for the pooled SFA models.
# In a fresh terminal session, Rscript 06_group_project.R 3 selects group 3.
# Outputs start with results_group_XX, so different groups have distinct files.
# Rerunning the SAME group replaces its existing result files.
#
# These empirical exercises estimate associations or audit measurement.
# A causal question is not automatically answered by adding a variable to SFA.


# 1. SELECT A PROJECT AND CHECK ITS INPUT FILE ------------------------------

if (!exists("GROUP", inherits = FALSE)) {
  args <- commandArgs(trailingOnly = TRUE)
  GROUP <- if (length(args)) as.integer(args[1]) else 1L
}
stopifnot(length(GROUP) == 1L, !is.na(GROUP), GROUP %in% 1:10)

if (!exists("RUN_FRONTIER", inherits = FALSE)) {
  RUN_FRONTIER <- FALSE
}
stopifnot(is.logical(RUN_FRONTIER), length(RUN_FRONTIER) == 1L, !is.na(RUN_FRONTIER))

project_titles <- c(
  "Research fellows", "Fixed-term researcher composition",
  "Doctoral scholarships", "Teaching pressure",
  "Technical and administrative support", "International research receipts",
  "Business research receipts", "Extraordinary funding",
  "COVID and field composition", "Patent ownership and measurement"
)
project_title <- project_titles[GROUP]
tag <- sprintf("results_group_%02d", GROUP)
cat("Group", GROUP, ":", project_title, "\n")

input_file <- if (GROUP <= 8) {
  "project_panel_2020_2023.csv"
} else if (GROUP == 9) {
  "scopus_panel_2018_2025.csv"
} else {
  "patents_audit_2020_2025.csv"
}
if (!file.exists(input_file)) {
  stop("Missing ", input_file, ". Run 01_prepare_data.R and 05_prepare_group_data.R first.")
}


# 2. GROUPS 1-8: DEFINE THE OUTCOME, EXPOSURE AND COMMON SAMPLE ---------------

if (GROUP <= 8) {
  library(sandwich)
  library(lmtest)

  d <- read.csv(input_file)
  stopifnot(!anyDuplicated(d[c("id", "year")]))

  # The exposure is a previous-year resource/composition measure.
  # Groups 1, 4 and 5 use logged exposures. Groups 2 and 3 use units of
  # 10 percentage points. Groups 6-8 use thousands of euros per faculty member.
  exposures <- c(
    "lag_log_fellows", "lag_fixed_term_share10", "lag_scholarship_share10",
    "lag_log_teaching_load", "lag_log_support", "lag_eu_per_faculty_1000",
    "lag_business_per_faculty_1000", "lag_extraordinary_per_faculty_1000"
  )
  exposure_name <- exposures[GROUP]
  outcome_name <- if (GROUP == 3) "log_phds_awarded" else "log_publications"

  # Hold previous-year faculty and current-management costs fixed.
  # The doctoral project also controls for the previous-year doctoral stock.
  controls <- c("lag_log_faculty", "lag_lcurrent")
  if (GROUP == 3) {
    controls <- c(controls, "lag_log_phd")
  }
  fields <- c("medicine_share18", "engphys_share18", "ssh_share18")

  # Retain field-complete observations even for OLS, so the optional pooled
  # frontiers use the same sample. Fixed field shares are absorbed by the
  # university effects in OLS; they enter explicitly in the pooled SFA models.
  needed <- c("id", "year", outcome_name, exposure_name, controls, fields)
  keep <- complete.cases(d[needed])
  for (variable in c(outcome_name, exposure_name, controls, fields)) {
    keep <- keep & is.finite(d[[variable]])
  }
  a <- d[keep, ]
  a <- a[order(a$id, a$year), ]
  rownames(a) <- NULL
  a$outcome <- a[[outcome_name]]
  a$exposure <- a[[exposure_name]]

  G <- length(unique(a$id))
  observed_years <- sort(unique(a$year))
  stopifnot(nrow(a) > 0L, G > 1L, length(observed_years) > 1L)

  # Explicit indicators make the frontier design independent of factor parsing.
  # With the supplied lagged sample, 2021 is the reference year.
  year_terms <- paste0("y", observed_years[-1L])
  for (j in seq_along(year_terms)) {
    a[[year_terms[j]]] <- as.integer(a$year == observed_years[-1L][j])
  }

  cat("Outcome:", outcome_name, "\nExposure:", exposure_name, "\n")
  cat("Estimation sample:", nrow(a), "rows;", G, "universities.\n")
  write.csv(a, paste0(tag, "_sample.csv"), row.names = FALSE)


  # 3. FIT A UNIVERSITY/YEAR FIXED-EFFECTS MEAN REGRESSION -------------------

  # The coefficient uses within-university changes after common year shifts
  # and the listed controls. A lag is not a source of random assignment.
  mean_formula <- reformulate(
    c("exposure", controls, "factor(id)", "factor(year)"),
    response = "outcome"
  )
  print(mean_formula)
  mean_fit <- lm(mean_formula, data = a, na.action = na.fail)
  V <- sandwich::vcovCL(mean_fit, cluster = a$id, type = "HC1")
  tests <- lmtest::coeftest(mean_fit, vcov. = V, df = G - 1)
  print(tests["exposure", , drop = FALSE])

  b <- unname(coef(mean_fit)["exposure"])
  se <- sqrt(unname(V["exposure", "exposure"]))
  stopifnot(is.finite(b), is.finite(se), se > 0)
  critical <- qt(0.975, df = G - 1)
  mean_result <- data.frame(
    group = GROUP, outcome = outcome_name, exposure = exposure_name,
    estimate = b, cluster_se = se,
    p = 2 * pt(-abs(b / se), df = G - 1),
    lo = b - critical * se, hi = b + critical * se,
    n = nobs(mean_fit), universities = G
  )
  write.csv(mean_result, paste0(tag, "_mean_coefficients.csv"), row.names = FALSE)
  saveRDS(list(fit = mean_fit, covariance = V), paste0(tag, "_mean_fit.rds"))

  # This unadjusted scatterplot describes the raw association, not the
  # within-university coefficient just estimated. Save it with the group tag.
  png(paste0(tag, "_scatter.png"), width = 1400, height = 1000, res = 150)
  plot(a$exposure, a$outcome, pch = 19, col = "darkgreen",
       xlab = exposure_name, ylab = outcome_name, main = project_title)
  dev.off()

  cat("Lagging exposure does not eliminate reverse causality or omitted variables.\n")
  cat("Controls must be justified by their timing and the intended causal estimand.\n")


  # 4. OPTIONAL: TWO DIRECT POOLED STOCHASTIC-FRONTIER CALLS -----------------

  if (RUN_FRONTIER) {
    library(sfa)

    # First placement: exposure shifts the frontier. University fixed effects
    # are not carried into this pooled likelihood. Frozen field shares enter
    # as observed controls, but do not absorb all university heterogeneity.
    production_formula <- reformulate(
      c(controls, fields, year_terms, "exposure"), response = "outcome"
    )
    print(production_formula)
    frontier_fit <- sfa::sfm(
      production_formula, model_name = "NHN", data = a,
      inefdec = TRUE, PSopt = FALSE, maxit.optim = 2000, trace = 0
    )
    if (frontier_fit$opt$convergence != 0L) {
      frontier_fit <- sfa::sfm(
        production_formula, model_name = "NHN", data = a,
        start_val = unname(coef(frontier_fit)),
        inefdec = TRUE, PSopt = FALSE, maxit.optim = 2000, trace = 0
      )
    }
    summary(frontier_fit)

    # Alternative placement: exposure changes log sigma_u under the default
    # NHN_Z link. The first intercept is the frontier intercept; the final
    # intercept belongs to the inefficiency-scale equation.
    # A negative scale slope reduces E[u|exposure] under this maintained model.
    rhs <- paste(c(controls, fields, year_terms), collapse = " + ")
    inefficiency_formula <- as.formula(paste("outcome ~", rhs, "| exposure"))
    print(inefficiency_formula)
    inefficiency_fit <- sfa::sfm(
      inefficiency_formula, model_name = "NHN_Z", data = a,
      inefdec = TRUE, PSopt = FALSE, maxit.optim = 2000, trace = 0
    )
    if (inefficiency_fit$opt$convergence != 0L) {
      inefficiency_fit <- sfa::sfm(
        inefficiency_formula, model_name = "NHN_Z", data = a,
        start_val = unname(coef(inefficiency_fit)),
        inefdec = TRUE, PSopt = FALSE, maxit.optim = 2000, trace = 0
      )
    }
    summary(inefficiency_fit)

    # These are different maintained placements of the exposure, not a test
    # that identifies the true causal channel. Do not select a channel solely
    # because its likelihood or a reported t statistic is larger.
    # Inputs measured in t-1 explain output in t here; these are lagged-resource
    # production specifications, not contemporaneous technical-efficiency models.
    optional_fits <- list(frontier = frontier_fit, inefficiency = inefficiency_fit)
    coefficient_rows <- list()
    diagnostic_rows <- list()

    for (placement in names(optional_fits)) {
      fit <- optional_fits[[placement]]
      cf <- coef(fit)
      coefficient_rows[[placement]] <- data.frame(
        placement = placement, term = names(cf), estimate = unname(cf)
      )

      # Inspect the noise boundary as well as the optimiser's code. The
      # NHN and NHN_Z outputs parameterise the noise scale differently.
      sv <- if (placement == "frontier") {
        unname(cf["sigma"] / sqrt(1 + cf["lambda"]^2))
      } else {
        unname(cf["sigma_v"])
      }
      H <- fit$opt$hessian
      h <- tryCatch(eigen(H, symmetric = TRUE, only.values = TRUE)$values,
                    error = function(e) NA_real_)
      condition <- tryCatch(kappa(H, exact = TRUE), error = function(e) NA_real_)
      diagnostic_rows[[placement]] <- data.frame(
        placement = placement, n = nrow(a), universities = G,
        convergence = fit$opt$convergence,
        logLik = -as.numeric(fit$opt$value), sigma_v = sv,
        boundary_noise = sv < 0.001,
        min_hessian_eigenvalue = if (anyNA(h)) NA_real_ else min(h),
        hessian_condition = condition,
        hessian_pd = if (anyNA(h)) NA else all(h > 0)
      )
      capture.output(summary(fit), file = paste0(tag, "_", placement, "_summary.txt"))
    }

    diagnostics <- do.call(rbind, diagnostic_rows)
    print(diagnostics)
    write.csv(diagnostics, paste0(tag, "_sfa_diagnostics.csv"), row.names = FALSE)
    write.csv(do.call(rbind, coefficient_rows),
              paste0(tag, "_sfa_coefficients.csv"), row.names = FALSE)
    saveRDS(optional_fits, paste0(tag, "_sfa_fits.rds"))

    # Model-based SFA standard errors are not university-cluster standard
    # errors. Even code 0 and a positive Hessian do not establish global
    # optimisation or causal identification. Retain flagged fits as provisional.
    bad_curvature <- is.na(diagnostics$hessian_pd) | !diagnostics$hessian_pd
    if (any(diagnostics$convergence != 0L | diagnostics$boundary_noise | bad_curvature)) {
      warning("Optional SFA has convergence, boundary or curvature flags; inspect the saved diagnostics.")
    }
  }


# 5. GROUP 9: MEDICAL COMPOSITION AND PANDEMIC-YEAR ASSOCIATIONS --------------

} else if (GROUP == 9) {
  library(sandwich)
  library(lmtest)

  a <- read.csv(input_file)
  stopifnot(!anyDuplicated(a[c("id", "year")]))
  a <- subset(a, year <= 2024 & !is.na(medicine_share18))
  a <- a[complete.cases(a[c("id", "year", "log_publications")]) &
           is.finite(a$log_publications) & is.finite(a$medicine_share18), ]
  rownames(a) <- NULL

  # One dose unit is a 10-percentage-point higher 2018 medical assignment
  # share. This is a field-composition proxy, not randomized pandemic exposure.
  # This Scopus sample can include institutions outside the resource panel.
  a$dose <- a$medicine_share18 / 0.10
  years <- c(2018, 2020:2024)
  stopifnot(2019 %in% a$year, all(years %in% a$year))
  for (yr in years) {
    a[[paste0("e", yr)]] <- a$dose * as.integer(a$year == yr)
  }

  # Omit dose*2019: each coefficient is a dose-slope contrast against 2019.
  # There is only ONE earlier contrast, 2018, so pretrend evidence is limited.
  event_terms <- paste0("e", years)
  event_formula <- reformulate(
    c(event_terms, "factor(id)", "factor(year)"), response = "log_publications"
  )
  event_fit <- lm(event_formula, data = a, na.action = na.fail)
  G <- length(unique(a$id))
  V <- sandwich::vcovCL(event_fit, cluster = a$id, type = "HC1")
  tests <- lmtest::coeftest(event_fit, vcov. = V, df = G - 1)
  print(tests[event_terms, , drop = FALSE])
  write.csv(tests[event_terms, , drop = FALSE],
            paste0(tag, "_event_associations.csv"), row.names = TRUE)
  write.csv(a, paste0(tag, "_sample.csv"), row.names = FALSE)
  saveRDS(list(fit = event_fit, covariance = V), paste0(tag, "_event_fit.rds"))

  cat("Sample:", nrow(a), "rows;", G, "universities.\n")
  cat("Field-specific output shocks or coverage changes can confound this comparison.\n")
  cat("Inputs begin in 2020, preventing a before/after frontier decomposition with these resource data.\n")
  cat("Use the common frontier exercise in 02 for SFA practice; no group-9 frontier is fitted here.\n")


# 6. GROUP 10: PATENT MEASUREMENT AND SAMPLE COMPOSITION ---------------------

} else {
  a <- read.csv(input_file)

  # Missing totals are not zero patents. Inspect the coverage by year and
  # both members of a duplicate key before considering any regression.
  missingness <- as.data.frame(with(a, table(year, missing_total)))
  print(missingness)
  duplicates <- a[a$duplicate_name_year,
                  c("ustat_name", "year", "total_raw", "id")]
  print(duplicates)
  cat("Observed zeros:", sum(a$patents == 0, na.rm = TRUE), "\n")
  write.csv(missingness, paste0(tag, "_missingness.csv"), row.names = FALSE)
  write.csv(duplicates, paste0(tag, "_duplicate_records.csv"), row.names = FALSE)

  # Exclude provisional 2025 and ALL flagged duplicate rows from this limited
  # descriptive summary. This does not resolve the underlying duplicate.
  use <- subset(a, year <= 2024 & !missing_total & !duplicate_name_year)
  stopifnot(nrow(use) > 0L, all(is.finite(use$patents)))
  n_by_year <- aggregate(patents ~ year, data = use, FUN = length)
  mean_by_year <- aggregate(patents ~ year, data = use, FUN = mean)
  names(n_by_year)[2] <- "n_observed"
  names(mean_by_year)[2] <- "mean_patents"
  descriptive <- merge(n_by_year, mean_by_year, by = "year")
  print(descriptive)
  write.csv(descriptive, paste0(tag, "_observed_patent_summary.csv"), row.names = FALSE)
  write.csv(use, paste0(tag, "_observed_patent_sample.csv"), row.names = FALSE)

  # The institutions observed can change across years. These means do not
  # describe changes in a fixed cohort unless comparable coverage is established.
  # The before/transition/after labels in 05 do not themselves establish legal
  # treatment dates or which patent records are exposed to a rule change.
  cat("Clarify applicant/inventor, priority/grant dates and patent families.\n")
  cat("Verify policy rules and effective dates before defining treatment.\n")
  cat("No causal or count-frontier model is fitted to this unresolved series.\n")
  cat("Use the common frontier exercise in 02 for SFA practice.\n")
}
