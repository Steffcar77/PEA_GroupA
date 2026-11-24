# 03_exposure_diagnostics.R
# Explore publication changes associated with VQR-related exposure measures.
#
# Before running: run 01_prepare_data.R, or use the supplied prepared CSVs.
# Keep this script and all three CSVs in the same workshop folder.
# Outputs are saved there with a results_ filename prefix.
#
# This script estimates OLS models with university and year fixed effects.
# It does not fit stochastic frontiers or identify a national VQR effect.
# The R1 score reflects earlier performance, not random policy assignment.


# 1. READ THE PREPARED DATA AND DEFINE THE COMPARISON SAMPLE -----------------

library(sandwich)

panel <- read.csv("teaching_panel_2020_2023.csv", check.names = FALSE)
long <- read.csv("scopus_panel_2018_2025.csv", check.names = FALSE)
inc <- read.csv("incites_panel_2020_2025.csv", check.names = FALSE)

# A university-year pair must identify one observation in each panel.
stopifnot(
  !anyDuplicated(panel[c("id", "year")]),
  !anyDuplicated(long[c("id", "year")]),
  !anyDuplicated(inc[c("id", "year")])
)

# Inputs are observed for 61 universities in 2020-2023. Scopus includes
# additional institutions and years. Restricting by ID makes the longer
# output series refer to the institutions covered by the resource panel.
ids <- unique(panel$id)
long61 <- subset(long, year <= 2024 & id %in% ids)

# Exclude 2025 until the completeness of its publication coverage is clear.
# Keep the longer output series for timing checks; do not invent inputs
# for years outside the observed resource panel.


# 2. UNDERSTAND THE EXPOSURE VARIABLES BEFORE ESTIMATING ---------------------

# Preparation defined:
#   dose_r1         = (R1_2015_2019 - 1) / 0.10
#   dose_dependence = (2020 performance-funding share) / 0.10
#   post22          = 1 in 2022 and later, 0 otherwise
#   r1_post22       = dose_r1 * post22
#   dependence_post22 = dose_dependence * post22
#
# One R1 dose unit is 0.10 in R1. One dependence dose unit is 10 percentage
# points of the 2020 funding share. Both doses are frozen across years.
# The exercise uses 2022 funding timing, not the assessment-window endpoint.
#
# University effects absorb the main effect of a fixed university dose.
# Year effects absorb the common post indicator and other common year shifts.
# The interaction measures whether post-period changes differ with dose.
#
# There is no untreated Italian group here. Moreover, the R1 assessment
# window overlaps some pre-period output years. Pre-period associations
# can therefore reflect how the score itself was constructed.


# 3. ESTIMATE THE MAIN COMPARISON AND SENSITIVITY SPECIFICATIONS --------------

# Each specification states its sample, outcome and regressors of interest.
# University and year indicators will be added to every formula below.
# RF labels retain the names in the earlier files, but describe associations.
specs <- list(
  list(label = "RF67_2018_2024", data = subset(long, year <= 2024),
       outcome = "log_publications", terms = "r1_post22"),
  list(label = "RF61_2018_2024", data = long61,
       outcome = "log_publications", terms = "r1_post22"),
  list(label = "RF61_2018_2023", data = subset(long61, year <= 2023),
       outcome = "log_publications", terms = "r1_post22"),
  list(label = "RF61_2020_2023", data = panel,
       outcome = "log_publications", terms = "r1_post22"),
  list(label = "Conditional61_2020_2023", data = panel,
       outcome = "log_publications", terms = c("llabor", "lcurrent", "r1_post22")),
  list(label = "Funding_share61_2020_2023", data = panel,
       outcome = "performance_share", terms = "r1_post22"),
  list(label = "Funding_total61_2020_2023", data = panel,
       outcome = "lffo", terms = "r1_post22"),
  list(label = "Dependence61_2018_2024", data = long61,
       outcome = "log_publications", terms = "dependence_post22"),
  list(label = "CNCI61_2020_2024",
       data = subset(inc, year <= 2024 & id %in% ids),
       outcome = "log_cnci", terms = "r1_post22")
)

# Why these comparisons?
# - The 67/61 contrast changes the institution coverage.
# - Shorter windows assess sensitivity to which years are included.
# - Compare the conditional model with RF61_2020_2023 on the same period.
#   Current inputs may themselves respond to funding. Conditioning on them
#   can remove part of a policy's resource channel; it does not simply
#   improve identification of a total effect.
# - Funding outcomes describe the proposed transmission channel. A finding
#   here alone does not establish a valid instrument or causal mechanism.
# - CNCI is a different outcome; citation windows still need scrutiny.

ans <- list()
table_rows <- list()

# This loop runs the same visible lm() and covariance commands for each
# specification. There is no user-defined estimation wrapper.
for (s in specs) {
  a <- s$data
  needed <- c("id", "year", s$outcome, s$terms)

  # Remove missing IDs/years and missing or non-finite numerical variables
  # BEFORE fitting. Thus the covariance clusters and row counts match the fit.
  keep <- complete.cases(a[needed])
  for (variable in c(s$outcome, s$terms)) {
    keep <- keep & is.finite(a[[variable]])
  }
  a <- a[keep, ]
  rownames(a) <- NULL

  # For the main model this constructs:
  # log_publications ~ r1_post22 + factor(id) + factor(year).
  f <- reformulate(
    c(s$terms, "factor(id)", "factor(year)"),
    response = s$outcome
  )
  fit <- lm(f, data = a, na.action = na.fail)

  # Cluster by university, permitting dependence across its annual errors.
  # This changes inference; it does not correct endogeneity or an invalid dose.
  G <- length(unique(a$id))
  stopifnot(G > 1L)
  V <- sandwich::vcovCL(fit, cluster = a$id, type = "HC1")

  # Report only the named regressors, rather than all fixed-effect coefficients.
  b <- coef(fit)[s$terms]
  variances <- diag(V)[s$terms]
  stopifnot(all(is.finite(b)), all(is.finite(variances)), all(variances > 0))
  se <- sqrt(variances)

  # Conventional cluster-based t intervals use G - 1 reference degrees
  # of freedom. They are not an exact finite-sample guarantee.
  critical <- qt(0.975, df = G - 1)
  tab <- data.frame(
    model = s$label,
    outcome = s$outcome,
    term = s$terms,
    estimate = unname(b),
    cluster_se = unname(se),
    p = unname(2 * pt(-abs(b / se), df = G - 1)),
    lo = unname(b - critical * se),
    hi = unname(b + critical * se),
    n = nobs(fit),
    universities = G
  )

  table_rows[[s$label]] <- tab
  ans[[s$label]] <- list(fit = fit, covariance = V, table = tab)
}

tabs <- do.call(rbind, table_rows)
rownames(tabs) <- NULL
print(tabs)
write.csv(tabs, "results_exposure_estimates.csv", row.names = FALSE)
saveRDS(ans, "results_exposure_fits.rds")


# 4. ESTIMATE YEAR-SPECIFIC DOSE ASSOCIATIONS -------------------------------

# Replace the single dose-by-post term with dose-by-year interactions.
# Omitting dose*2021 normalizes every reported association relative to 2021.
# These are contrasts between year-specific dose slopes, not national year
# effects. The 2021 contrast is zero by construction, not an estimated zero.
#
# The year-specific post coefficients need not all equal the pooled post
# coefficient: the pooled model imposes additional restrictions on dynamics.
# Nonzero pre-period coefficients raise concerns about the exposure design;
# insignificant ones do not establish parallel counterfactual trends.

reference_year <- 2021L
years <- c(2018:2020, 2022:2024)
events <- list()
pretests <- list()
event_fits <- list()

for (exposure in c("R1", "Dependence")) {
  dose <- if (exposure == "R1") "dose_r1" else "dose_dependence"
  a <- long61

  keep <- complete.cases(a[c("id", "year", "log_publications", dose)]) &
    is.finite(a$log_publications) & is.finite(a[[dose]])
  a <- a[keep, ]
  rownames(a) <- NULL
  stopifnot(reference_year %in% a$year, all(years %in% a$year))

  # Make each interaction explicitly so the coefficient names are easy to
  # locate. A row's interaction is its dose in that year and zero otherwise.
  event_terms <- paste0("exposure_", years)
  for (j in seq_along(years)) {
    a[[event_terms[j]]] <- a[[dose]] * as.integer(a$year == years[j])
  }

  event_formula <- reformulate(
    c(event_terms, "factor(id)", "factor(year)"),
    response = "log_publications"
  )
  event_fit <- lm(event_formula, data = a, na.action = na.fail)
  G <- length(unique(a$id))
  V <- sandwich::vcovCL(event_fit, cluster = a$id, type = "HC1")

  b <- coef(event_fit)[event_terms]
  variances <- diag(V)[event_terms]
  stopifnot(all(is.finite(b)), all(is.finite(variances)), all(variances > 0))
  se <- sqrt(variances)
  critical <- qt(0.975, df = G - 1)

  event_table <- data.frame(
    exposure = exposure,
    year = years,
    reference_year = reference_year,
    term = event_terms,
    estimate = unname(b),
    cluster_se = unname(se),
    p = unname(2 * pt(-abs(b / se), df = G - 1)),
    lo = unname(b - critical * se),
    hi = unname(b + critical * se),
    n = nobs(event_fit),
    universities = G
  )
  events[[exposure]] <- event_table
  event_fits[[exposure]] <- list(fit = event_fit, covariance = V)


  # 5. JOINTLY TEST THE THREE PRE-PERIOD CONTRASTS ---------------------------

  # Test H0: the 2018, 2019 and 2020 dose slopes all equal the 2021 slope.
  # W = b' V^(-1) b is the clustered Wald statistic. Dividing by q and
  # comparing with F(q, G-1) gives the conventional approximate F test.
  # Use the SAME fitted sample and covariance as the event model.
  pre_terms <- paste0("exposure_", 2018:2020)
  pre_b <- coef(event_fit)[pre_terms]
  V_pre <- V[pre_terms, pre_terms, drop = FALSE]
  q <- length(pre_terms)

  if (qr(V_pre)$rank < q) {
    # Do not silently replace a singular covariance with a generalized inverse.
    # Retain the event estimates and flag this joint test as unavailable.
    warning("Singular pre-period covariance for ", exposure, "; joint test not reported.")
    F_stat <- NA_real_
    p_joint <- NA_real_
  } else {
    W <- as.numeric(crossprod(pre_b, solve(V_pre, pre_b)))
    F_stat <- W / q
    p_joint <- pf(F_stat, df1 = q, df2 = G - 1, lower.tail = FALSE)
  }

  pretests[[exposure]] <- data.frame(
    exposure = exposure,
    F = F_stat,
    df1 = q,
    df2 = G - 1,
    p = p_joint
  )
}

event_results <- do.call(rbind, events)
pretrend_results <- do.call(rbind, pretests)
rownames(event_results) <- NULL
rownames(pretrend_results) <- NULL

print(event_results)
print(pretrend_results)
write.csv(event_results, "results_event_diagnostics.csv", row.names = FALSE)
write.csv(pretrend_results, "results_pretrend_tests.csv", row.names = FALSE)
saveRDS(event_fits, "results_event_fits.rds")


# 6. EXPRESS THE MAIN COEFFICIENT IN ITS ORIGINAL DOSE UNIT ------------------

# The outcome is log publications. For one dose unit (+0.10 R1),
# 100 * (exp(beta) - 1) expresses the fitted contrast on a percentage scale.
# This describes a geometric-mean contrast, not automatically an arithmetic-
# mean publication effect. A causal interpretation requires further assumptions.
baseline <- subset(tabs, model == "RF61_2018_2024" & term == "r1_post22")
stopifnot(nrow(baseline) == 1L)

cat(sprintf(
  "\n+0.10 R1: %.2f%% associated change in geometric-mean publications; 95%% interval %.2f%% to %.2f%%.\n",
  100 * expm1(baseline$estimate),
  100 * expm1(baseline$lo),
  100 * expm1(baseline$hi)
))

# performance_share is a fraction, unlike the logged outcomes. Its coefficient
# times 100 is a percentage-point contrast; do not exponentiate that coefficient.
# Neither the intervals nor a pretrend p-value resolve endogeneity, spillovers,
# differential shocks or the fact that all Italian universities face the policy.
cat("The interval does not address endogeneity or causal identification.\n")
capture.output(sessionInfo(), file = "results_exposure_sessionInfo.txt")
