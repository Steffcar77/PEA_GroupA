# 02_frontiers.R
# Compare ordinary regression, pooled NHN frontiers and an NHN_Z model.
#
# Before running: run 01_prepare_data.R, or use the supplied prepared CSV.
# Keep scripts, data and generated results in the same workshop folder.
# Every estimated frontier below calls sfa::sfm() directly.
#
# These are descriptive research-production models. Neither a frontier fit
# nor an association with an observed VQR score identifies a policy effect.


# 1. LOAD PACKAGES AND DEFINE THE ESTIMATION SAMPLES ------------------------

library(sfa)
library(sandwich)
library(lmtest)

panel <- read.csv("teaching_panel_2020_2023.csv", check.names = FALSE)
panel <- panel[order(panel$id, panel$year), ]
rownames(panel) <- NULL

# Explicit year indicators leave 2020 as the reference year.
for (yr in 2021:2023) {
  panel[[paste0("y", yr)]] <- as.integer(panel$year == yr)
}

# Prepared inputs and outputs must be present for every observation.
# Do not silently fit different input-output samples across models.
stopifnot(
  !anyDuplicated(panel[c("id", "year")]),
  all(is.finite(panel$log_publications)),
  all(is.finite(panel$llabor)),
  all(is.finite(panel$lcurrent))
)

# A first cross section avoids the repeated-observation complication.
cs <- subset(panel, year == 2023)

# One university has no usable 2018 subject-assignment shares. Exclude it
# only for the models requiring those controls. Keep the unadjusted model
# on this SAME sample when isolating the effect of field adjustment.
fields <- c("medicine_share18", "engphys_share18", "ssh_share18")
field_panel <- panel[complete.cases(panel[fields]), ]
rownames(field_panel) <- NULL

cat("Full panel:", nrow(panel), "rows;",
    length(unique(panel$id)), "universities.\n")
cat("Field-complete panel:", nrow(field_panel), "rows;",
    length(unique(field_panel$id)), "universities.\n")


# 2. ESTIMATE A CONDITIONAL-MEAN REGRESSION ---------------------------------

# The slopes on logged inputs are conditional output elasticities.
# OLS estimates a conditional mean, not an attainable production frontier.
ols <- lm(
  log_publications ~ llabor + lcurrent + y2021 + y2022 + y2023,
  data = panel
)
summary(ols)

# Repeated observations from one university need not be independent.
# Cluster the OLS covariance by university and use G - 1 reference degrees
# of freedom, where G is the number of universities in this sample.
G <- length(unique(panel$id))
V_ols <- sandwich::vcovCL(ols, cluster = panel$id, type = "HC1")
ols_clustered <- lmtest::coeftest(ols, vcov. = V_ols, df = G - 1)
print(ols_clustered)

write.csv(
  as.data.frame(unclass(ols_clustered)),
  "results_ols_clustered_coefficients.csv",
  row.names = TRUE
)


# 3. FIT THE FIRST NHN MODELS DIRECTLY -------------------------------------

# A production frontier has a composed error v - u:
# v is symmetric noise and u >= 0 is the one-sided output shortfall.
# NHN specifies normal noise and half-normal inefficiency.
# inefdec = TRUE requests the production-frontier orientation.

# Model A: 2023 cross section, without field-composition controls.
f_cs <- sfa::sfm(
  log_publications ~ llabor + lcurrent,
  model_name = "NHN",
  data = cs,
  inefdec = TRUE,
  PSopt = FALSE,
  maxit.optim = 2000,
  trace = 0
)
summary(f_cs)

# Model B: pool all four years and include common year shifts.
# Pooling does not itself separate university heterogeneity from inefficiency.
f_raw <- sfa::sfm(
  log_publications ~ llabor + lcurrent + y2021 + y2022 + y2023,
  model_name = "NHN",
  data = panel,
  inefdec = TRUE,
  PSopt = FALSE,
  maxit.optim = 2000,
  trace = 0
)
summary(f_raw)

# Model C: the same unadjusted specification on the field-complete sample.
# Comparing B with C reveals the effect of changing the sample.
f_same <- sfa::sfm(
  log_publications ~ llabor + lcurrent + y2021 + y2022 + y2023,
  model_name = "NHN",
  data = field_panel,
  inefdec = TRUE,
  PSopt = FALSE,
  maxit.optim = 2000,
  trace = 0
)
summary(f_same)


# 4. ADD FROZEN FIELD-COMPOSITION CONTROLS ----------------------------------

# Model D uses the SAME observations as C and adds field shares.
# Comparing C with D therefore isolates specification rather than sample.
# The shares are frozen in 2018 and represent overlapping subject assignments.
f_field <- sfa::sfm(
  log_publications ~ llabor + lcurrent + medicine_share18 +
    engphys_share18 + ssh_share18 + y2021 + y2022 + y2023,
  model_name = "NHN",
  data = field_panel,
  inefdec = TRUE,
  PSopt = FALSE,
  maxit.optim = 2000,
  trace = 0
)

# If the optimiser did not report convergence, try an explicit restart.
# This is another direct package call, not a replacement estimator.
if (f_field$opt$convergence != 0L) {
  f_field <- sfa::sfm(
    log_publications ~ llabor + lcurrent + medicine_share18 +
      engphys_share18 + ssh_share18 + y2021 + y2022 + y2023,
    model_name = "NHN",
    data = field_panel,
    start_val = unname(coef(f_field)),
    inefdec = TRUE,
    PSopt = FALSE,
    maxit.optim = 2000,
    trace = 0
  )
}
summary(f_field)
# Your installed diagnostic helper may suggest keep_objective = TRUE.
# That argument is absent from your sfm() signature; do not add it.
sfa::sfa_diagnostics(f_field)

# Important: unadjusted fits can approach a zero-noise boundary and return
# code 52 or unavailable standard errors. Keep these as diagnostic examples;
# do not interpret their efficiency scores as a university league table.
# A converged adjusted fit still requires specification and numerical checks.


# 5. COMPARE PARAMETERS, EFFICIENCY PREDICTIONS AND NUMERICAL DIAGNOSTICS ------

# Lists collect the existing fits for reporting only. No estimator wrapper
# or additional attributes are attached to the package's returned objects.
fits <- list(
  NHN_2023 = f_cs,
  NHN_pooled61 = f_raw,
  NHN_unadjusted60 = f_same,
  NHN_fields60 = f_field
)
samples <- list(
  NHN_2023 = cs,
  NHN_pooled61 = panel,
  NHN_unadjusted60 = field_panel,
  NHN_fields60 = field_panel
)

summary_rows <- list()
coefficient_rows <- list()
efficiency_rows <- list()

for (nm in names(fits)) {
  fit <- fits[[nm]]
  dat <- samples[[nm]]
  cf <- coef(fit)

  # In the NHN output, lambda = sigma_u / sigma_v and
  # sigma = sqrt(sigma_u^2 + sigma_v^2). Recover the separate normal SDs.
  stopifnot(all(c("lambda", "sigma") %in% names(cf)))
  lambda <- unname(cf["lambda"])
  sigma <- unname(cf["sigma"])
  sv <- sigma / sqrt(1 + lambda^2)
  su <- lambda * sv

  # exp_u_hat is the package's predictor E[exp(-u) | composed residual].
  # It is not exp(-E[u | residual]); those are different predictors.
  # Verify the component and row count before attaching university-year IDs.
  if (is.null(fit$exp_u_hat)) {
    stop("No exp_u_hat component for ", nm, "; inspect names(fit).")
  }
  te <- as.numeric(fit$exp_u_hat)
  stopifnot(
    length(te) == nrow(dat),
    all(is.finite(te)),
    all(te >= 0 & te <= 1)
  )

  # opt$value stores the minimized negative log likelihood in these fits.
  # Negating it avoids assuming an available logLik() method.
  # Curvature and condition numbers supplement the convergence code.
  H <- fit$opt$hessian
  hvalues <- tryCatch(
    eigen(H, symmetric = TRUE, only.values = TRUE)$values,
    error = function(e) NA_real_
  )
  hcondition <- tryCatch(kappa(H, exact = TRUE), error = function(e) NA_real_)

  summary_rows[[nm]] <- data.frame(
    model = nm,
    n = nrow(dat),
    universities = length(unique(dat$id)),
    logLik = -as.numeric(fit$opt$value),
    convergence = fit$opt$convergence,
    sigma_v = sv,
    sigma_u = su,
    mean_te = mean(te),
    median_te = median(te),
    min_te = min(te),
    max_te = max(te),
    hessian_condition = hcondition,
    hessian_pd = if (anyNA(hvalues)) NA else all(hvalues > 0),
    boundary_noise = sv < 0.001
  )

  # The near-zero-noise threshold is a descriptive flag, not a formal test.
  # Package-summary SFA standard errors are model based. They should not be
  # presented as university-cluster standard errors. Only OLS is clustered
  # above; research SFA inference requires a supported covariance procedure.
  coefficient_rows[[nm]] <- data.frame(
    model = nm,
    term = names(cf),
    estimate = unname(cf)
  )
  efficiency_rows[[nm]] <- data.frame(
    model = nm,
    id = dat$id,
    year = dat$year,
    te = te
  )
}

frontier_comparison <- do.call(rbind, summary_rows)
print(frontier_comparison)

write.csv(frontier_comparison, "results_frontier_summary.csv", row.names = FALSE)
write.csv(
  do.call(rbind, coefficient_rows),
  "results_frontier_coefficients.csv", row.names = FALSE
)
write.csv(
  do.call(rbind, efficiency_rows),
  "results_descriptive_efficiency_scores.csv", row.names = FALSE
)
saveRDS(fits, "results_frontier_fits.rds")

if (any(frontier_comparison$convergence != 0L |
        frontier_comparison$boundary_noise)) {
  message("Some NHN fits have optimisation or boundary flags. Inspect results_frontier_summary.csv before interpreting them.")
}


# 6. PLACE THE VQR SCORE IN THE INEFFICIENCY-SCALE EQUATION ------------------

# The expression after | specifies the inefficiency-scale covariates.
# Under the default NHN_Z normal-scale link:
#   sigma_u(z) = exp(delta_0 + delta_1 * dose_r1).
# Thus positive delta_1 raises expected inefficiency, holding the frontier fixed.
# This models an association with an observed score, not an exogenous policy.
# The dose unit remains a 0.10 increase in R1, as defined in preparation.
stopifnot(all(is.finite(field_panel$dose_r1)))

f_z <- sfa::sfm(
  log_publications ~ llabor + lcurrent + medicine_share18 +
    engphys_share18 + ssh_share18 + y2021 + y2022 + y2023 | dose_r1,
  model_name = "NHN_Z",
  data = field_panel,
  inefdec = TRUE,
  PSopt = FALSE,
  maxit.optim = 2000,
  trace = 0
)

# The simulation showed why a restart can help with a line-search error.
# Keep the original point for comparison and restart only when code != 0.
f_z_initial <- f_z
if (f_z$opt$convergence != 0L) {
  f_z <- sfa::sfm(
    log_publications ~ llabor + lcurrent + medicine_share18 +
      engphys_share18 + ssh_share18 + y2021 + y2022 + y2023 | dose_r1,
    model_name = "NHN_Z",
    data = field_panel,
    start_val = unname(coef(f_z_initial)),
    inefdec = TRUE,
    PSopt = FALSE,
    maxit.optim = 2000,
    trace = 0
  )
}
summary(f_z)
sfa::sfa_diagnostics(f_z)

# Record whether the restart changed the attained objective and convergence.
# Code 0 alone does not establish a global maximum or valid causal inference.
z_optimization <- data.frame(
  stage = c("initial", "final"),
  logLik = c(-as.numeric(f_z_initial$opt$value), -as.numeric(f_z$opt$value)),
  convergence = c(f_z_initial$opt$convergence, f_z$opt$convergence)
)
print(z_optimization)
write.csv(z_optimization, "results_nhn_z_optimization.csv", row.names = FALSE)

if (f_z$opt$convergence != 0L) {
  warning("NHN_Z still reports non-convergence. Treat its saved outputs as provisional and inspect the diagnostics.")
}


# 7. CALCULATE AN EXPLICIT MODEL-BASED DERIVATIVE AND SAVE RESULTS ------------

# The final two NHN_Z coefficients are the scale intercept and dose slope.
# Write the derivative directly instead of assuming a marginal_effects() helper.
# For this half-normal model:
#   E[u | z] = sqrt(2/pi) * sigma_u(z)
#   d E[u | z] / d dose_r1 = sqrt(2/pi) * sigma_u(z) * delta_1.
# This is expected inefficiency conditional on z, not the residual-based TE
# predictor and not a causal treatment effect. A finite dose contrast is
# also different from an infinitesimal derivative under this nonlinear link.
Z <- model.matrix(~ dose_r1, data = field_panel)
delta_hat <- tail(coef(f_z), ncol(Z))
sigma_u_hat <- exp(drop(Z %*% delta_hat))
average_derivative <- mean(sqrt(2 / pi) * sigma_u_hat * delta_hat[2])

write.csv(
  data.frame(
    term = "dose_r1",
    average_derivative_expected_inefficiency = unname(average_derivative)
  ),
  "results_nhn_z_marginal_effects.csv", row.names = FALSE
)
write.csv(
  data.frame(term = names(coef(f_z)), estimate = unname(coef(f_z))),
  "results_nhn_z_coefficients.csv", row.names = FALSE
)
saveRDS(f_z, "results_nhn_z_fit.rds")
capture.output(summary(f_z), file = "results_nhn_z_summary.txt")
capture.output(sfa::sfa_diagnostics(f_z), file = "results_nhn_z_diagnostics.txt")
capture.output(sessionInfo(), file = "results_sessionInfo.txt")
