# 04_panel_extension.R
# Advanced extension: university heterogeneity in a panel frontier.
#
# Input: teaching_panel_2020_2023.csv, in the same folder as this script.
# Outputs: files in that folder with a results_cre_tre_ prefix.
# This script can run independently of 02 and 03 once the CSV exists.
#
# The panel contains only four years. This is a descriptive sensitivity
# exercise, not an identified estimate of the effect of VQR.


# 1. READ THE PANEL AND CHECK UNIVERSITY-YEAR KEYS ---------------------------

library(sfa)

panel <- read.csv("teaching_panel_2020_2023.csv", check.names = FALSE)
stopifnot(!anyDuplicated(panel[c("id", "year")]))

# Explicit year indicators leave 2020 as the reference category.
for (yr in 2021:2023) {
  panel[[paste0("y", yr)]] <- as.integer(panel$year == yr)
}

# Use the field-complete sample, as in the adjusted models in 02.
# Do not replace undefined subject-assignment shares with zero.
fields <- c("medicine_share18", "engphys_share18", "ssh_share18")
field_panel <- panel[complete.cases(panel[fields]), ]

# Your psfm() interface has individual but no time argument. Establish
# university groups and chronological order explicitly before estimation.
# Merely sorting does not add a time trend; the year indicators do that job.
field_panel <- field_panel[order(field_panel$id, field_panel$year), ]
rownames(field_panel) <- NULL

needed <- c("id", "year", "log_publications", "llabor", "lcurrent", fields)
stopifnot(
  !anyNA(field_panel[needed]),
  setequal(unique(field_panel$year), 2020:2023),
  all(table(field_panel$id) == 4L)
)
for (variable in c("log_publications", "llabor", "lcurrent", fields)) {
  stopifnot(all(is.finite(field_panel[[variable]])))
}

cat("Panel sample:", nrow(field_panel), "rows;",
    length(unique(field_panel$id)), "universities; four years.\n")


# 2. CONSTRUCT THE MUNDLAK INPUT MEANS EXPLICITLY ----------------------------

# The true random-effects model separates a persistent, two-sided university
# effect from the time-varying one-sided inefficiency component:
#
# log(output_it) = x_it' beta + alpha_i + v_it - u_it.
#
# A correlated random-effects (Mundlak) extension writes alpha_i as a linear
# function of university input means plus a residual random effect.
# Including those means relaxes the assumption that the university effect
# is independent of the whole input history, under this maintained form.
# It does not remove every source of input endogeneity.
#
# ave() calculates each university's four-year mean and places it on all
# four rows. The means use the estimation sample, not a different panel.
field_panel$mean_llabor <- ave(
  field_panel$llabor, field_panel$id, FUN = mean
)
field_panel$mean_lcurrent <- ave(
  field_panel$lcurrent, field_panel$id, FUN = mean
)

# Frozen field shares and common year indicators also enter the frontier.
# Means of the year indicators would be constants in this balanced panel,
# so adding them would only duplicate the intercept.
#
# These input means use 2020-2023, including post-2022 observations. They
# serve this descriptive panel decomposition; they are not pre-policy
# controls for identifying a total causal effect of funding.


# 3. PREPARE EXPLICIT NUMERICAL STARTING VALUES ------------------------------

# The full design has both time-varying inputs and their university means.
# In a preliminary between-university regression, however, an input is
# replaced by its mean and duplicates the explicit mean column. Year
# indicators also have identical university means in this balanced panel.
# Some package builds cannot initialize this specification automatically.
# Supply numeric starts to bypass that between-regression initialization.
# These are starting guesses, not reported estimates or fixed parameters.

cre_formula <- log_publications ~ llabor + lcurrent + medicine_share18 +
  engphys_share18 + ssh_share18 + y2021 + y2022 + y2023 +
  mean_llabor + mean_lcurrent

initial_ols <- lm(cre_formula, data = field_panel, na.action = na.fail)
X <- model.matrix(initial_ols)
stopifnot(initial_ols$rank == ncol(X), all(is.finite(coef(initial_ols))))

# Split OLS residuals into a university average and a deviation from it.
# Use their dispersions to choose positive initial scales. These simple
# quantities are not an estimated stochastic-frontier variance decomposition.
e0 <- residuals(initial_ols)
university_mean_e <- ave(e0, field_panel$id, FUN = mean)
within_sd <- sqrt(mean((e0 - university_mean_e)^2))
university_means_e <- tapply(e0, field_panel$id, mean)

# Start with equal normal SDs for v and the underlying half-normal u.
# Var(v-u) = sigma_v^2 + (1-2/pi)*sigma_u^2 when independent.
# A small positive floor prevents a zero starting scale.
sv_start <- max(within_sd / sqrt(1 + (1 - 2/pi)), 0.01)
su_start <- sv_start
sr_start <- max(sd(university_means_e), 0.01)

# OLS estimates the conditional mean. Raise its intercept by the starting
# E[u] to obtain an initial frontier intercept under the production model.
beta_start <- coef(initial_ols)
beta_start[1] <- beta_start[1] + su_start * sqrt(2/pi)

# TRE expects: lambda, sigma, random-effect SD, then frontier coefficients
# in formula order. lambda = sigma_u/sigma_v and sigma^2 = sigma_v^2+sigma_u^2.
start_tre <- c(
  su_start / sv_start,
  sqrt(su_start^2 + sv_start^2),
  sr_start,
  unname(beta_start)
)
write.csv(
  data.frame(
    term = c("lambda", "sigma", "sigr", names(beta_start)),
    start_value = start_tre
  ),
  "results_cre_tre_start_values.csv", row.names = FALSE
)


# 4. ESTIMATE THE PANEL FRONTIER WITH A DIRECT psfm() CALL -------------------

# TRE separates transient one-sided inefficiency from persistent symmetric
# university heterogeneity. It does not identify persistent inefficiency.
# Halton draws approximate the random-effect integral. rand.gtre fixes the
# draw sequence. Check more draws before using substantive estimates.
# This fit can take substantially longer than the pooled models in 02.
f_cre <- sfa::psfm(
  cre_formula,
  model_name = "TRE",
  data = field_panel,
  individual = "id",
  start_val = start_tre,
  halton_num = 200,
  rand.gtre = 20261019,
  PSopt = FALSE,
  maxit.bobyqa = 5000,
  maxit.optim = 1000,
  inefdec = TRUE,
  trace = 0
)
summary(f_cre)

# The coefficients on llabor and lcurrent describe changes holding the
# university means fixed. The coefficients on the means capture additional
# between-university associations in the correlated-effects specification.
# Neither set automatically measures a causal resource effect.


# 5. SAVE THE ESTIMATES AND EXAMINE NUMERICAL CURVATURE ----------------------

# Save the complete object before interpreting the output. A convergence
# code alone does not establish a well-identified variance decomposition.
saveRDS(f_cre, "results_cre_tre_fit.rds")
capture.output(summary(f_cre), file = "results_cre_tre_summary.txt")
write.csv(
  data.frame(term = names(coef(f_cre)), estimate = unname(coef(f_cre))),
  "results_cre_tre_coefficients.csv", row.names = FALSE
)

# The optimiser stores the minimized negative log likelihood in opt$value.
# Negating it avoids assuming that a logLik() method is available.
# A positive-definite Hessian is useful evidence about local curvature,
# but a very large condition number signals directions with unequal curvature
# and depends on the units and parameterisation.
H <- f_cre$opt$hessian
h <- tryCatch(
  eigen(H, symmetric = TRUE, only.values = TRUE)$values,
  error = function(e) NA_real_
)
h_condition <- tryCatch(kappa(H, exact = TRUE), error = function(e) NA_real_)

# TRE reports lambda, sigma and sigr. Recover the two transient normal SDs;
# sigr is the SD of the residual symmetric university effect, after the means.
cf <- coef(f_cre)
stopifnot(all(c("lambda", "sigma", "sigr") %in% names(cf)))
sigma_v_hat <- unname(cf["sigma"] / sqrt(1 + cf["lambda"]^2))
sigma_u_hat <- unname(cf["lambda"] * sigma_v_hat)
sigma_r_hat <- unname(cf["sigr"])

fit_diagnostics <- data.frame(
  n = nrow(field_panel),
  universities = length(unique(field_panel$id)),
  years_per_university = 4L,
  halton_draws = 200L,
  convergence = f_cre$opt$convergence,
  logLik = -as.numeric(f_cre$opt$value),
  sigma_v = sigma_v_hat,
  sigma_u = sigma_u_hat,
  sigma_random_intercept = sigma_r_hat,
  min_hessian_eigenvalue = if (anyNA(h)) NA_real_ else min(h),
  max_hessian_eigenvalue = if (anyNA(h)) NA_real_ else max(h),
  hessian_condition = h_condition,
  hessian_pd = if (anyNA(h)) NA else all(h > 0)
)
print(fit_diagnostics)
write.csv(fit_diagnostics, "results_cre_tre_diagnostics.csv", row.names = FALSE)

# Inspect the reported scale parameters for boundary solutions as well.
# In the TRE parameterisation lambda = sigma_u / sigma_v. A tiny lambda
# may leave little transient inefficiency to distinguish from symmetric noise.
# A near-zero random-effect SD would instead indicate a different boundary.
# Do not force convergence by repeatedly increasing iteration limits.
if (f_cre$opt$convergence != 0L) {
  warning("The panel fit did not report convergence. Inspect the saved summary and diagnostics before interpreting it.")
}
if (!isTRUE(fit_diagnostics$hessian_pd)) {
  warning("The Hessian is not verified positive definite. Treat model-based inference as provisional.")
}


# 6. LIMITS AND FOLLOW-UP CHECKS FOR THE RESEARCH APPLICATION ----------------

# Four observations per university offer limited information for separating
# persistent heterogeneity, transient noise and transient inefficiency.
# The decomposition rests on distributional and dependence assumptions.
# A boundary estimate is not proof that universities have no inefficiency.
#
# For a substantive analysis:
# - Check alternative starts, even when the optimiser reports code 0.
# - Increase halton_num (for example to 500, then 1000) and check whether
#   coefficients, variance components and the attained likelihood stabilize.
# - Examine draw-sequence sensitivity and input/mean collinearity.
# - Investigate serial dependence beyond the model's maintained structure.
#
# Standard errors printed by summary() are model based. They are not a
# university-cluster bootstrap. Do not apply a pooled-model sandwich formula
# without checking that suitable panel score contributions are available.
# A university-block bootstrap resamples entire four-year trajectories and
# re-estimates the model. Repeatedly selected universities need new bootstrap
# group IDs, so separate copies are not treated as one enlarged panel group.
#
# Adding university fixed effects to this short nonlinear frontier is not
# a routine repair; it introduces a different incidental-parameter problem.
capture.output(sessionInfo(), file = "results_cre_tre_sessionInfo.txt")
cat("Panel fit saved. Inspect numerical diagnostics before interpretation.\n")
