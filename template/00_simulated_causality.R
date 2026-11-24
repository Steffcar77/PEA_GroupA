# Simulated outcomes and randomly assigned support.
# These are not estimates of an Italian policy effect.

library(sfa)

set.seed(20261023)

n <- 2000L

toy <- data.frame(
  x = rnorm(n),
  support = rbinom(n, 1, 0.5)
)

v <- rnorm(n, sd = 0.15)
u0 <- abs(rnorm(n, sd = 0.45))


# EXAMPLE A: support shifts the frontier.
# The frontier rises by 0.12 log points.
# The inefficiency distribution is unchanged.

toy$y_frontier <- 1 + 0.7 * toy$x +
  0.12 * toy$support + v - u0

ols_frontier <- lm(
  y_frontier ~ x + support,
  data = toy
)

fit_frontier <- sfa::sfm(
  y_frontier ~ x + support,
  model_name = "NHN",
  data = toy,
  inefdec = TRUE,
  PSopt = FALSE,
  trace = 0
)

print(summary(ols_frontier))
print(summary(fit_frontier))
print(coef(fit_frontier))

cat(
  "\nKnown frontier shift: 0.12 log points, or",
  round(100 * expm1(0.12), 2),
  "percent.\n"
)


# EXAMPLE B: support reduces inefficiency.
# The frontier is unchanged.
#
# Support multiplies inefficiency by exp(-0.4).
# Thus the underlying normal scale changes from
# 0.45 to 0.45 * exp(-0.4).

delta_scale <- -0.4

toy$y_efficiency <- 1 + 0.7 * toy$x + v -
  u0 * exp(delta_scale * toy$support)

ols_efficiency <- lm(
  y_efficiency ~ x + support,
  data = toy
)

fit_efficiency <- sfa::sfm(
  y_efficiency ~ x | support,
  model_name = "NHN_Z",
  data = toy,
  inefdec = TRUE,
  PSopt = FALSE,
  trace = 0
)

# Restart from the first fit to check numerical convergence.
fit_efficiency <- sfa::sfm(
  y_efficiency ~ x | support,
  model_name = "NHN_Z",
  data = toy,
  start_val = unname(coef(fit_efficiency)),
  inefdec = TRUE,
  PSopt = FALSE,
  maxit.optim = 2000,
  trace = 0
)

summary(fit_efficiency)

print(summary(ols_efficiency))
print(summary(fit_efficiency))
print(coef(fit_efficiency))

# E[u0] = 0.45 * sqrt(2/pi).
# The treatment effect on expected log output is
# the reduction in expected inefficiency.

known_log_gain <- 0.45 * sqrt(2 / pi) *
  (1 - exp(delta_scale))

cat(
  "\nKnown change in expected log output:",
  round(known_log_gain, 4),
  "\n"
)

cat(
  "OLS estimate of that change:",
  round(coef(ols_efficiency)[["support"]], 4),
  "\n"
)


# Save results in the working folder.

truth <- data.frame(
  example = c("A_frontier", "B_inefficiency"),
  known_frontier_shift = c(0.12, 0),
  known_log_scale_change = c(0, delta_scale),
  known_mean_log_output_change = c(0.12, known_log_gain)
)

write.csv(
  truth,
  "results_simulation_truth.csv",
  row.names = FALSE
)

saveRDS(
  list(
    ols_frontier = ols_frontier,
    fit_frontier = fit_frontier,
    ols_efficiency = ols_efficiency,
    fit_efficiency = fit_efficiency,
    truth = truth
  ),
  "results_simulation_fits.rds"
)