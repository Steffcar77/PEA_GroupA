# 11_nhn_start_sensitivity.R
# Group A, PEA project 4. Sensitivity of the NHN frontier of 06 (exposure in the
# frontier) to the starting values. Same formula, same sample, same options as 06;
# 06 itself is not modified. The only added option is keep_objective = TRUE,
# required by sandwich::vcovCL (handout recipe); it does not change the model.
#
# Starts (32):
#   A. grid: lambda {0.5, 1, 2, 5, 9.4} x sigma {0.1, 0.2, 0.36, 0.5}, OLS coefficients,
#      intercept raised by sigma_u * sqrt(2/pi), sigma_u = sigma * lambda / sqrt(1 + lambda^2);
#   B. 10 perturbations of the 06 solution, each parameter x (1 + U(-0.1, 0.1));
#   C. one start mapped from the NHN_Z coefficients of 06;
#   D. the 06 solution itself (reference).
# Path towards sigma_v -> 0: profile log-likelihood over a fixed lambda grid,
# maximised over the other parameters with our own NHN log-likelihood (checked
# against sfa at the 06 solution).
#
# A BOUNDARY solution (sigma_v < 0.001, the 06 threshold, or lambda > 100) is a
# DIAGNOSTIC, NOT A RESULT: it shows that the NHN with the exposure in the frontier
# is poorly identified in this sample. It must not be adopted as an estimate.
#
# Frontier coefficients of this NHN (e.g. faculty and cost elasticities) must never be
# cited. NHN_Z: two solutions are reported, the kit solution of 06 (logLik lower, not a
# maximum: cluster SE for comparison only) and our refit (higher likelihood: cite = TRUE).
#
# Run through run_all_g4.R (working directory = PEA_GroupA). Runtime: a few minutes.
# Outputs: output/tables/table_11_nhn_starts.csv, table_11_nhn_profile.csv,
#          table_11_sfa_cluster_se.csv, table_11_nhn_z_returns_to_scale.csv,
#          table_11_notes.csv; output/figures/fig_11_nhn_starts.png

library(sfa)
library(sandwich)

run_dir <- file.path("output", "template_run")
tab_dir <- file.path("output", "tables")
fig_dir <- file.path("output", "figures")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

a <- read.csv(file.path(run_dir, "results_group_04_sample.csv"), check.names = FALSE)
a <- a[order(a$id, a$year), ]
rownames(a) <- NULL
stopifnot(nrow(a) == 180L)
a$outcome <- a$log_publications
a$exposure <- a$lag_log_teaching_load
G <- length(unique(a$id))
fits_06 <- readRDS(file.path(run_dir, "results_group_04_sfa_fits.rds"))


# 1. SAME FORMULAS AS 06; CHECK THE ORDER OF THE PARAMETERS ---------------------

controls <- c("lag_log_faculty", "lag_lcurrent")
fields <- c("medicine_share18", "engphys_share18", "ssh_share18")
year_terms <- c("y2022", "y2023")
frontier_formula <- reformulate(c(controls, fields, year_terms, "exposure"), response = "outcome")
inefficiency_formula <- as.formula(paste("outcome ~", paste(c(controls, fields, year_terms),
                                                            collapse = " + "), "| exposure"))

b06 <- coef(fits_06$frontier)
beta_names <- c("(Intercept)", controls, fields, year_terms, "exposure")
stopifnot(identical(names(b06), c("lambda", "sigma", beta_names)))  # start_val order

ols <- lm(frontier_formula, data = a)
stopifnot(identical(names(coef(ols)), beta_names))
X <- model.matrix(ols)

sfa_options <- list(inefdec = TRUE, PSopt = FALSE, maxit.optim = 2000, trace = 0,
                    keep_objective = TRUE)

# Fit one model; record the first warning instead of printing every warning.
fit_quietly <- function(formula, model_name, start) {
  first_warning <- ""
  fit <- withCallingHandlers(
    tryCatch(do.call(sfa::sfm, c(list(formula, model_name = model_name, data = a,
                                      start_val = unname(start)), sfa_options)),
             error = function(e) e),
    warning = function(w) {
      if (first_warning == "") first_warning <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    })
  list(fit = fit, warning = first_warning)
}


# 2. STARTING VALUES -------------------------------------------------------------

starts <- list()
for (lambda in c(0.5, 1, 2, 5, 9.4)) {
  for (sigma in c(0.1, 0.2, 0.36, 0.5)) {
    sigma_u <- sigma * lambda / sqrt(1 + lambda^2)
    beta <- coef(ols)
    beta["(Intercept)"] <- beta["(Intercept)"] + sigma_u * sqrt(2 / pi)
    starts[[length(starts) + 1]] <- list(group = "A. Grid (OLS start)",
                                         value = c(lambda, sigma, beta))
  }
}
set.seed(20261122)
for (k in 1:10) {
  starts[[length(starts) + 1]] <- list(group = "B. Perturbed 06 solution",
                                       value = b06 * (1 + runif(length(b06), -0.1, 0.1)))
}
# NHN_Z of 06: sigma_v, frontier betas, then log sigma_u = d0 + d1 * exposure
# (default z_link "sd"). Map to NHN at the mean exposure; frontier exposure slope = 0.
cz <- coef(fits_06$inefficiency)
sv_z <- unname(cz[1])
su_z <- exp(unname(cz[10]) + unname(cz[11]) * mean(a$exposure))
starts[[length(starts) + 1]] <- list(group = "C. From NHN_Z",
                                     value = c(su_z / sv_z, sqrt(su_z^2 + sv_z^2),
                                               unname(cz[2:9]), 0))
starts[[length(starts) + 1]] <- list(group = "D. 06 solution", value = b06)


# 3. FIT FROM EVERY START --------------------------------------------------------

describe_fit <- function(result, start, k) {
  fit <- result$fit
  row <- data.frame(start_id = k, start_group = start$group,
                    start_lambda = start$value[1], start_sigma = start$value[2],
                    logLik = NA_real_, convergence = NA_integer_, lambda = NA_real_,
                    sigma = NA_real_, sigma_u = NA_real_, sigma_v = NA_real_,
                    exposure = NA_real_, min_hessian_eigenvalue = NA_real_,
                    hessian_pd = NA, wrong_skew = NA, boundary = NA, note = "",
                    first_warning = result$warning)
  if (inherits(fit, "error")) {
    row$note <- paste("fit failed:", conditionMessage(fit))
    return(row)
  }
  cf <- coef(fit)
  h <- tryCatch(eigen(fit$opt$hessian, symmetric = TRUE, only.values = TRUE)$values,
                error = function(e) NA_real_)
  row$logLik <- -as.numeric(fit$opt$value)
  row$convergence <- fit$opt$convergence
  row$lambda <- unname(cf["lambda"])
  row$sigma <- unname(cf["sigma"])
  row$sigma_u <- row$sigma * row$lambda / sqrt(1 + row$lambda^2)
  row$sigma_v <- row$sigma / sqrt(1 + row$lambda^2)
  row$exposure <- unname(cf["exposure"])
  row$min_hessian_eigenvalue <- if (anyNA(h)) NA_real_ else min(h)
  row$hessian_pd <- if (anyNA(h)) NA else all(h > 0)
  row$wrong_skew <- isTRUE(fit$wrong_skew)
  row$boundary <- row$sigma_v < 0.001 || row$lambda > 100
  if (row$boundary) row$note <- "boundary solution: diagnostic only, NOT adopted as a result"
  row
}

nhn_fits <- vector("list", length(starts))
rows <- list()
for (k in seq_along(starts)) {
  result <- fit_quietly(frontier_formula, "NHN", starts[[k]]$value)
  nhn_fits[[k]] <- result$fit
  rows[[k]] <- describe_fit(result, starts[[k]], k)
}
start_table <- do.call(rbind, rows)

# Reference rows: OLS (sigma_u = 0) and the NHN reported by 06.
reference <- start_table[c(1, 1), ]
reference[] <- NA
reference$start_group <- c("Reference: OLS (sigma_u = 0)", "Reference: NHN of 06")
reference$logLik <- c(as.numeric(logLik(ols)), -as.numeric(fits_06$frontier$opt$value))
reference$note <- c("", "as reported by 06")
start_table <- rbind(start_table, reference)
rownames(start_table) <- NULL

round_cols <- c("start_lambda", "start_sigma", "logLik", "lambda", "sigma", "sigma_u",
                "sigma_v", "exposure", "min_hessian_eigenvalue")
printed <- start_table
printed[round_cols] <- lapply(printed[round_cols], signif, digits = 5)
write.csv(printed, file.path(tab_dir, "table_11_nhn_starts.csv"), row.names = FALSE)
print(printed[, c("start_id", "start_group", "logLik", "convergence", "lambda",
                  "sigma_v", "exposure", "hessian_pd", "boundary")])


# 4. PATH TOWARDS sigma_v -> 0: PROFILE LOG-LIKELIHOOD OVER lambda -------------

# NHN production frontier: y = x'b + v - u, eps = y - x'b,
# log f(eps) = log 2 - log sigma + log phi(eps / sigma) + log Phi(-lambda * eps / sigma).
nhn_loglik <- function(beta, sigma, lambda) {
  e <- as.vector(a$outcome - X %*% beta)
  sum(log(2) - log(sigma) + dnorm(e / sigma, log = TRUE) +
        pnorm(-lambda * e / sigma, log.p = TRUE))
}
own_06 <- nhn_loglik(b06[beta_names], b06["sigma"], b06["lambda"])
if (!isTRUE(all.equal(own_06, -as.numeric(fits_06$frontier$opt$value), tolerance = 1e-6))) {
  stop("Own NHN log-likelihood (", own_06, ") does not match sfa at the 06 solution.")
}
cat("Check passed: own NHN log-likelihood equals sfa at the 06 solution.\n")

# For each lambda, maximise over (beta, log sigma) from three starts and keep the best:
# the previous optimum (warm start), the OLS start with raised intercept, and the
# best sfa fit over all starts. A single warm start can stall on this flat surface.
ok <- which(!is.na(start_table$logLik[seq_along(starts)]))
best <- ok[which.max(start_table$logLik[ok])]
best_coef <- coef(nhn_fits[[best]])
best_par <- c(best_coef[beta_names], log(best_coef[["sigma"]]))
ols_par <- c(coef(ols), log(sd(resid(ols))))

lambda_grid <- c(1, 2, 5, 9.37, 20, 50, 100, 300, 1000, 1e4, 1e5)
current <- c(b06[beta_names], log(b06[["sigma"]]))
profile_rows <- list()
for (lambda in lambda_grid) {
  objective <- function(p) -nhn_loglik(p[seq_along(beta_names)], exp(p[length(p)]), lambda)
  ols_raised <- ols_par
  ols_raised[1] <- ols_raised[1] + exp(ols_par[length(ols_par)]) * sqrt(2 / pi)
  # BFGS first, then nlminb (PORT) to refine: near the boundary the surface is steep.
  candidates <- lapply(list(current, ols_raised, best_par), function(p0) {
    o1 <- optim(p0, objective, method = "BFGS", control = list(maxit = 10000, reltol = 1e-12))
    o2 <- nlminb(o1$par, objective, control = list(eval.max = 1e5, iter.max = 1e5))
    if (o2$objective < o1$value) list(par = o2$par, value = o2$objective,
                                      convergence = o2$convergence) else o1
  })
  opt <- candidates[[which.min(vapply(candidates, function(o) o$value, numeric(1)))]]
  current <- opt$par
  sigma <- exp(opt$par[length(opt$par)])
  profile_rows[[as.character(lambda)]] <- data.frame(
    lambda = lambda, profile_logLik = -opt$value, convergence = opt$convergence,
    sigma = sigma, sigma_u = sigma * lambda / sqrt(1 + lambda^2),
    sigma_v = sigma / sqrt(1 + lambda^2), exposure = opt$par[length(beta_names)]
  )
}
profile <- do.call(rbind, profile_rows)
rownames(profile) <- NULL
profile$note <- ifelse(profile$sigma_v < 0.001 | profile$lambda > 100,
                       "towards boundary: diagnostic only", "")
print(signif(profile[-ncol(profile)], 5))
write.csv(profile, file.path(tab_dir, "table_11_nhn_profile.csv"), row.names = FALSE)


# 5. UNIVERSITY-CLUSTERED STANDARD ERRORS (vcovCL HC1, df = G - 1) --------------

cluster_table <- function(fit, label, extra_note) {
  first_warning <- ""
  V <- withCallingHandlers(
    tryCatch(sandwich::vcovCL(fit, cluster = a$id, type = "HC1"),
             error = function(e) NULL),
    warning = function(w) {
      if (first_warning == "") first_warning <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    })
  cf <- coef(fit)
  se_cluster <- if (is.null(V)) rep(NA_real_, length(cf)) else sqrt(diag(V))
  available <- all(is.finite(se_cluster))
  t_value <- cf / se_cluster
  data.frame(model = label, term = names(cf), estimate = unname(cf),
             model_se = unname(fit$std.errors), cluster_se = unname(se_cluster),
             t = unname(t_value), p = unname(2 * pt(-abs(t_value), df = G - 1)),
             logLik = -as.numeric(fit$opt$value),
             note = if (available) extra_note else
               paste("cluster SE not available:", first_warning, extra_note))
}

best_note <- if (isTRUE(start_table$boundary[best])) {
  "best NHN is a boundary solution: diagnostic only, do not cite (NOT adopted as a result)"
} else "best NHN over all starts: diagnostic only, do not cite"

# NHN_Z: two solutions.
#   refit: sfm from the 06 coefficients with keep_objective = TRUE (higher likelihood);
#   kit:   the 06 solution as stored (lower likelihood: 06 stopped before the maximum).
# The kit fit does not keep its likelihood, so its scores are computed with the stored
# likelihood of the refit evaluated at the kit parameters, and the kit Hessian is used
# as bread. Check: that likelihood at the kit parameters equals the kit log-likelihood.
z_kit_fit <- fits_06$inefficiency
z_refit <- fit_quietly(inefficiency_formula, "NHN_Z", coef(z_kit_fit))$fit
stopifnot(!inherits(z_refit, "error"))
z_kit <- z_refit
z_kit$opt$par <- z_kit_fit$opt$par
z_kit$coefficients <- coef(z_kit_fit)
z_kit$std.errors <- z_kit_fit$std.errors
z_kit$opt$hessian <- z_kit_fit$opt$hessian
z_kit$opt$value <- z_kit_fit$opt$value
stopifnot(identical(names(coef(z_kit_fit)), names(coef(z_refit))),
          isTRUE(all.equal(unname(z_kit_fit$opt$par), unname(coef(z_kit_fit)))),
          isTRUE(all.equal(unname(z_refit$opt$par), unname(coef(z_refit)))))
kit_loglik_check <- sum(z_refit$objective(z_kit_fit$opt$par, per_obs = TRUE))
if (!isTRUE(all.equal(kit_loglik_check, -as.numeric(z_kit_fit$opt$value), tolerance = 1e-8))) {
  stop("The refit likelihood at the kit parameters (", kit_loglik_check,
       ") does not equal the kit log-likelihood.")
}
max_score_kit <- max(abs(colSums(sandwich::estfun(z_kit))))
max_score_refit <- max(abs(colSums(sandwich::estfun(z_refit))))
ll_kit <- -as.numeric(z_kit_fit$opt$value)
ll_refit <- -as.numeric(z_refit$opt$value)
cite_refit <- ll_refit >= ll_kit

z_note_refit <- sprintf(paste("NHN_Z refitted from the 06 coefficients (keep_objective = TRUE);",
                              "logLik %.3f vs %.3f of the kit solution; max |sum of scores| = %.3f."),
                        ll_refit, ll_kit, max_score_refit)
z_note_kit <- sprintf(paste("NHN_Z kit solution of 06 (convergence code %d, but below the maximum);",
                            "cluster SE for comparison only: scores evaluated at the kit parameters",
                            "do not sum to zero (max |sum| = %.2f) because this is not a maximum, so",
                            "the sandwich SE has no inferential justification here."),
                      z_kit_fit$opt$convergence, max_score_kit)

se_table <- rbind(cluster_table(nhn_fits[[best]], "NHN (best start)", best_note),
                  cluster_table(z_refit, "NHN_Z (refit)", z_note_refit),
                  cluster_table(z_kit, "NHN_Z (kit solution of 06)", z_note_kit))
# cite = TRUE only for the NHN_Z refit, the solution with the higher likelihood. Never for
# the NHN rows (boundary, not identified) nor for the kit point (not a maximum).
if (!cite_refit) warning("The NHN_Z refit does not improve on the kit likelihood: no row is cited.")
se_table$cite <- se_table$model == "NHN_Z (refit)" & cite_refit
num <- c("estimate", "model_se", "cluster_se", "t", "p", "logLik")
se_table[num] <- lapply(se_table[num], signif, digits = 5)
print(se_table[se_table$term == "exposure", c("model", "estimate", "cluster_se", "p", "logLik", "cite")])
write.csv(se_table, file.path(tab_dir, "table_11_sfa_cluster_se.csv"), row.names = FALSE)

# Returns to scale in the NHN_Z frontier: sum of the faculty and cost elasticities.
# SE of the sum from the cluster covariance V: sqrt(V_ff + V_cc + 2 V_fc).
rts_row <- function(fit, label) {
  V <- sandwich::vcovCL(fit, cluster = a$id, type = "HC1")
  j <- match(controls, names(coef(fit)))
  stopifnot(!anyNA(j), length(unique(j)) == 2L)
  est <- unname(coef(fit)[j])
  se <- sqrt(diag(V)[j])
  sum_se <- sqrt(V[j[1], j[1]] + V[j[2], j[2]] + 2 * V[j[1], j[2]])
  data.frame(model = label, faculty_elasticity = est[1], faculty_cluster_se = se[1],
             cost_elasticity = est[2], cost_cluster_se = se[2], sum_elasticities = sum(est),
             sum_cluster_se = sum_se, t_sum_equals_1 = (sum(est) - 1) / sum_se,
             p_sum_equals_1 = 2 * pt(-abs((sum(est) - 1) / sum_se), df = G - 1))
}
rts_table <- rbind(rts_row(z_refit, "NHN_Z (refit)"), rts_row(z_kit, "NHN_Z (kit solution of 06)"))
rts_table$cite <- rts_table$model == "NHN_Z (refit)" & cite_refit
rts_table$note <- c(
  "Frontier coefficients of lag_log_faculty and lag_lcurrent; SE of the sum = sqrt(V_ff + V_cc + 2 V_fc) from the university-clustered covariance (vcovCL HC1, df = G - 1). Pooled maintained specification without university effects. Labour input = faculty only (as in 06), while the main DEA frontier of g4/12 uses research staff (faculty + fellows + PhD students): consistency with constant returns here is an indication from a different specification, not a confirmation of the returns-to-scale test of that frontier.",
  "For comparison only: kit point is not a maximum (scores do not sum to zero); its SEs have no inferential justification.")
rts_table[2:9] <- lapply(rts_table[2:9], signif, digits = 5)
print(rts_table[, 1:8])
write.csv(rts_table, file.path(tab_dir, "table_11_nhn_z_returns_to_scale.csv"), row.names = FALSE)

# Why the NHN with the exposure in the frontier is not identified (diagnostics only).
e_ols <- resid(ols)
skew_ols <- mean((e_ols - mean(e_ols))^3) / mean((e_ols - mean(e_ols))^2)^1.5
n_starts <- length(starts)
n_boundary <- sum(start_table$boundary[seq_len(n_starts)], na.rm = TRUE)
profile_monotone <- all(diff(profile$profile_logLik) > 0)
best_min_eigen <- start_table$min_hessian_eigenvalue[best]
z_exposure <- se_table[se_table$term == "exposure" & grepl("^NHN_Z", se_table$model), ]
notes_11 <- data.frame(topic = c("NHN identification", "NHN identification", "NHN identification",
                                 "NHN identification", "NHN elasticities", "NHN_Z solutions",
                                 "NHN_Z kit solution", "NHN_Z returns to scale"),
  note = c(
    sprintf("Skewness of the OLS residuals of the same frontier specification: %+.3f (positive: the 'wrong' sign for a production frontier, where the composed error should be negatively skewed).", skew_ols),
    sprintf("Profile log-likelihood over lambda is %s from lambda = %g (%.2f) to lambda = %g (%.2f): it rises towards sigma_v -> 0 (deterministic frontier).",
            if (profile_monotone) "monotonically increasing" else "not monotone",
            min(profile$lambda), profile$profile_logLik[1], max(profile$lambda),
            profile$profile_logLik[nrow(profile)]),
    sprintf("%d of %d starting values end at the boundary (sigma_v < 0.001 or lambda > 100).", n_boundary, n_starts),
    sprintf("At the best (boundary) solution the information matrix is numerically singular: the minimum Hessian eigenvalue is about zero (absolute value %.0e), so standard errors cannot be computed. Diagnostic only; no estimate is adopted.", abs(best_min_eigen)),
    "Frontier coefficients of the NHN with the exposure in the frontier (e.g. faculty and cost elasticities) come from a non-identified model: diagnostic only, do not cite. Use NHN_Z (table_11_nhn_z_returns_to_scale.csv) or OLS column (6) of g4/10.",
    sprintf("The two NHN_Z log-likelihoods are almost equal (refit %.3f, kit %.3f) while delta1 differs (%.3f vs %.3f): the likelihood is flat in the direction of the exposure, so delta1 is poorly determined and not significant in either solution (cluster p = %.2f and %.2f). Cite the refit (higher likelihood).",
            ll_refit, ll_kit, z_exposure$estimate[1], z_exposure$estimate[2], z_exposure$p[1], z_exposure$p[2]),
    sprintf("Kit cluster SEs are for comparison only: at a point that is not a maximum the scores do not sum to zero (max |sum| = %.2f, refit %.3f), so the sandwich has no inferential justification.",
            max_score_kit, max_score_refit),
    sprintf(paste("NHN_Z (refit) elasticities: faculty %.3f, costs %.3f, sum %.3f (cluster SE %.3f), consistent with",
                  "constant returns to scale. They are estimated with faculty only as the labour input (specification of 06),",
                  "while the main DEA frontier of g4/12 uses research staff (faculty + fellows + PhD students): this is an",
                  "indication from a different specification, not a confirmation of the returns-to-scale test of that frontier."),
            rts_table$faculty_elasticity[1], rts_table$cost_elasticity[1], rts_table$sum_elasticities[1],
            rts_table$sum_cluster_se[1])
  ))
print(notes_11)
write.csv(notes_11, file.path(tab_dir, "table_11_notes.csv"), row.names = FALSE)


# 6. FIGURE: FINAL logLik AGAINST FINAL log10(lambda) ---------------------------

# Readable without colour: one symbol per start group, profile path as a line,
# reference levels as labelled dashed lines.
groups <- c("A. Grid (OLS start)", "B. Perturbed 06 solution", "C. From NHN_Z", "D. 06 solution")
symbols <- c(1, 2, 15, 8)
s <- start_table[seq_along(starts), ]
s <- s[is.finite(s$logLik) & is.finite(s$lambda) & s$lambda > 0, ]
x_all <- c(log10(s$lambda), log10(profile$lambda))
y_ref <- c(as.numeric(logLik(ols)), -as.numeric(fits_06$frontier$opt$value))
png(file.path(fig_dir, "fig_11_nhn_starts.png"), width = 1500, height = 1100, res = 150)
par(mar = c(4.5, 4.5, 3, 1))
plot(NA, xlim = range(x_all), ylim = range(c(s$logLik, profile$profile_logLik, y_ref)),
     xlab = "log10(lambda) at the solution (large lambda = sigma_v close to 0)",
     ylab = "Log-likelihood", main = "NHN with exposure in the frontier: starting values")
lines(log10(profile$lambda), profile$profile_logLik, lty = 1)
points(log10(profile$lambda), profile$profile_logLik, pch = 20, cex = 0.8)
abline(h = y_ref, lty = 2)
text(min(x_all), y_ref[1], "OLS (sigma_u = 0)", adj = c(0, -0.4), cex = 0.8)
text(max(x_all), y_ref[2], "NHN reported by 06", adj = c(1, -0.4), cex = 0.8)
points(log10(s$lambda), s$logLik, pch = symbols[match(s$start_group, groups)], cex = 1.2)
legend("bottomright", legend = c(groups, "Profile log-likelihood (path)"),
       pch = c(symbols, 20), lty = c(NA, NA, NA, NA, 1), bty = "n", cex = 0.85)
dev.off()

cat("11 completed: start, profile and cluster-SE tables and fig_11_nhn_starts.png written.\n")
