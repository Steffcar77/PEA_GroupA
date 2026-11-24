# 12_research_frontier.R
# Group A, PEA project 4, Block 2 (common trunk). "Research-only" frontier with FEAR.
#
# Pooled frontier 2021-2023: 61 universities, 183 university-years.
#   inputs  (t-1): faculty; current-management costs deflated to 2020 euros (ISTAT NIC)
#   output  (t)  : Scopus publications
# Unlike 06, Siena Stranieri is included: the 2018 field shares are not needed here.
#
# FEAR conventions: variables by rows (t()), ORIENTATION = 2 (output), METRIC = 2 (Farrell:
# 1 = efficient, 1.25 = output could rise by 25% with the same inputs).
#
# Estimator rule fixed before seeing the results (5% level):
#   convexity not rejected -> DEA (RTS = 3 if CRS not rejected, otherwise RTS = 1);
#   convexity rejected     -> FDH.
# Tests: test.convexity.fast / test.rts.fast with NSPLIT = 1000 (FEAR chapter: at least
# 1000 sample splits are needed to avoid ambiguity from the random splitting). These fast
# tests return reject (1) / not reject (0) indicators at the 10%, 5% and 1% levels: they are
# based on the Markov inequality and do not produce p-values.
#
# Confidence intervals: dea.subsample / fdh.subsample with subsample.sizes = NULL, i.e. the
# subsample size m is chosen for each observation by the Simar and Wilson (2011a) method,
# as recommended by the FEAR chapter (a non-optimised m gives poor coverage).
#
# LIMITS (also written to table_12_notes.csv):
# - the pooled frontier uses each university three times: observations are not independent,
#   while the tests and the subsampling treat them as independent draws;
# - one technology is assumed for 2021-2023;
# - Scopus indexing may be incomplete for recent years: a fall in 2023 publications can make
#   2023 look less efficient (the SFA year effects of 06 are about -0.05 in 2022 and -0.11 in
#   2023); the year-by-year frontier in g4/13 is the check.
#
# Run through run_all_g4.R (working directory = PEA_GroupA). Runtime: about 2-3 minutes.

library(FEAR)

run_dir <- file.path("output", "template_run")
tab_dir <- file.path("output", "tables")
fig_dir <- file.path("output", "figures")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
set.seed(20261122)

NSPLIT <- 1000L
NDEL <- 12L      # deletions examined by ap(), as in the FEAR tutorial and chapter
ALPHA_COLUMN <- 2L  # fast-test indicators are for the 10%, 5%, 1% levels: use the 5% one


# 1. DEFLATOR AND FRONTIER DATASET ------------------------------------------------

deflator_file <- file.path("data", "istat_nic_annual_2020_2022.csv")
if (!file.exists(deflator_file)) stop("Missing ", deflator_file, " (see README, data/).")
nic <- read.csv(deflator_file)
stopifnot(all(2020:2022 %in% nic$year), all(is.finite(nic$nic_annual_mean)),
          all(nic$nic_annual_mean > 0))
nic_index <- nic$nic_annual_mean[match(2020:2022, nic$year)]
names(nic_index) <- 2020:2022

panel <- read.csv(file.path(run_dir, "teaching_panel_2020_2023.csv"), check.names = FALSE)
d <- panel[panel$year %in% 2021:2023, c("id", "year", "publications")]
d <- d[order(d$id, d$year), ]
rownames(d) <- NULL
previous <- match(paste(d$id, d$year - 1L), paste(panel$id, panel$year))
stopifnot(!anyNA(previous))
d$faculty_lag <- panel$faculty[previous]
d$cost_nominal_lag <- panel$current_management[previous] / 1e6
d$nic_lag <- unname(nic_index[as.character(d$year - 1L)])
d$cost_real_lag <- d$cost_nominal_lag * nic_index[["2020"]] / d$nic_lag  # million EUR, 2020 prices
d$lag_log_teaching_load <- log(panel$students[previous] / panel$faculty[previous])
num <- c("publications", "faculty_lag", "cost_real_lag")
stopifnot(nrow(d) == 183L, length(unique(d$id)) == 61L,
          all(vapply(d[num], function(x) all(is.finite(x) & x > 0), logical(1))))

# Difference with the estimation sample of 06.
sample_06 <- read.csv(file.path(run_dir, "results_group_04_sample.csv"), check.names = FALSE)
only_here <- setdiff(unique(d$id), unique(sample_06$id))
sample_comparison <- data.frame(
  sample = c("Frontier (this script)", "OLS/SFA of 06"),
  rows = c(nrow(d), nrow(sample_06)),
  universities = c(length(unique(d$id)), length(unique(sample_06$id))),
  note = c(paste("includes", paste(only_here, collapse = "; "),
                 "(no 2018 field shares needed)"), "requires 2018 field shares")
)
print(sample_comparison)
write.csv(sample_comparison, file.path(tab_dir, "table_12_sample_comparison.csv"), row.names = FALSE)

X <- t(as.matrix(d[, c("faculty_lag", "cost_real_lag")]))  # 2 x 183
Y <- t(as.matrix(d$publications))                          # 1 x 183


# 2. OUTLIERS: WILSON (1993) DIAGNOSTIC -------------------------------------------

ap_result <- ap(X, Y, NDEL = NDEL)
png(file.path(fig_dir, "fig_12_ap_plot.png"), width = 1400, height = 1000, res = 150)
ap.plot(ap_result$ratio, main = "Wilson outlier diagnostic (ap.plot)")
dev.off()

# Row i of imat lists the i observations whose joint deletion gives the smallest
# log-ratio r0[i]. Every observation appearing in some row is a POTENTIAL outlier.
imat <- ap_result$imat
flagged <- sort(unique(as.vector(imat[!is.na(imat)])))
first_level <- vapply(flagged, function(k) min(which(apply(imat, 1, function(r) k %in% r))),
                      integer(1))
times_in_sets <- vapply(flagged, function(k) sum(imat == k, na.rm = TRUE), integer(1))

# Comparison with small universities (scatter of 06) and with the NHN residuals of 06
# (descriptive only: that NHN is poorly identified, see g4/11).
fits_06 <- readRDS(file.path(run_dir, "results_group_04_sfa_fits.rds"))
# Composite residual eps = y - x'b with the frontier coefficients of the 06 NHN
# (residuals() needs the original data, so it is computed here on the 06 sample).
b_nhn <- coef(fits_06$frontier)
X_06 <- model.matrix(reformulate(c("lag_log_faculty", "lag_lcurrent", "medicine_share18",
                                   "engphys_share18", "ssh_share18", "y2022", "y2023",
                                   "exposure"), response = "outcome"), data = sample_06)
stopifnot(identical(colnames(X_06), names(b_nhn)[-(1:2)]))
nhn_resid <- as.vector(sample_06$outcome - X_06 %*% b_nhn[-(1:2)])
resid_pct <- rank(nhn_resid) / length(nhn_resid)
match_06 <- match(paste(d$id[flagged], d$year[flagged]), paste(sample_06$id, sample_06$year))

outliers <- data.frame(
  obs = flagged, id = d$id[flagged], year = d$year[flagged],
  publications = d$publications[flagged], faculty_lag = d$faculty_lag[flagged],
  cost_real_lag = round(d$cost_real_lag[flagged], 2),
  first_deletion_level = first_level, times_in_deletion_sets = times_in_sets,
  small_university = log(d$publications[flagged]) < 4.5,
  nhn_residual_06 = round(nhn_resid[match_06], 3),
  nhn_residual_percentile = round(resid_pct[match_06], 3)
)
outliers <- outliers[order(outliers$first_deletion_level, outliers$obs), ]
rownames(outliers) <- NULL
print(outliers)
write.csv(outliers, file.path(tab_dir, "table_12_outliers.csv"), row.names = FALSE)
write.csv(data.frame(deleted = seq_along(ap_result$r0), log_ratio_r0 = ap_result$r0),
          file.path(tab_dir, "table_12_ap_r0.csv"), row.names = FALSE)


# 3. TESTS: CONVEXITY, THEN RETURNS TO SCALE --------------------------------------

describe_test <- function(name, indicators) {
  stopifnot(length(indicators) == 3L)
  data.frame(test = name, nsplit = NSPLIT,
             level_10 = ifelse(indicators[1] == 1, "rejected", "not rejected"),
             level_05 = ifelse(indicators[2] == 1, "rejected", "not rejected"),
             level_01 = ifelse(indicators[3] == 1, "rejected", "not rejected"))
}
convexity <- test.convexity.fast(X = X, Y = Y, ORIENTATION = 2, METRIC = 2, NSPLIT = NSPLIT)
convexity_rejected <- convexity[ALPHA_COLUMN] == 1
rts <- test.rts.fast(X = X, Y = Y, ORIENTATION = 2, METRIC = 2, NSPLIT = NSPLIT,
                     CONVEXITY = !convexity_rejected)
crs_rejected <- rts[ALPHA_COLUMN] == 1
tests <- rbind(
  describe_test("H0: convex technology (test.convexity.fast)", convexity),
  describe_test(paste0("H0: constant returns to scale (test.rts.fast, CONVEXITY = ",
                       !convexity_rejected, ")"), rts)
)
tests$note <- "fast tests give reject/not-reject decisions, not p-values; not rejecting H0 is not evidence that H0 is true"
print(tests)
write.csv(tests, file.path(tab_dir, "table_12_tests.csv"), row.names = FALSE)


# 4. ESTIMATOR CHOSEN BY THE FIXED RULE, EFFICIENCY AND INTERVALS -----------------

if (!convexity_rejected) {
  rts_code <- if (crs_rejected) 1L else 3L
  estimator <- if (rts_code == 3L) "DEA, constant returns to scale (RTS = 3)" else
    "DEA, variable returns to scale (RTS = 1)"
  eff <- dea(X, Y, RTS = rts_code, ORIENTATION = 2, METRIC = 2)
  ci <- dea.subsample(eff, X, Y, RTS = rts_code, ORIENTATION = 2, METRIC = 2,
                      nrep = 1000, subsample.sizes = NULL)
} else {
  estimator <- "FDH"
  eff <- fdh(X, Y, ORIENTATION = 2, METRIC = 2)
  ci <- fdh.subsample(eff, X, Y, ORIENTATION = 2, METRIC = 2,
                      nrep = 1000, subsample.sizes = NULL)
}
cat("Estimator chosen by the rule:", estimator, "\n")

# ci rows: lower bounds at 90/95/99%, then upper bounds at 90/95/99% (FEAR chapter uses
# rows 2 and 5 for the 95% interval). msize rows: chosen subsample size per level.
stopifnot(all(ci$ci[2, ] <= ci$ci[5, ]))
d$efficiency_farrell <- as.vector(eff)
d$ci95_low <- ci$ci[2, ]
d$ci95_high <- ci$ci[5, ]
d$subsample_size_95 <- ci$msize[2, ]
d$efficient <- abs(d$efficiency_farrell - 1) < 1e-8
d$ap_potential_outlier <- seq_len(nrow(d)) %in% flagged
write.csv(d, file.path(tab_dir, "table_12_efficiency.csv"), row.names = FALSE)

summarise_eff <- function(x, label) {
  data.frame(period = label, n = nrow(x), efficient = sum(x$efficient),
             share_efficient = mean(x$efficient),
             mean_farrell = mean(x$efficiency_farrell),
             median_farrell = median(x$efficiency_farrell),
             mean_ci95_low = mean(x$ci95_low), mean_ci95_high = mean(x$ci95_high),
             median_ci95_width = median(x$ci95_high - x$ci95_low))
}
efficiency_summary <- rbind(
  do.call(rbind, lapply(2021:2023, function(yr) summarise_eff(d[d$year == yr, ], yr))),
  summarise_eff(d, "2021-2023")
)
efficiency_summary$estimator <- estimator
efficiency_summary[3:9] <- lapply(efficiency_summary[3:9], round, digits = 3)
print(efficiency_summary)
write.csv(efficiency_summary, file.path(tab_dir, "table_12_efficiency_summary.csv"),
          row.names = FALSE)

subsample_info <- data.frame(
  candidate_sizes = paste(range(ci$subsample.sizes), collapse = "-"),
  n_candidates = ci$nm,
  chosen_m_95_median = median(ci$msize[2, ]),
  chosen_m_95_min = min(ci$msize[2, ]), chosen_m_95_max = max(ci$msize[2, ]),
  method = "subsample.sizes = NULL: m chosen per observation (Simar and Wilson 2011a)"
)
print(subsample_info)
write.csv(subsample_info, file.path(tab_dir, "table_12_subsample_sizes.csv"), row.names = FALSE)


# 5. FIGURE: EFFICIENCY AGAINST TEACHING LOAD IN t-1 ------------------------------

# Vertical axis: 1 / Farrell, in (0, 1] (1 = on the frontier); intervals transformed too.
# Readable without colour: one symbol per year, interval segments, labelled outliers,
# dashed lowess line (descriptive only).
d$shephard <- 1 / d$efficiency_farrell
years <- 2021:2023
symbols <- c(1, 2, 0)
png(file.path(fig_dir, "fig_12_efficiency_vs_load.png"), width = 1600, height = 1100, res = 150)
par(mar = c(4.5, 4.5, 3, 1))
plot(d$lag_log_teaching_load, d$shephard, type = "n", ylim = c(0, 1.05),
     xlab = "log(students / faculty), t-1",
     ylab = "Research efficiency, 1 / Farrell output score (1 = frontier)",
     main = paste("Research-only efficiency and teaching load,", estimator))
segments(d$lag_log_teaching_load, 1 / d$ci95_high, d$lag_log_teaching_load, 1 / d$ci95_low,
         col = "grey60")
points(d$lag_log_teaching_load, d$shephard, pch = symbols[match(d$year, years)])
smooth <- lowess(d$lag_log_teaching_load, d$shephard)
lines(smooth, lty = 2, lwd = 2)
# Potential outliers are marked with a cross (names in table_12_outliers.csv):
# text labels would overlap because they cluster in the centre of the plot.
out <- d[d$ap_potential_outlier, ]
points(out$lag_log_teaching_load, out$shephard, pch = 4, cex = 1.6, lwd = 1.5)
legend("bottomleft", legend = c(years, "ap potential outlier (see table_12_outliers.csv)",
                                "95% interval (subsampling)", "lowess (descriptive)"),
       pch = c(symbols, 4, NA, NA), lty = c(NA, NA, NA, NA, 1, 2),
       col = c(1, 1, 1, 1, "grey60", 1), bty = "n", cex = 0.85)
dev.off()


# 6. NOTES AND LIMITS ---------------------------------------------------------------

notes <- c(
  paste("Estimator chosen by the fixed rule:", estimator),
  "Pooled frontier: each university appears three times, so observations are not independent; tests and subsampling treat them as independent.",
  "One technology is assumed for 2021-2023.",
  "Scopus indexing may be incomplete for recent years: 2023 may look less efficient (SFA year effects in 06 about -0.05 in 2022 and -0.11 in 2023); g4/13 estimates one frontier per year as a check.",
  "Main analysis keeps all observations; the version without the ap potential outliers is a robustness check in g4/13.",
  "Fast tests (NSPLIT = 1000) give reject/not-reject decisions at 10/5/1%; failing to reject H0 is not evidence that H0 is true.",
  "Efficiency is relative to the observed best practice in this sample: it is not overall university quality."
)
if (estimator == "FDH") {
  notes <- c(notes, "FDH converges slowly (rate n^(1/(p+q))) and its intervals are wide.")
}
write.csv(data.frame(note = notes), file.path(tab_dir, "table_12_notes.csv"), row.names = FALSE)

cat("12 completed: frontier tables and figures written.\n")
