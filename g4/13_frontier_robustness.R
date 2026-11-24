# 13_frontier_robustness.R
# Group A, PEA project 4, Block 2. Robustness checks of the research-only frontier of 12.
#
# Versions (each re-applies the estimator rule of 12 with the fast tests, NSPLIT = 1000):
#   R1-R3 one frontier per year (2021, 2022, 2023);
#   R4    nominal instead of deflated current costs;
#   R5    faculty + research fellows as the labour input;
#   R6    without the ap potential outliers of 12 (13 university-years);
#   R7    without the 3 universities with almost no Scopus output (Napoli L'Orientale,
#         Siena Stranieri, Perugia Stranieri): sensitivity to a possible coverage problem.
#         Scopus also indexes books; the completeness of this extract is not established.
# For each version: estimator chosen, median Farrell score, share of efficient units and the
# Spearman correlation of the scores with the main version (common university-years).
#
# Only if RUN_SENSITIVITY = TRUE: bootstrap (non-fast) test.convexity on the main dataset,
# NSPLIT = 10, NREP = 200, to report a p-value next to the fast decision. The bootstrap
# test.rts was dropped: it took more than 30 minutes even with NREP = 200.
#
# Run through run_all_g4.R after 12 (working directory = PEA_GroupA). Runtime: several minutes.

library(FEAR)

tab_dir <- file.path("output", "tables")
if (!exists("RUN_SENSITIVITY")) RUN_SENSITIVITY <- TRUE
set.seed(20261122)
NSPLIT <- 1000L

main <- read.csv(file.path(tab_dir, "table_12_efficiency.csv"), check.names = FALSE)
main_tests <- read.csv(file.path(tab_dir, "table_12_tests.csv"), check.names = FALSE)
main_summary <- read.csv(file.path(tab_dir, "table_12_efficiency_summary.csv"),
                         check.names = FALSE)
stopifnot(nrow(main) == 183L)
main_key <- paste(main$id, main$year)

# Research fellows in t-1, from the teaching panel.
panel <- read.csv(file.path("output", "template_run", "teaching_panel_2020_2023.csv"),
                  check.names = FALSE)
previous <- match(paste(main$id, main$year - 1L), paste(panel$id, panel$year))
stopifnot(!anyNA(previous))
main$labour_lag <- main$faculty_lag + panel$fellows[previous]


# 1. ONE VERSION = TESTS + ESTIMATOR RULE + SCORES --------------------------------

run_version <- function(label, dat, x_cols) {
  X <- t(as.matrix(dat[, x_cols]))
  Y <- t(as.matrix(dat$publications))
  convexity <- test.convexity.fast(X = X, Y = Y, ORIENTATION = 2, METRIC = 2, NSPLIT = NSPLIT)
  convexity_rejected <- convexity[2] == 1
  rts <- test.rts.fast(X = X, Y = Y, ORIENTATION = 2, METRIC = 2, NSPLIT = NSPLIT,
                       CONVEXITY = !convexity_rejected)
  crs_rejected <- rts[2] == 1
  if (!convexity_rejected) {
    rts_code <- if (crs_rejected) 1L else 3L
    estimator <- if (rts_code == 3L) "DEA CRS" else "DEA VRS"
    eff <- dea(X, Y, RTS = rts_code, ORIENTATION = 2, METRIC = 2)
  } else {
    estimator <- "FDH"
    eff <- fdh(X, Y, ORIENTATION = 2, METRIC = 2)
  }
  eff <- as.vector(eff)
  main_eff <- main$efficiency_farrell[match(paste(dat$id, dat$year), main_key)]
  scores <- data.frame(version = label, id = dat$id, year = dat$year, efficiency_farrell = eff)
  summary_row <- data.frame(
    version = label, rows = nrow(dat), universities = length(unique(dat$id)),
    convexity_5pct = ifelse(convexity_rejected, "rejected", "not rejected"),
    crs_5pct = ifelse(crs_rejected, "rejected", "not rejected"),
    estimator = estimator,
    median_farrell = median(eff),
    n_efficient = sum(abs(eff - 1) < 1e-8),
    share_efficient = mean(abs(eff - 1) < 1e-8),
    spearman_with_main = cor(eff, main_eff, method = "spearman")
  )
  cat(label, ":", estimator, "\n")
  list(summary = summary_row, scores = scores)
}

inputs_main <- c("faculty_lag", "cost_real_lag")
humanities <- c("Napoli L'Orientale", "Siena Stranieri", "Perugia Stranieri")
stopifnot(all(humanities %in% main$id))

versions <- list()
for (yr in 2021:2023) {
  versions[[length(versions) + 1]] <- run_version(paste("R", yr - 2020, ". Frontier ", yr, " only",
                                                        sep = ""),
                                                  main[main$year == yr, ], inputs_main)
}
versions[[length(versions) + 1]] <- run_version("R4. Nominal costs", main,
                                                c("faculty_lag", "cost_nominal_lag"))
versions[[length(versions) + 1]] <- run_version("R5. Faculty + fellows", main,
                                                c("labour_lag", "cost_real_lag"))
versions[[length(versions) + 1]] <- run_version("R6. Without ap potential outliers",
                                                main[!main$ap_potential_outlier, ], inputs_main)
versions[[length(versions) + 1]] <- run_version("R7. Without 3 humanities universities",
                                                main[!main$id %in% humanities, ], inputs_main)


# 2. SUMMARY TABLE (MAIN VERSION FIRST) ------------------------------------------

total <- main_summary[main_summary$period == "2021-2023", ]
main_row <- data.frame(
  version = "Main (g4/12): pooled 2021-2023, faculty + deflated costs",
  rows = nrow(main), universities = length(unique(main$id)),
  convexity_5pct = main_tests$level_05[1], crs_5pct = main_tests$level_05[2],
  estimator = "DEA CRS", median_farrell = total$median_farrell,
  n_efficient = total$efficient, share_efficient = total$share_efficient,
  spearman_with_main = 1
)
robustness <- rbind(main_row, do.call(rbind, lapply(versions, `[[`, "summary")))
robustness[c("median_farrell", "share_efficient", "spearman_with_main")] <-
  lapply(robustness[c("median_farrell", "share_efficient", "spearman_with_main")],
         round, digits = 3)
print(robustness)
write.csv(robustness, file.path(tab_dir, "table_13_robustness_summary.csv"), row.names = FALSE)
write.csv(do.call(rbind, lapply(versions, `[[`, "scores")),
          file.path(tab_dir, "table_13_robustness_scores.csv"), row.names = FALSE)

removed <- main[main$ap_potential_outlier, ]
years_removed <- table(removed$id)
all_years_out <- sort(names(years_removed)[years_removed == length(unique(main$year))])
notes <- data.frame(version = c("R1-R3", "R6", "R7", "All"), note = c(
  "One frontier per year: each year is compared only with itself, so a fall in recent Scopus counts cannot penalise 2023 against 2021.",
  sprintf(paste("R6 removes single university-year observations, not universities as such:",
                "%d observations of %d universities (%s). %s %s flagged in every year, so %s",
                "the sample entirely."),
          nrow(removed), length(unique(removed$id)), paste(sort(unique(removed$id)), collapse = "; "),
          paste(all_years_out, collapse = " and "),
          if (length(all_years_out) == 1) "is" else "are",
          if (length(all_years_out) == 1) "it leaves" else "they leave"),
  "R7 removes all years of Napoli L'Orientale, Siena Stranieri and Perugia Stranieri as a sensitivity check for possible output-coverage differences. Scopus also indexes books and monographs; a coverage error in this dataset has not been established. See Elsevier's Scopus content policy and selection page.",
  "Spearman correlations use the university-years common to the version and the main frontier; scores of different frontiers are not directly comparable in levels."
))
write.csv(notes, file.path(tab_dir, "table_13_notes.csv"), row.names = FALSE)


# 3. SUMMARY FIGURE (ALSO FOR SLIDES) ---------------------------------------------

# Dot plot, readable without colour: filled dot = main version (12), open dots = R1-R7.
fig_dir <- file.path("output", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
labels <- sub(":.*", "", robustness$version)
k <- nrow(robustness)
symbols <- c(19, rep(1, k - 1))
png(file.path(fig_dir, "fig_13_robustness_summary.png"), width = 1800, height = 1000, res = 150)
par(mfrow = c(1, 2), mar = c(4.5, 16, 3, 1))
plot(robustness$median_farrell, k:1, pch = symbols, cex = 1.4, yaxt = "n", ylab = "",
     xlab = "Median Farrell output score (1 = frontier)", main = "Median inefficiency",
     xlim = range(c(1, robustness$median_farrell)))
axis(2, at = k:1, labels = labels, las = 1, cex.axis = 0.8)
abline(v = robustness$median_farrell[1], lty = 2)
par(mar = c(4.5, 2, 3, 1))
plot(robustness$spearman_with_main, k:1, pch = symbols, cex = 1.4, yaxt = "n", ylab = "",
     xlab = "Spearman correlation with the main scores", main = "Ranking stability",
     xlim = c(min(0, robustness$spearman_with_main), 1))
abline(v = 1, lty = 2)
dev.off()


# 4. OPTIONAL: BOOTSTRAP TESTS WITH P-VALUES ON THE MAIN DATASET ------------------

if (RUN_SENSITIVITY) {
  set.seed(20261122)
  X <- t(as.matrix(main[, inputs_main]))
  Y <- t(as.matrix(main$publications))
  # Only the convexity test: the bootstrap test.rts took more than 30 minutes even with
  # NREP = 200 and was dropped; returns to scale rely on test.rts.fast (NSPLIT = 1000) in 12.
  convexity_boot <- test.convexity(X = X, Y = Y, ORIENTATION = 2, METRIC = 2,
                                   NSPLIT = 10, NREP = 200)
  format_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
  boot_tests <- data.frame(
    test = "H0: convex technology (test.convexity, bootstrap)",
    nsplit = 10, nrep = 200,
    p_mean_statistic = format_p(convexity_boot$pval[1]),
    p_ks_statistic = format_p(convexity_boot$pval[2]),
    note = "Simar and Wilson bootstrap test; first p from the averaged statistic, second from the Kolmogorov-Smirnov statistic over splits; only 10 splits (runtime), so less stable than the fast test with 1000 splits. Bootstrap test.rts dropped (over 30 minutes with NREP = 200)."
  )
  print(boot_tests)
  write.csv(boot_tests, file.path(tab_dir, "table_13_bootstrap_tests.csv"), row.names = FALSE)
} else {
  cat("RUN_SENSITIVITY = FALSE: bootstrap tests skipped.\n")
}

cat("13 completed: robustness tables written.\n")
