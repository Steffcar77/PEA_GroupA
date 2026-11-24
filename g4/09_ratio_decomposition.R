# 09_ratio_decomposition.R
# Group A, PEA project 4. Decomposition of the student-faculty ratio:
#   log(S/F) = log S - log F,  so  dlog(S/F) = dlog S - dlog F.
# A higher ratio can come from more students or from fewer faculty: these are
# different sources (project card). Years 2020-2022 = years of the lagged exposure.
#
# Variance shares (they always sum to 1):
#   student share = cov(dlog S, dlog r) / var(dlog r)
#   faculty share = -cov(dlog F, dlog r) / var(dlog r)
# A share below 0 or above 1 means students and faculty moved in the same direction.
#
# Versions:
#   1. raw year-on-year changes (2020-2021, 2021-2022 and pooled);
#   2. pooled changes net of year effects (period means removed);
#   3. levels net of university and year effects: the residuals are additive,
#      res(log r) = res(log S) - res(log F). These remove the fixed effects used
#      in 06, but do not yet partial out its faculty and cost controls.
#
# Run through run_all_g4.R (working directory = PEA_GroupA).
# Outputs: output/tables/table_09_decomposition.csv
#          output/tables/table_09_change_sources.csv
#          output/figures/fig_09_change_sources.png

run_dir <- file.path("output", "template_run")
tab_dir <- file.path("output", "tables")
fig_dir <- file.path("output", "figures")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

panel <- read.csv(file.path(run_dir, "teaching_panel_2020_2023.csv"), check.names = FALSE)
sample_06 <- read.csv(file.path(run_dir, "results_group_04_sample.csv"), check.names = FALSE)


# 1. DATA: THE 60 UNIVERSITIES OF THE 06 SAMPLE, 2020-2022 --------------------

ids <- sort(unique(sample_06$id))
d <- panel[panel$id %in% ids & panel$year %in% 2020:2022,
           c("id", "year", "students", "faculty")]
d <- d[order(d$id, d$year), ]
rownames(d) <- NULL
stopifnot(nrow(d) == 180L)
d$log_s <- log(d$students)
d$log_f <- log(d$faculty)
d$log_r <- d$log_s - d$log_f

# Year-on-year changes within the same university (match on id and year - 1).
previous <- match(paste(d$id, d$year - 1L), paste(d$id, d$year))
has_previous <- !is.na(previous)
ch <- data.frame(
  id = d$id[has_previous],
  period = paste0(d$year[has_previous] - 1L, "-", d$year[has_previous]),
  d_log_s = d$log_s[has_previous] - d$log_s[previous[has_previous]],
  d_log_f = d$log_f[has_previous] - d$log_f[previous[has_previous]]
)
ch$d_log_r <- ch$d_log_s - ch$d_log_f
stopifnot(nrow(ch) == 120L)


# 2. VARIANCE SHARES ------------------------------------------------------------

decompose <- function(s, f, version, sample_label) {
  r <- s - f
  share_s <- cov(s, r) / var(r)
  share_f <- -cov(f, r) / var(r)
  stopifnot(isTRUE(all.equal(share_s + share_f, 1)))
  data.frame(version = version, sample = sample_label, n = length(r),
             sd_ratio_component = sd(r), share_students = share_s,
             share_faculty = share_f,
             flag = if (share_s < 0 || share_s > 1) "share outside [0, 1]" else "")
}

rows <- list()
for (p in sort(unique(ch$period))) {
  x <- ch[ch$period == p, ]
  rows[[p]] <- decompose(x$d_log_s, x$d_log_f, "1. Raw changes", p)
}
rows[["raw_pooled"]] <- decompose(ch$d_log_s, ch$d_log_f, "1. Raw changes", "pooled 2020-2022")

# Version 2: remove the common shock of each period.
s_net_year <- ch$d_log_s - ave(ch$d_log_s, ch$period)
f_net_year <- ch$d_log_f - ave(ch$d_log_f, ch$period)
rows[["net_year"]] <- decompose(s_net_year, f_net_year,
                                "2. Changes net of year effects", "pooled 2020-2022")

# Version 3: levels net of university and year effects.
res_s <- resid(lm(log_s ~ factor(id) + factor(year), data = d))
res_f <- resid(lm(log_f ~ factor(id) + factor(year), data = d))
res_r <- resid(lm(log_r ~ factor(id) + factor(year), data = d))
stopifnot(isTRUE(all.equal(unname(res_s - res_f), unname(res_r))))
rows[["levels_fe"]] <- decompose(res_s, res_f,
                                 "3. Levels net of university and year effects",
                                 "2020-2022 (180 university-years)")

decomposition <- do.call(rbind, rows)
rownames(decomposition) <- NULL
numeric_cols <- c("sd_ratio_component", "share_students", "share_faculty")
decomposition[numeric_cols] <- lapply(decomposition[numeric_cols], round, digits = 3)
print(decomposition)
if (any(decomposition$flag != "")) {
  warning("Some variance shares are outside [0, 1]: see table_09_decomposition.csv.")
}
write.csv(decomposition, file.path(tab_dir, "table_09_decomposition.csv"), row.names = FALSE)


# 3. SOURCES OF EACH CHANGE: SIGN OF dlog S AND dlog F --------------------------

direction <- function(x) ifelse(x > 0, "up", ifelse(x < 0, "down", "unchanged"))
ch$students_change <- direction(ch$d_log_s)
ch$faculty_change <- direction(ch$d_log_f)
ch$one <- 1L
ch$ratio_rose <- as.integer(ch$d_log_r > 0)
sources <- aggregate(cbind(changes = one, ratio_rose) ~ students_change + faculty_change,
                     data = ch, FUN = sum)
sources <- sources[order(-sources$changes), ]
rownames(sources) <- NULL
print(sources)
write.csv(sources, file.path(tab_dir, "table_09_change_sources.csv"), row.names = FALSE)


# 4. FIGURE: dlog S AGAINST dlog F ----------------------------------------------

# Readable without colour: symbols distinguish periods; the dashed 45-degree line
# marks an unchanged ratio; regions are labelled directly.
png(file.path(fig_dir, "fig_09_change_sources.png"), width = 1400, height = 1200, res = 150)
par(mar = c(4.5, 4.5, 3, 1))
lim <- range(c(ch$d_log_s, ch$d_log_f))
symbols <- c(1, 2)
periods <- sort(unique(ch$period))
plot(ch$d_log_f, ch$d_log_s, xlim = lim, ylim = lim,
     pch = symbols[match(ch$period, periods)],
     xlab = "Change in log faculty (dlog F)", ylab = "Change in log students (dlog S)",
     main = "Sources of changes in the student-faculty ratio, 2020-2022")
abline(0, 1, lty = 2)
abline(h = 0, v = 0, lty = 3)
text(lim[1], lim[2], "Ratio rises\n(above dashed line)", adj = c(0, 1), cex = 0.85)
text(lim[2], lim[1] + 0.15 * diff(lim), "Ratio falls\n(below dashed line)",
     adj = c(1, 0), cex = 0.85)
text(lim[2], lim[2], "Ratio unchanged", adj = c(1, 1.5), cex = 0.8, srt = 45)
legend("bottomleft", legend = periods, pch = symbols, bty = "n", inset = c(0, 0.12))
dev.off()

cat("09 completed: decomposition tables and fig_09_change_sources.png written.\n")
