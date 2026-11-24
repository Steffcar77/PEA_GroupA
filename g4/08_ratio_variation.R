# 08_ratio_variation.R
# Group A, PEA project 4. Variation of the student-faculty ratio log(S/F):
# between universities versus within the same university, 2020-2022
# (the years in which the lagged exposure of 06 is measured).
#
# Run through run_all_g4.R (working directory = PEA_GroupA).
# Inputs:  output/template_run/teaching_panel_2020_2023.csv
#          output/template_run/results_group_04_sample.csv (sample of 06)
# Outputs: output/tables/table_08_ratio_sd.csv
#          output/tables/table_08_ratio_by_year.csv
#          output/figures/fig_08_ratio_variation.png

run_dir <- file.path("output", "template_run")
tab_dir <- file.path("output", "tables")
fig_dir <- file.path("output", "figures")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

panel <- read.csv(file.path(run_dir, "teaching_panel_2020_2023.csv"), check.names = FALSE)
sample_06 <- read.csv(file.path(run_dir, "results_group_04_sample.csv"), check.names = FALSE)


# 1. RATIO DATA: THE 60 UNIVERSITIES OF THE 06 SAMPLE, 2020-2022 --------------

ids <- sort(unique(sample_06$id))
d <- panel[panel$id %in% ids & panel$year %in% 2020:2022,
           c("id", "year", "students", "faculty")]
d <- d[order(d$id, d$year), ]
rownames(d) <- NULL
stopifnot(nrow(d) == 180L, length(unique(d$id)) == 60L)

d$log_ratio <- log(d$students / d$faculty)
d$university_mean <- ave(d$log_ratio, d$id)
d$within <- d$log_ratio - d$university_mean
d$net_fe <- resid(lm(log_ratio ~ factor(id) + factor(year), data = d))


# 2. STANDARD DEVIATIONS (n - 1) -----------------------------------------------

sd_total <- sd(d$log_ratio)
sd_between <- sd(tapply(d$log_ratio, d$id, mean))
sd_within <- sd(d$within)
sd_net <- sd(d$net_fe)

# Check: the same two SDs recomputed from the exposure of the 06 sample.
# Year t in the 06 sample corresponds to year t-1 here, so the year effects match.
s06_sd_total <- sd(sample_06$lag_log_teaching_load)
s06_sd_net <- sd(resid(lm(lag_log_teaching_load ~ factor(id) + factor(year),
                          data = sample_06)))
if (!isTRUE(all.equal(sd_total, s06_sd_total, tolerance = 1e-8)) ||
    !isTRUE(all.equal(sd_net, s06_sd_net, tolerance = 1e-8))) {
  stop("SDs of log(S/F) do not match the exposure of the 06 sample: total ",
       sd_total, " vs ", s06_sd_total, "; net of effects ", sd_net, " vs ", s06_sd_net)
}
cat("Check passed: SDs equal those of lag_log_teaching_load in the 06 sample.\n")

# Variance shares use sums of squares, so that SS between + SS within = SS total.
# The residual net of university AND year effects is part of the within component.
ss_total <- sum((d$log_ratio - mean(d$log_ratio))^2)
ss_between <- sum((d$university_mean - mean(d$log_ratio))^2)
ss_within <- sum(d$within^2)
ss_net <- sum(d$net_fe^2)
stopifnot(isTRUE(all.equal(ss_between + ss_within, ss_total)))

sd_table <- data.frame(
  measure = c("Total", "Between universities (SD of university means)",
              "Within universities (deviation from university mean)",
              "Net of university and year effects (residual; part of within)"),
  sd = c(sd_total, sd_between, sd_within, sd_net),
  share_of_total_ss = c(ss_total, ss_between, ss_within, ss_net) / ss_total,
  n = c(nrow(d), length(ids), nrow(d), nrow(d))
)
sd_table$sd <- round(sd_table$sd, 4)
sd_table$share_of_total_ss <- round(sd_table$share_of_total_ss, 4)
print(sd_table)
write.csv(sd_table, file.path(tab_dir, "table_08_ratio_sd.csv"), row.names = FALSE)

by_year <- do.call(rbind, lapply(2020:2022, function(yr) {
  x <- d[d$year == yr, ]
  ratio <- x$students / x$faculty
  data.frame(year = yr, universities = nrow(x),
             mean_log_ratio = mean(x$log_ratio), sd_log_ratio = sd(x$log_ratio),
             median_ratio = median(ratio), min_ratio = min(ratio), max_ratio = max(ratio))
}))
by_year[-(1:2)] <- lapply(by_year[-(1:2)], round, digits = 3)
print(by_year)
write.csv(by_year, file.path(tab_dir, "table_08_ratio_by_year.csv"), row.names = FALSE)


# 3. FIGURE: BETWEEN VERSUS WITHIN VARIATION, SAME VERTICAL SPAN ---------------

# Readable without colour: black lines and symbols, line types and direct labels.
# Both panels use the same vertical span, so their spreads are directly comparable.
span <- diff(range(d$log_ratio))
left_ylim <- range(d$log_ratio)
right_ylim <- c(-span / 2, span / 2)

png(file.path(fig_dir, "fig_08_ratio_variation.png"), width = 2000, height = 1000, res = 150)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3.5, 1))

# Panel A: one line per university; bold line = mean across universities.
plot(NA, xlim = c(2020, 2022.75), ylim = left_ylim, xaxt = "n",
     xlab = "Year (t-1, exposure year)", ylab = "log(students / faculty)",
     main = "A. Levels: differences between universities")
axis(1, at = 2020:2022)
for (u in ids) {
  x <- d[d$id == u, ]
  lines(x$year, x$log_ratio, col = "grey55", lwd = 0.8)
}
yearly_mean <- tapply(d$log_ratio, d$year, mean)
lines(2020:2022, yearly_mean, lwd = 3, lty = 1)
points(2020:2022, yearly_mean, pch = 15, cex = 1.3)
text(2022.05, yearly_mean[3], "Mean", pos = 4, font = 2)
last <- d[d$year == 2022, ]
extremes <- last[c(which.max(last$log_ratio), which.min(last$log_ratio)), ]
text(2022.05, extremes$log_ratio, extremes$id, pos = 4, cex = 0.75)
mtext(sprintf("SD total = %.3f; SD between = %.3f", sd_total, sd_between),
      side = 3, line = 0.2, cex = 0.85)

# Panel B: residuals net of university and year effects, by year.
# Deterministic horizontal spread (no random jitter) so the plot is reproducible.
plot(NA, xlim = c(2019.6, 2022.4), ylim = right_ylim, xaxt = "n",
     xlab = "Year (t-1, exposure year)", ylab = "Residual of log(students / faculty)",
     main = "B. Net of university and year effects")
axis(1, at = 2020:2022)
abline(h = 0, lty = 2)
for (yr in 2020:2022) {
  r <- d$net_fe[d$year == yr]
  offset <- ((rank(r, ties.method = "first") %% 7) - 3) * 0.04
  points(yr + offset, r, pch = 1, cex = 0.9)
}
mtext(sprintf("SD net of effects = %.3f (same vertical span as panel A)", sd_net),
      side = 3, line = 0.2, cex = 0.85)
dev.off()

cat("08 completed: SD tables and fig_08_ratio_variation.png written.\n")
