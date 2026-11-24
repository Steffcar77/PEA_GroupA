# Group A, PEA project 4. Presentation assets from existing outputs only.
# Run through run_all_g4.R. No model fitting or new dependencies.
# Figures: 16:9 PNG and vector PDF; tables: CSV and a self-contained HTML index.

tab_dir <- file.path("output", "tables")
fig_dir <- file.path("output", "figures")
read_table <- function(name) read.csv(file.path(tab_dir, name), check.names = FALSE)
flow <- read_table("table_sample_flow.csv")
desc <- read_table("table_sample_descriptives.csv")
ols <- read_table("table_10_ols_wide.csv")
variation <- read_table("table_08_ratio_sd.csv")
eff <- read_table("table_12_efficiency.csv")
rob <- read_table("table_13_robustness_summary.csv")
comparison <- read_table("table_12_sample_comparison.csv")

# Validate the presentation inputs; never silently drop missing observations.
stopifnot(nrow(eff) == 183L, length(unique(eff$id)) == 61L,
          !anyDuplicated(paste(eff$id, eff$year)),
          all(table(eff$id) == 3L), all(eff$year %in% 2021:2023),
          all(is.finite(eff$efficiency_farrell)), all(eff$efficiency_farrell >= 1 - 1e-8),
          all(is.finite(eff$publications)), all(eff$publications > 0),
          all(is.finite(eff$lag_log_teaching_load)),
          all(is.finite(eff$ci95_low)), all(is.finite(eff$ci95_high)),
          all(eff$ci95_low > 0), all(eff$ci95_low <= eff$ci95_high),
          nrow(variation) == 4L, nrow(rob) == 8L,
          identical(names(ols)[-1], paste0("(", 1:7, ")")),
          rob$estimator[1] == "DEA CRS")

eff$efficiency <- 1 / eff$efficiency_farrell
quad <- aggregate(cbind(efficiency, publications) ~ id, data = eff, FUN = median)
names(quad) <- c("university", "median_efficiency", "median_publications")
x_mid <- median(quad$median_efficiency)
y_mid <- median(quad$median_publications)
quad$efficiency_group <- ifelse(quad$median_efficiency >= x_mid,
                               "At or above median", "Below median")
quad$publication_group <- ifelse(quad$median_publications >= y_mid,
                                "At or above median", "Below median")
quad$efficiency_cutoff <- x_mid
quad$publication_cutoff <- y_mid

# Keep full original precision in CSV exports. Formatting applies only to HTML.
tables <- list(sample_flow = flow, sample_descriptives = desc,
               sample_comparison = comparison, ols_results = ols,
               frontier_robustness = rob, university_quadrants = quad)
for (name in names(tables)) {
  write.csv(tables[[name]], file.path(tab_dir, paste0("table_15_", name, ".csv")),
            row.names = FALSE)
}

draw_asset <- function(name, draw) {
  for (format in c("png", "pdf")) {
    path <- file.path(fig_dir, paste0("fig_15_", name, ".", format))
    if (format == "png") {
      png(path, width = 2400, height = 1350, res = 180, pointsize = 12)
    } else {
      pdf(path, width = 40 / 3, height = 7.5, pointsize = 12, useDingbats = FALSE)
    }
    tryCatch({
      par(mar = c(4.5, 5, 3, 1.5), oma = c(3, 0, 2, 0), las = 1,
          col = "grey15", col.axis = "grey15", col.lab = "grey15", bty = "l")
      draw()
    }, finally = dev.off())
  }
}
footer <- function(lines) mtext(lines, side = 1, outer = TRUE,
                                line = seq_along(lines) - 0.2, cex = 0.8)

draw_asset("variation", function() {
  par(mfrow = c(1, 2), mar = c(5, 8, 3, 1))
  labels <- c("Total", "Between\nuniversities", "Within\nuniversities", "Net of university\nand year effects")
  pos <- 4:1
  plot(variation$sd, pos, xlim = c(0, max(variation$sd) * 1.25),
       ylim = c(0.6, 4.4), yaxt = "n", ylab = "", pch = c(19, 1, 0, 17), cex = 1.6,
       xlab = "Standard deviation of log(students / faculty)", main = "Dispersion of teaching load")
  axis(2, at = pos, labels = labels, tick = FALSE, cex.axis = 0.9)
  segments(0, pos, variation$sd, pos, col = "grey60")
  points(variation$sd, pos, pch = c(19, 1, 0, 17), cex = 1.6)
  text(variation$sd, pos, sprintf("%.3f", variation$sd), pos = 4, cex = 0.9)
  par(mar = c(5, 2, 3, 2))
  shares <- 100 * variation$share_of_total_ss
  plot(shares, pos, xlim = c(0, 120), ylim = c(0.6, 4.4), yaxt = "n", ylab = "",
       xlab = "Share of total sum of squares (%)", main = "Variation available within universities",
       pch = c(19, 1, 0, 17), cex = 1.6)
  segments(0, pos, shares, pos, col = "grey60")
  points(shares, pos, pch = c(19, 1, 0, 17), cex = 1.6)
  text(shares, pos, sprintf("%.1f%%", shares), pos = 4, cex = 0.9)
  mtext("Teaching-load variation | 60 universities, exposure years 2020-2022", outer = TRUE,
        side = 3, font = 2, cex = 1.2)
  footer(c("Between + within shares = 100%; the net-of-effects component is part of within variation.",
           "Source: g4/08. SDs use n - 1; the between SD is calculated across university means."))
})

draw_asset("efficiency_vs_load", function() {
  plot(eff$lag_log_teaching_load, eff$efficiency, type = "n", ylim = c(0, 1.06),
       xlab = "log(students / faculty), t-1", ylab = "Research efficiency: 1 / Farrell (1 = frontier)",
       main = "Research efficiency and teaching load | pooled DEA CRS, 2021-2023")
  segments(eff$lag_log_teaching_load, 1 / eff$ci95_high,
           eff$lag_log_teaching_load, 1 / eff$ci95_low, col = "grey70")
  points(eff$lag_log_teaching_load, eff$efficiency, pch = c(1, 2, 0)[match(eff$year, 2021:2023)])
  flagged <- eff$ap_potential_outlier
  points(eff$lag_log_teaching_load[flagged], eff$efficiency[flagged], pch = 4, cex = 1.4)
  legend("bottomleft", c("2021", "2022", "2023", "ap potential outlier", "95% subsampling interval"),
         pch = c(1, 2, 0, 4, NA), lty = c(NA, NA, NA, NA, 1),
         col = c(rep("grey15", 4), "grey70"), bty = "n", cex = 0.85)
  footer(c("183 university-years, 61 universities. Descriptive association; not an estimated causal effect.",
           "Source: g4/12. Tests and subsampling treat repeated observations as independent; intervals inherit this limitation."))
})

draw_asset("robustness", function() {
  labels <- c("Main: pooled 2021-2023", "R1: frontier 2021", "R2: frontier 2022", "R3: frontier 2023",
              "R4: nominal costs", "R5: faculty + fellows", "R6: without ap outliers", "R7: without 3 humanities univ.")
  labels <- paste0(labels, " [", rob$estimator, "]")
  pos <- nrow(rob):1
  symbols <- ifelse(rob$estimator == "FDH", 17, 1)
  symbols[1] <- 19
  layout(matrix(1:2, nrow = 1), widths = c(1.65, 1))
  par(mar = c(4.5, 19, 3, 1))
  plot(rob$median_farrell, pos, xlim = c(1, 1.43), ylim = c(0.6, 8.4),
       yaxt = "n", xaxt = "n", ylab = "", pch = symbols, cex = 1.3,
       xlab = "Median Farrell output score (1 = frontier)", main = "Frontier-specific scores")
  axis(1, at = c(1, 1.2, 1.4), labels = c("1.0", "1.2", "1.4"))
  axis(2, at = pos, labels = labels, tick = FALSE, cex.axis = 0.75)
  abline(v = rob$median_farrell[1], lty = 2, col = "grey60")
  text(rob$median_farrell, pos, sprintf("%.3f", rob$median_farrell), pos = 4, cex = 0.8)
  par(mar = c(4.5, 1.5, 3, 3))
  plot(rob$spearman_with_main, pos, xlim = c(0.75, 1.07), ylim = c(0.6, 8.4),
       yaxt = "n", ylab = "", pch = symbols, cex = 1.3,
       xlab = "Spearman correlation with main scores", main = "Ranking stability")
  abline(v = 1, lty = 2, col = "grey60")
  text(rob$spearman_with_main, pos, sprintf("%.3f", rob$spearman_with_main), pos = 4, cex = 0.8)
  mtext("Robustness of the research frontier", outer = TRUE, side = 3, font = 2, cex = 1.2)
  footer(c("Source: g4/13. Correlations use common university-years; score levels across different frontiers are not directly comparable.",
           "R7 changes the selected estimator to FDH (triangle). Exclusion of the three humanities universities remains a sensitivity check."))
})

draw_asset("university_quadrants", function() {
  par(mar = c(4.5, 7, 3, 1.5))
  plot(quad$median_efficiency, quad$median_publications, log = "y",
       xlim = c(0, 1.08), ylim = c(min(quad$median_publications) / 2.5,
                                 max(quad$median_publications) * 2.5),
       pch = 1, cex = 1.1, yaxt = "n",
       xlab = "Median research efficiency, 2021-2023 (1 / Farrell; higher = more efficient)",
       ylab = "",
       main = "Research efficiency and publication volume | one point per university")
  mtext("Median annual Scopus publications, 2021-2023 (log scale)", side = 2, line = 5, las = 0)
  ticks <- c(30, 100, 300, 1000, 3000, 10000)
  axis(2, at = ticks, labels = format(ticks, big.mark = ",", trim = TRUE))
  abline(v = x_mid, h = y_mid, lty = 2, col = "grey45")
  # Label four deterministic extrema: a small, reproducible selection, no rankings implied.
  chosen <- unique(c(which.min(quad$median_efficiency), which.max(quad$median_efficiency),
                     which.min(quad$median_publications), which.max(quad$median_publications)))
  label_pos <- ifelse(quad$median_efficiency[chosen] > x_mid, 2, 4)
  label_pos[chosen == which.max(quad$median_efficiency)] <- 4
  text(quad$median_efficiency[chosen], quad$median_publications[chosen],
       quad$university[chosen], pos = label_pos,
       offset = 0.65, cex = 0.8)
  mtext(sprintf("Median cutoffs: efficiency %.3f; publications %s", x_mid,
                format(y_mid, big.mark = ",", trim = TRUE)), side = 3, line = 0.2, cex = 0.85)
  footer(c("Source: g4/12; medians of the three annual observations for each of 61 universities. Dashed lines: medians across universities.",
           "Descriptive quadrants, not university quality or causal effects. Scopus coverage differs across disciplines."))
})

# Readable tables and figure captions in a portable, generated HTML document.
escape_html <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
html_table <- function(d) {
  d[] <- lapply(d, function(x) {
    if (is.numeric(x)) x <- format(x, digits = 6, trim = TRUE)
    x[is.na(x)] <- ""
    escape_html(x)
  })
  c("<div class='scroll'><table><thead><tr>",
    paste0("<th>", escape_html(names(d)), "</th>", collapse = ""), "</tr></thead><tbody>",
    apply(d, 1, function(row) paste0("<tr>", paste0("<td>", row, "</td>", collapse = ""), "</tr>")),
    "</tbody></table></div>")
}
titles <- c("Sample construction", "Descriptive statistics: OLS sample", "OLS and frontier samples",
            "OLS results: seven specifications", "Frontier robustness", "University quadrant data")
notes <- c(
  "Source: g4/07. Row losses are sequential; Siena Stranieri lacks 2018 field shares.",
  "Source: g4/07. 180 university-years, 60 universities. Costs in this table are nominal; the main DEA uses deflated costs.",
  "Source: g4/12. The DEA sample includes Siena Stranieri because field shares are not required.",
  paste("Source: g4/10. Outcome: log publications. SEs in parentheses: university-clustered HC1; 95% intervals use t with 59 degrees of freedom.",
        "All columns control for lagged log faculty and year effects. Column (7) uses regular students.",
        "Targets label the intended estimands under identifying assumptions; these observational estimates do not establish causality."),
  paste("Source: g4/13. Farrell scores >= 1; 1 is the frontier. Fast tests report decisions at 5%, not p-values.",
        "Correlations use common observations; score levels from different frontiers are not directly comparable.",
        "R7 excludes Napoli L'Orientale, Siena Stranieri and Perugia Stranieri. Non-rejection does not prove the null."),
  "Source: g4/12. Each row aggregates three years by medians. Cutoffs are medians across universities; ties belong to the at-or-above group."
)
html <- c("<!doctype html><html lang='en'><meta charset='utf-8'>",
          "<title>PEA Group A - Final tables and figures</title>",
          "<style>body{font:16px Arial,sans-serif;max-width:1400px;margin:40px auto;padding:0 24px;color:#222}h1,h2{line-height:1.3}.scroll{overflow-x:auto}table{border-collapse:collapse;font-size:14px;width:100%}th,td{border-bottom:1px solid #ccc;padding:10px;text-align:left}th{background:#eee}img{width:100%;height:auto}p{line-height:1.6}@media print{h2{break-after:avoid}table{font-size:10px}}</style>",
          "<h1>Teaching pressure and research output</h1>",
          "<p>Presentation assets generated by g4/15 from existing results. No new estimation. Figures are available as 16:9 PNG and vector PDF.</p>")
for (i in seq_along(tables)) {
  html <- c(html, paste0("<h2>", titles[i], "</h2><p>", notes[i], "</p>"),
            html_table(tables[[i]]), paste0("<p><a href='table_15_", names(tables)[i], ".csv'>Download CSV</a></p>"))
}
for (name in c("variation", "efficiency_vs_load", "robustness", "university_quadrants")) {
  stem <- paste0("../figures/fig_15_", name)
  html <- c(html, paste0("<h2>", gsub("_", " ", name), "</h2><img src='", stem,
                         ".png' alt='", gsub("_", " ", name), "'><p><a href='", stem, ".pdf'>Vector PDF</a></p>"))
}
html <- c(html, "<h2>Causal diagram (g4/16)</h2>",
          "<p>Working assumptions, not estimated effects. Generated by g4/16 after this index.</p>",
          "<img src='../figures/fig_16_causal_dag.png' alt='Assumed causal diagram'>",
          "<p><a href='../figures/fig_16_causal_dag.pdf'>Vector PDF</a> | <a href='table_16_dag_edges.csv'>Edges and rationale</a> | <a href='table_16_dag_notes.csv'>Assumptions and limits</a></p>")
writeLines(c(html, "</html>"), file.path(tab_dir, "table_15_presentation.html"), useBytes = TRUE)
cat("15 completed: six CSV tables, HTML index and four figures (PNG + PDF); no new estimation.\n")
