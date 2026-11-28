# Group A, project 4. Assumed causal structure, not an estimated model.
# Run through run_all_g4.R, including its --presentation-only mode.
# Sources: project booklet pp. 1, 5, 12; intervention and concerns in CLAUDE.md.
# Only reputation -> students / research was explicitly specified in the old notes.
# Other edges below make plausible working assumptions explicit for discussion.
# PhD students (added 28 November 2026, now in the research-staff input of g4/12): dashed
# "ambiguous" edges from reputation, size and students, and to publications; they may be a
# confounder or a mediator, so they are not a control of the main OLS specification.
# No additional package, model, random operation or adjustment-set claim.

fig_dir <- file.path("output", "figures")
tab_dir <- file.path("output", "tables")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

nodes <- data.frame(
  id = c("reputation", "field", "size", "students", "faculty", "load", "costs", "publications", "phd"),
  label = c("Prior reputation", "Field composition\n(2018 proxy)", "Prior institutional\nsize / capacity",
            "Students S\n(t-1)", "Faculty F\n(t-1; held fixed)", "Teaching load\nL = S / F (t-1)",
            "Resources / costs C\n(t-1; timing unclear)", "Scopus publications Y\n(t)",
            "PhD students\n(t-1; role ambiguous)"),
  x = c(0.13, 0.13, 0.13, 0.42, 0.42, 0.67, 0.67, 0.90, 0.42),
  y = c(0.80, 0.50, 0.20, 0.80, 0.20, 0.73, 0.27, 0.50, 0.385),
  width = c(0.20, 0.20, 0.20, 0.17, 0.17, 0.19, 0.21, 0.19, 0.17),
  height = rep(0.13, 9),
  status = c("not directly observed", "observed proxy", "conceptual baseline capacity",
             "observed", "observed; initial capacity is an intervention assumption",
             "deterministic ratio, not an independently assigned treatment",
             "observed annual spending; within-year causal order is unknown", "observed bibliometric outcome",
             "observed; possible confounder or mediator, part of the research-staff frontier input")
)
edges <- data.frame(
  from = c("reputation", "reputation", "field", "field", "size", "size",
           "students", "faculty", "faculty", "load", "students", "costs",
           "reputation", "size", "students", "phd"),
  to = c("students", "publications", "students", "publications", "students", "faculty",
         "load", "load", "publications", "publications", "costs", "publications",
         "phd", "phd", "phd", "publications"),
  type = c(rep("assumed", 6), "definition", "definition", "assumed", "assumed", "timing", "assumed",
           rep("ambiguous", 4)),
  rationale = c(
    "Prior reputation can attract students (project card).",
    "Prior reputation / research strength can predict subsequent research (project card).",
    "Programme / field mix can influence enrolment; 2018 shares are an imperfect proxy.",
    "Field mix can influence research production and its bibliometric measurement.",
    "Baseline institutional capacity can influence enrolment.",
    "Baseline institutional capacity can influence faculty stock.",
    "S is the numerator of L = S/F; this is a deterministic definition.",
    "F is the denominator of L = S/F; this is a deterministic definition.",
    "Faculty supplies research capacity in addition to entering the ratio.",
    "Teaching demands may change time available for research; sign is not imposed.",
    "Possible fee/spending response to enrolment; same-year data do not establish this ordering.",
    "Spending may support research production; the measured category is broader than research spending.",
    "Prior reputation / research strength can attract PhD students and funding (confounding path).",
    "Baseline institutional capacity can determine the size of PhD programmes (confounding path).",
    "Mediator channel: more students -> more resources -> more PhD positions; timing and existence unverified.",
    "PhD students contribute to research output (co-authorship); part of the research-staff input of g4/12."
  )
)

# A real DAG check: remove zero-indegree nodes until none remain. A remaining node
# would indicate an unintended directed cycle in the graph being delivered.
stopifnot(!anyDuplicated(nodes$id), all(edges$from %in% nodes$id), all(edges$to %in% nodes$id))
remaining <- nodes$id
while (length(remaining)) {
  incoming <- edges$to[edges$from %in% remaining & edges$to %in% remaining]
  roots <- setdiff(remaining, incoming)
  if (!length(roots)) stop("The proposed causal graph contains a directed cycle.")
  remaining <- setdiff(remaining, roots)
}
write.csv(nodes, file.path(tab_dir, "table_16_dag_nodes.csv"), row.names = FALSE)
write.csv(edges, file.path(tab_dir, "table_16_dag_edges.csv"), row.names = FALSE)
notes <- c(
  "Working DAG for an externally imposed increase in students at fixed initial faculty; arrows are assumptions, not estimated effects.",
  "Annual exposure, faculty and costs are all observed in t-1. Their common lag does not establish which preceded assignment.",
  "The dashed S -> C arrow represents a candidate mediator. If pre-existing resources also drive enrolment, a time-indexed graph with pre- and post-assignment resources is needed.",
  "Without cost control: candidate total response to students at fixed faculty. With cost control: candidate fixed-faculty, fixed-spending target. Neither regression identifies these effects without additional assumptions.",
  "L = S/F is deterministic. Changing students at fixed faculty differs from reducing faculty; do not interpret an independent intervention on the ratio without specifying its source.",
  "Prior reputation and institutional capacity are conceptual common causes, not new regressors. University effects cannot guarantee removal of time-varying confounding.",
  "PhD students have an ambiguous role: a confounder if reputation or capacity drive both PhD programmes and publications, a mediator if more students bring resources and PhD positions (dashed S -> PhD). Controlling for them could remove part of the effect of interest, so they are not in the main OLS specification (only in column (8) of g4/10); they enter the frontier as part of research capacity (g4/12).",
  "The diagram is deliberately incomplete: student quality, programme changes, staff responses and common year shocks may add paths. No sufficient adjustment set is claimed.",
  "Source: docs/VQR_Ten_Group_Projects.pdf, pp. 1, 5 and 12; working assumptions documented in CLAUDE.md."
)
write.csv(data.frame(note = notes), file.path(tab_dir, "table_16_dag_notes.csv"), row.names = FALSE)

# Short routes avoid crossing boxes. Crossings without a node are not junctions.
draw_dag <- function() {
  par(mar = c(0, 0, 0, 0), xpd = NA)
  plot.new()
  plot.window(xlim = c(0, 1.02), ylim = c(-0.29, 1.11), xaxs = "i", yaxs = "i")
  text(0.51, 1.065, "Teaching demand and research: an assumed causal diagram", font = 2, cex = 1.35)
  text(0.51, 1.015, "Intervention: more students at fixed initial faculty | Outcome: publications in the following year", cex = 0.92)
  for (i in seq_len(nrow(edges))) {
    a <- nodes[match(edges$from[i], nodes$id), ]
    b <- nodes[match(edges$to[i], nodes$id), ]
    route <- switch(paste(edges$from[i], edges$to[i], sep = ":"),
      "reputation:publications" = rbind(c(a$x, a$y + a$height / 2), c(a$x, 0.94),
                                          c(b$x, 0.94), c(b$x, b$y + b$height / 2)),
      "faculty:publications" = rbind(c(a$x, a$y - a$height / 2), c(a$x, 0.08),
                                      c(b$x, 0.08), c(b$x, b$y - b$height / 2)),
      "size:students" = rbind(c(a$x + a$width / 2, a$y + 0.025), c(0.28, 0.34),
                                c(0.28, 0.70), c(b$x - b$width / 2, b$y - 0.025)),
      "students:phd" = rbind(c(a$x, a$y - a$height / 2), c(b$x, b$y + b$height / 2)),
      "reputation:phd" = rbind(c(a$x + a$width / 2, a$y - 0.04), c(b$x - b$width / 2, b$y + 0.03)),
      "size:phd" = rbind(c(a$x + a$width / 2, a$y + 0.04), c(b$x - b$width / 2, b$y - 0.03)),
      rbind(c(a$x + a$width / 2, a$y), c(b$x - b$width / 2, b$y))
    )
    lty <- switch(edges$type[i], assumed = 1, definition = 3, timing = 2, ambiguous = 2)
    lwd <- if (edges$from[i] == "load") 2.5 else 1.2
    lines(route, lty = lty, lwd = lwd, col = "grey30")
    n <- nrow(route)
    arrows(route[n - 1, 1], route[n - 1, 2], route[n, 1], route[n, 2],
           length = 0.09, angle = 22, lty = lty, lwd = lwd, col = "grey30")
  }
  for (i in seq_len(nrow(nodes))) {
    n <- nodes[i, ]
    rect(n$x - n$width / 2, n$y - n$height / 2, n$x + n$width / 2, n$y + n$height / 2,
         col = if (n$id %in% c("load", "publications")) "grey92" else "white",
         border = "grey20", lwd = if (n$id == "load") 2 else 1)
    text(n$x, n$y, n$label, cex = 0.9)
  }
  legend(0.04, 0.025, legend = c("Assumed causal link (no sign imposed)", "Definition of S/F",
                                 "Possible mediation or confounding: role / timing unverified"),
         lty = c(1, 3, 2), lwd = 1.2, bty = "n", cex = 0.8, ncol = 3, x.intersp = 0.6)
  text(0.51, -0.085, "S, F and C share the same annual lag: this does not establish their within-year order.", cex = 0.88)
  text(0.51, -0.14, "If resources precede enrolment, the graph must distinguish baseline resources from later spending responses.", cex = 0.84)
  text(0.51, -0.195, "Student quality, staff responses and year shocks may add paths. This diagram does not establish causal identification.", cex = 0.84)
  text(0.51, -0.255, "Source: project booklet, pp. 1, 5, 12; explicit working assumptions in table_16_dag_edges.csv and table_16_dag_notes.csv.", cex = 0.75)
}
for (format in c("png", "pdf")) {
  path <- file.path(fig_dir, paste0("fig_16_causal_dag.", format))
  if (format == "png") png(path, width = 2400, height = 1350, res = 180, pointsize = 12)
  else pdf(path, width = 40 / 3, height = 7.5, pointsize = 12, useDingbats = FALSE)
  tryCatch(draw_dag(), finally = dev.off())
}
cat("16 completed: DAG validated as acyclic; PNG, PDF, node/edge tables and assumptions written.\n")
