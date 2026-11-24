# run_all_g4.R
# Master script of Group A, PEA project 4: "Teaching pressure and research output".
#
# Run from a CLEAN R session with the working directory set to the PEA_GroupA folder:
#   in RStudio: Session > Set Working Directory > (PEA_GroupA), then source("run_all_g4.R")
#   in a terminal opened in PEA_GroupA: Rscript run_all_g4.R
#
# Steps:
#   0. check the folder and the R version (R 4.5.x is required);
#   1. copy the instructor kit (template/) into output/template_run/,
#      so that template/ is never modified;
#   2. run the instructor scripts inside output/template_run/, with row checks:
#      00_setup, 00_simulated_causality, 01, 02, 05, 06 (GROUP = 4, RUN_FRONTIER = TRUE);
#   3. run the group scripts in g4/;
#   4. save software versions and sessionInfo() to output/sessionInfo.txt.
#
# Reference versions: R 4.5.3, sfa 1.2.0, FEAR 5.0.

# SWITCH ----------------------------------------------------------------------
# TRUE: run everything, including g4/11 and the g4/13 bootstrap convexity test.
#       Required for the submission: every output must be regenerated.
# FALSE: skip g4/11 and the g4/13 bootstrap test; their previous outputs are left
#        untouched and may be out of date.
RUN_SENSITIVITY <- TRUE


# 0. CHECK FOLDER AND R VERSION -----------------------------------------------

if (!file.exists("run_all_g4.R") || !dir.exists("template") || !dir.exists("g4")) {
  stop("Set the working directory to the PEA_GroupA folder before running run_all_g4.R.\n",
       "Current working directory: ", getwd())
}

r_minor <- strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
if (R.version$major != "4" || r_minor != "5") {
  stop("This project requires R 4.5.x (reference: R 4.5.3). You are using ",
       R.version.string, ".\n",
       "In RStudio choose R 4.5.x in Tools > Global Options > General > R version, ",
       "restart RStudio and run this script again.")
}

# Optional CLI mode for presentation edits after a successful complete run.
# It reuses existing tables, performs no estimation and preserves sessionInfo.txt.
# The default remains the complete pipeline, required for submission.
if ("--presentation-only" %in% commandArgs(trailingOnly = TRUE)) {
  source(file.path("g4", "15_final_figures_tables.R"), local = new.env(parent = globalenv()))
  source(file.path("g4", "16_causal_dag.R"), local = new.env(parent = globalenv()))
  cat("Presentation assets regenerated from existing tables; no estimation was run.\n")
  quit(save = "no", status = 0L)
}

root <- getwd()
run_dir <- file.path("output", "template_run")
dir.create(file.path("output", "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("output", "figures"), showWarnings = FALSE, recursive = TRUE)


# 1. COPY THE INSTRUCTOR KIT INTO A FRESH RUN FOLDER --------------------------

# The instructor scripts read and write files in their working folder.
# Running them in a copy keeps template/ identical to the original kit.
if (dir.exists(run_dir)) unlink(run_dir, recursive = TRUE)
dir.create(run_dir, recursive = TRUE)
kit_files <- list.files("template", full.names = TRUE)
stopifnot(length(kit_files) > 0L, all(file.copy(kit_files, run_dir, copy.date = TRUE)))

# Source one instructor script inside the run folder, in its own environment.
# Values in `settings` (e.g. GROUP) are assigned in that environment first.
run_kit_script <- function(script, settings = list()) {
  cat("\n========== ", script, " ==========\n", sep = "")
  env <- new.env(parent = globalenv())
  for (name in names(settings)) assign(name, settings[[name]], envir = env)
  setwd(run_dir)
  on.exit(setwd(root), add = TRUE)
  source(script, local = env)
  invisible(env)
}

check_rows <- function(d, rows, universities, label) {
  n_univ <- length(unique(d$id))
  if (nrow(d) != rows || n_univ != universities) {
    stop("Row check failed ", label, ": expected ", rows, " rows and ", universities,
         " universities, found ", nrow(d), " rows and ", n_univ, " universities.")
  }
  cat("Row check passed ", label, ": ", rows, " rows, ", universities,
      " universities.\n", sep = "")
}


# 2. RUN THE INSTRUCTOR SCRIPTS -----------------------------------------------

run_kit_script("00_setup.R")

sfa_version <- as.character(packageVersion("sfa"))
if (sfa_version != "1.2.0") {
  warning("The instructor verified the results with sfa 1.2.0; this session uses sfa ",
          sfa_version, ". All group members should use the same version.")
}

run_kit_script("00_simulated_causality.R")

# 01 regenerates teaching_panel_2020_2023.csv. Remove the copied file first,
# so the comparison below is made against a file that 01 actually wrote.
file.remove(file.path(run_dir, "teaching_panel_2020_2023.csv"))
run_kit_script("01_prepare_data.R")

# Check 1: the regenerated teaching panel equals the instructor's file (content, not bytes).
original_panel <- read.csv(file.path("template", "teaching_panel_2020_2023.csv"),
                           check.names = FALSE)
regenerated_panel <- read.csv(file.path(run_dir, "teaching_panel_2020_2023.csv"),
                              check.names = FALSE)
check_rows(regenerated_panel, 244L, 61L, "after 01 (teaching panel)")
panel_comparison <- all.equal(original_panel, regenerated_panel)
if (!isTRUE(panel_comparison)) {
  stop("The teaching panel regenerated by 01 differs from template/teaching_panel_2020_2023.csv:\n",
       paste(head(panel_comparison, 10), collapse = "\n"))
}
cat("Content check passed: the regenerated teaching panel equals the instructor's file.\n")

run_kit_script("02_frontiers.R")
run_kit_script("05_prepare_group_data.R")

# Check 2: 183 rows with a lagged exposure (61 universities x 2021-2023).
project_panel <- read.csv(file.path(run_dir, "project_panel_2020_2023.csv"),
                          check.names = FALSE)
lagged_rows <- project_panel[!is.na(project_panel$lag_log_teaching_load), ]
check_rows(lagged_rows, 183L, 61L, "after 05 (rows with lagged teaching load)")
stopifnot(identical(sort(unique(lagged_rows$year)), 2021:2023))

run_kit_script("06_group_project.R", settings = list(GROUP = 4L, RUN_FRONTIER = TRUE))

# Check 3: the effective estimation sample of 06 (2018 field shares required).
sample_06 <- read.csv(file.path(run_dir, "results_group_04_sample.csv"),
                      check.names = FALSE)
check_rows(sample_06, 180L, 60L, "after 06 (estimation sample)")


# 3. RUN THE GROUP SCRIPTS ----------------------------------------------------

g4_scripts <- c("07_sample_checks.R", "08_ratio_variation.R",
                "09_ratio_decomposition.R", "10_ols_specifications.R")
if (RUN_SENSITIVITY) {
  g4_scripts <- c(g4_scripts, "11_nhn_start_sensitivity.R")
} else {
  cat("\nRUN_SENSITIVITY = FALSE: g4/11_nhn_start_sensitivity.R skipped.\n")
}
# 12 always runs: its convexity and returns-to-scale tests choose the frontier estimator.
g4_scripts <- c(g4_scripts, "12_research_frontier.R")
# 13 always runs its robustness frontiers; its bootstrap tests run only if RUN_SENSITIVITY.
g4_scripts <- c(g4_scripts, "13_frontier_robustness.R")
# 15 formats existing results for the report and slides; no new estimation.
g4_scripts <- c(g4_scripts, "15_final_figures_tables.R")
g4_scripts <- c(g4_scripts, "16_causal_dag.R")

for (script in g4_scripts) {
  cat("\n========== g4/", script, " ==========\n", sep = "")
  source(file.path("g4", script), local = new.env(parent = globalenv()))
}


# 4. SAVE SOFTWARE VERSIONS ---------------------------------------------------

fear_version <- if ("FEAR" %in% rownames(installed.packages())) {
  as.character(packageVersion("FEAR"))
} else {
  "not installed"
}
writeLines(
  c("Group A, PEA project 4: software used for this run",
    "Reference versions: R 4.5.3, sfa 1.2.0, FEAR 5.0",
    paste("R:", R.version.string),
    paste("sfa:", sfa_version),
    paste("FEAR:", fear_version),
    paste("Run date:", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "",
    capture.output(sessionInfo())),
  file.path("output", "sessionInfo.txt")
)
cat("\nAll steps completed. Software versions saved to output/sessionInfo.txt.\n")
