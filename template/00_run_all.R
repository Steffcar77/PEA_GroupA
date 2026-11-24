# In RStudio: open VQR_Starter_V1 as a project/folder, then source this file.
# In a terminal: cd VQR_Starter_V1 && Rscript R/00_run_all.R
needed <- c("sfa","readxl","sandwich","lmtest", "frontier")
missing <- needed[!vapply(needed,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop("Install required packages first: install.packages(c(",
  paste(sprintf('"%s"',missing),collapse=", "),"))")
if(as.character(packageVersion("sfa"))!="1.2.0") warning("Results were verified with sfa 1.2.0; record version differences.")
dir.create("results",showWarnings=FALSE)
source("R/01_prepare_data.R")
source("R/02_frontiers.R")
source("R/03_exposure_diagnostics.R")
capture.output(sessionInfo(),file="results/sessionInfo.txt")
cat("Core teaching and pilot workflow completed.\n")
