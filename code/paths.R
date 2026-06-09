#-------------------------------------------------
# PSPS: All paths in one place
#
# When moving to a new machine:
#   1. Edit project_root_fallback below
#   2. Find-replace the same path in the bootstrap else-branch of:
#      01_process_results.R, 02_paper_tables.R, 03_paper_figures.R,
#      04_number_plug.R, 00_results_diagnostic.R
#-------------------------------------------------

project_root_fallback <- "~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025"

# Resolve project root: from Rscript --file when available, else fallback
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  project_root <- dirname(dirname(normalizePath(sub("^--file=", "", file_arg))))
} else {
  project_root <- normalizePath(path.expand(project_root_fallback), mustWork = FALSE)
}
setwd(project_root)

# ---- Paths relative to project root ----
code_dir           <- file.path(project_root, "code")
results_dir        <- file.path(project_root, "results", "jan_2026_results")
case_crossover_dir <- file.path(results_dir, "case_crossover_results")
out_dir            <- file.path(project_root, "tables_figures")

# ---- Paths outside project (edit when moving) ----
data_dir           <- path.expand("~/Desktop/Desktop/epidemiology_PhD/01_data/clean")
analysis_dir       <- path.expand("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/data")
exp_dir            <- file.path(project_root, "exposure_data")

# ---- Aliases and subdirs (use these for consistency) ----
jan2026_results_dir <- results_dir
jun2026_results_dir <- file.path(project_root, "results", "jun_2026_results")
results_data_dir    <- case_crossover_dir
plots_dir           <- file.path(results_dir, "plots")
oneweek_out_dir     <- file.path(out_dir, "1week_duration")
