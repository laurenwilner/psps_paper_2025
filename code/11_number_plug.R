#-------------------------------------------------
# PSPS: Paper figures
# March 2025
#-------------------------------------------------
# question for joan 
# should we plot the act interaction term or the summed, actual effect? 

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(dplyr, readr)

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/Results\ -\ Mar\ 2025/")
exp_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/exposure_data/")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/")
data_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")

# load data -------------------------------------------------


# create each number to plug as a var -----------------------
vala <- 1
valb <- 2
valc <- 3

# write the numbers to a file -----------------------
all_vars <- ls()
val_to_tex <- sapply(all_vars, function(var_name) {
  var_value <- get(var_name)
  # Only include numeric values
  if (is.numeric(var_value) && length(var_value) == 1) {
    paste0("\\newcommand{\\", var_name, "}{", var_value, "}")
  } else {
    NULL
  }
})

values <- val_to_tex[!sapply(val_to_tex, is.null)]
write_lines(values, "analysis-values.tex")
