#-------------------------------------------------
# PSPS: Paper figures
# March 2025
#-------------------------------------------------
# question for joan 
# should we plot the act interaction term or the summed, actual effect? 

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(dplyr)

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/Results\ -\ Mar\ 2025/")
exp_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/exposure_data/")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/")
data_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")

# load data -------------------------------------------------
test_csv <- data.frame(x = c("val1", "val2", "val3"), y = c(1, 2, 3))
write.csv(test_csv, paste0(out_dir, "test.csv"))
