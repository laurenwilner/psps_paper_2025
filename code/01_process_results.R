#-------------------------------------------------
# PSPS: Process analytic results - January 2026
# New results with lag and age group metadata
#-------------------------------------------------

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(tidyverse)

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/jan_2026_results/case_crossover_results")
exp_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/exposure_data/")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/tables_figures/")
data_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
analysis_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/data/")

# Function to parse file name and extract metadata
parse_filename <- function(filename) {
  # Remove path and extension
  f <- gsub(".*/", "", filename)
  f <- gsub(".csv", "", f)
  f <- gsub("results_", "", f)
  
  # Extract cause (first part before underscore)
  cause <- str_extract(f, "^[^_]+")
  
  # Extract lag type (same_day or lag4)
  lag_type <- ifelse(grepl("same_day", f), "same_day", 
                     ifelse(grepl("lag4", f), "lag4", 
                            ifelse(grepl("1week_duration", f), "1week_duration", NA)))
  
  # Extract age group
  age_group <- ifelse(grepl("20-64 years", f), "20-64 years",
                      ifelse(grepl("65 and older", f), "65 and older",
                             ifelse(grepl("age 20 and older", f), "age 20 and older", NA)))
  
  # Map cause to display name
  cause_display <- case_when(
    cause == "cardio" ~ "Cardiovascular",
    cause == "copd" ~ "COPD",
    cause == "psych" ~ "Psychiatric",
    cause == "resp" ~ "Respiratory",
    TRUE ~ cause
  )
  
  return(list(
    cause = cause_display,
    cause_code = cause,
    lag_type = lag_type,
    age_group = age_group
  ))
}

#-------------------------------------------------
# Process same_day and lag4 results (excluding 1week_duration)
#-------------------------------------------------

# Get result files (excluding 1week_duration)
result_files <- list.files(results_dir, full.names = TRUE, recursive = TRUE)
result_files <- str_subset(result_files, "results_.*\\.csv$")
result_files <- str_subset(result_files, "1week_duration", negate = TRUE)

# Get covariance files (excluding 1week_duration)
cov_files_abs <- list.files(results_dir, full.names = TRUE, recursive = TRUE)
cov_files_abs <- str_subset(cov_files_abs, "vcov_absmodel_.*\\.csv$")
cov_files_abs <- str_subset(cov_files_abs, "1week_duration", negate = TRUE)

cov_files_hyb <- list.files(results_dir, full.names = TRUE, recursive = TRUE)
cov_files_hyb <- str_subset(cov_files_hyb, "vcov_hybmodel_.*\\.csv$")
cov_files_hyb <- str_subset(cov_files_hyb, "1week_duration", negate = TRUE)

# Process results for each combination
all_results_abs <- data.frame()
all_results_hyb <- data.frame()
cov_matrices_list <- list()

for(f in result_files) {
  metadata <- parse_filename(f)
  
  # Read results file
  df <- read.csv(f) %>%
    mutate(across(everything(), ~str_replace_all(., ":", ".")))
  
  # Split into abs (rows 1-10) and hyb (rows 11-20)
  df_abs <- df %>%
    slice(1:10) %>%
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
    mutate(
      Cause = metadata$cause,
      lag_type = metadata$lag_type,
      age_group = metadata$age_group,
      cause_code = metadata$cause_code
    )
  
  df_hyb <- df %>%
    slice(11:20) %>%
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
    mutate(
      Cause = metadata$cause,
      lag_type = metadata$lag_type,
      age_group = metadata$age_group,
      cause_code = metadata$cause_code
    )
  
  all_results_abs <- bind_rows(all_results_abs, df_abs)
  all_results_hyb <- bind_rows(all_results_hyb, df_hyb)
  
  # Process covariance matrices
  # Find matching covariance files - need to match cause, lag_type, and age_group
  # Match on cause_code_lag_type pattern and exact age_group string
  pattern_base <- paste0(metadata$cause_code, "_", metadata$lag_type)
  
  cov_file_abs <- cov_files_abs[
    str_detect(cov_files_abs, pattern_base) &
    str_detect(cov_files_abs, fixed(metadata$age_group))
  ]
  
  cov_file_hyb <- cov_files_hyb[
    str_detect(cov_files_hyb, pattern_base) &
    str_detect(cov_files_hyb, fixed(metadata$age_group))
  ]
  
  if(length(cov_file_abs) > 0) {
    cov_abs <- read.csv(cov_file_abs[1]) %>%
      mutate(across(everything(), ~str_replace_all(., ":", ".")))
    cov_name_abs <- paste0(metadata$cause_code, "_", metadata$lag_type, "_", 
                          gsub(" ", "_", metadata$age_group), "_abs_cov")
    cov_matrices_list[[cov_name_abs]] <- cov_abs
  }
  
  if(length(cov_file_hyb) > 0) {
    cov_hyb <- read.csv(cov_file_hyb[1]) %>%
      mutate(across(everything(), ~str_replace_all(., ":", ".")))
    cov_name_hyb <- paste0(metadata$cause_code, "_", metadata$lag_type, "_", 
                          gsub(" ", "_", metadata$age_group), "_hyb_cov")
    cov_matrices_list[[cov_name_hyb]] <- cov_hyb
  }
}

# Make OR, CI_Lower, CI_Upper numeric
all_results_abs <- all_results_abs %>%
  mutate(across(c("OR", "CI_Lower", "CI_Upper"), as.numeric))

all_results_hyb <- all_results_hyb %>%
  mutate(across(c("OR", "CI_Lower", "CI_Upper"), as.numeric))

# Save covariance matrices
if(length(cov_matrices_list) > 0) {
  saveRDS(cov_matrices_list, file = paste0(results_dir, "/../cov_matrices_jan2026.rds"))
}

# Save combined results files
# Option 1: Save all together with metadata columns
write.csv(all_results_abs, paste0(results_dir, "/../all_results_abs_jan2026.csv"), row.names = FALSE)
write.csv(all_results_hyb, paste0(results_dir, "/../all_results_hyb_jan2026.csv"), row.names = FALSE)

# Option 2: Also save separate files by lag_type and age_group for easier access
for(lag in unique(all_results_abs$lag_type)) {
  for(age in unique(all_results_abs$age_group)) {
    age_clean <- gsub(" ", "_", age)
    age_clean <- gsub("-", "_", age_clean)
    
    all_results_abs %>%
      filter(lag_type == lag, age_group == age) %>%
      select(-lag_type, -age_group, -cause_code) %>%
      write.csv(paste0(results_dir, "/../all_", lag, "_", age_clean, "_abs.csv"), row.names = FALSE)
    
    all_results_hyb %>%
      filter(lag_type == lag, age_group == age) %>%
      select(-lag_type, -age_group, -cause_code) %>%
      write.csv(paste0(results_dir, "/../all_", lag, "_", age_clean, "_hyb.csv"), row.names = FALSE)
  }
}

cat("\nProcessed", length(result_files), "result files\n")
cat("Lag types:", paste(unique(all_results_abs$lag_type), collapse = ", "), "\n")
cat("Age groups:", paste(unique(all_results_abs$age_group), collapse = ", "), "\n")
cat("Causes:", paste(unique(all_results_abs$Cause), collapse = ", "), "\n")
