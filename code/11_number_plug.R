#-------------------------------------------------
# PSPS: Paper figures
# March 2025
#-------------------------------------------------
# question for joan 
# should we plot the act interaction term or the summed, actual effect? 

# setup -------------------------------------------------
rm(list = ls()) # important to get rid of existing vars! 
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(dplyr, readr)

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/")
exp_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/exposure_data/")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/")
data_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")

# load data -------------------------------------------------
psps_exp_df <- read_csv(paste0(exp_dir, "ca_ZIP_daily_psps_no_washout_classified_2013-2022.csv"))
psps_exp_summary <- read.csv(paste0(exp_dir, "daily_psps_binary.csv")) 
wf_exp_df <- read_csv(paste0(exp_dir, "zip_wfpm20132019.csv"))
results_abs_df <- read_csv(paste0(results_dir, "all_lag0_abs.csv"))
results_hyb_df <- read_csv(paste0(results_dir, "all_lag0_hyb.csv"))
exposure_summary_abs_df <- read.csv(paste0(results_dir, "/Exposure\ summaries/AbsPSPS_wf_expsummary_byOOI_V2.csv"))
exposure_summary_hybrid_df <- read.csv(paste0(results_dir, "/Exposure\ summaries/HybPSPS_wf_expsummary_byOOI_V2.csv"))
exp_abs_sm_df <- read.csv(paste0(results_dir, "/Exposure\ summaries/AbsPSPS_wf_expsummary_V2.csv"))
exp_hyb_sm_df <- read.csv(paste0(results_dir, "/Exposure\ summaries/HybPSPS_wf_expsummary_V2.csv"))
zip_shp <- st_read(paste0(exp_dir, "ca_zip.geojson")) %>% 
            rename(zip_code = ZIP_CODE) %>%
            select(c("zip_code", "geometry"))
ca_zips <- zip_shp$zip_code %>% unique()
combined_exp_df <- read_csv(paste0(exp_dir, "zip_daily_psps_wf_exposure.csv"))

# create each number to plug as a var -----------------------
# XXXX hospital admission or emergency department visits
total_visits <- 999

# Respiratory visits were most commonly observed (n = XXXX)
resp_visits <- 999

# we included XXXX PSPS events in this study
n_psps_events <- length(unique(psps_exp_df$psps_event_id))

# median duration of a PSPS event was XX hours
median_duration <- median(psps_exp_df$duration, na.rm = TRUE)

# XXXX zip code -days in our 7-year study period that experienced PSPS events
zip_days <- nrow(psps_exp_summary)

# each zip code experienced, on average, XXXX events
zip_events <- psps_exp_summary %>% group_by(zip_code) %>% 
  summarise(n_events = n()) %>% 
  summarise(mean_events = mean(n_events, na.rm = TRUE)) %>% 
  pull(mean_events)

# XXXX {severity level} was the most common when we used our absolute metric
abs_severity_df <- psps_exp_df %>% 
  group_by(severity_customers) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
most_common_sev_abs <- abs_severity_df[1,]$severity_customers
second_most_common_sev_abs <- abs_severity_df[2,]$severity_customers

# XXXX {severity level} was most common according to our relative metric
hyb_severity_df <- psps_exp_df %>% 
  group_by(severity_hybrid) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

most_common_sev_hyb <- hyb_severity_df[1,]$severity_hybrid
second_most_common_sev_hyb <- hyb_severity_df[2,]$severity_hybrid

# XXXX of the YYYY zip codes in California experienced at least one PSPS event
zip_codes_with_events <- psps_exp_summary %>% 
  group_by(zip_code) %>% 
  summarise(n_events = n()) %>% 
  filter(n_events > 0) %>% 
  nrow()

ca_zip_codes <- length(ca_zips)

# XXX zip code-days of wildfire smoke \PM
zip_code_days_wf <- wf_exp_df %>% 
  filter(mean_lag05_per10 > 0) %>%
  group_by(zip_code) %>% 
  summarise(n_days = n()) %>% 
  summarise(total_days = sum(n_days, na.rm = TRUE)) %>% 
  pull(total_days)

# YYY zip code-days of co-occurring wildfire smoke
zip_days_dual_exp <- combined_exp_df %>% 
    filter(wf > 0 & psps_event > 0) %>%
    group_by(zip_code, date) %>%
    summarise(n_days = n()) %>%
    ungroup() %>%
    mutate(total_days = sum(n_days, na.rm = TRUE)) %>%
    pull(total_days) %>% unique()

# mean wildfire smoke \PM across the study period was XXXX
mean_wf_pm <- wf_exp_df %>% 
  filter(mean_lag05_per10 > 0) %>% 
  summarise(mean_pm = mean(mean_lag05_per10, na.rm = TRUE)) %>% 
  pull(mean_pm)
  
# XXX case-days (absolute, all severities)
case_days_abs <- exp_abs_sm_df %>% 
    filter(case_indicator == 1) %>%
    mutate(n = sum(count)) %>% 
    pull(n) %>% unique()

# XXX control-days (absolute, all severities)
control_days_abs <- exp_abs_sm_df %>% 
    filter(case_indicator == 0) %>%
    mutate(n = sum(count)) %>% 
    pull(n) %>% unique()

# XXX case-days (hybrid, all severities)
case_days_hyb <- exp_hyb_sm_df %>% 
    filter(case_indicator == 1) %>%
    mutate(n = sum(count)) %>% 
    pull(n) %>% unique()

# XXX control-days (hybrid, all severities)
control_days_hyb <- exp_hyb_sm_df %>% 
    filter(case_indicator == 0) %>%
    mutate(n = sum(count)) %>% 
    pull(n) %>% unique()

# XXXX cardiovascular case-days during a severe outage and XXXX respiratory case-days
case_days_abs_sev_cvd <- exposure_summary_abs_df %>% 
    filter(severity_customers == "Severe" & OOI == "cardio") %>% 
    mutate(n = sum(count)) %>% 
    pull(n) %>% unique()

case_days_abs_sev_resp <- exposure_summary_abs_df %>%
    filter(severity_customers == "Severe" & OOI == "resp") %>% 
    mutate(n = sum(count)) %>% 
    pull(n) %>% unique()

# greatest number of encounters associated with the XXX exposure level, with XXXX {cause} case-days during a severe outage and XXXX {cause} case-days
 highest_encounters_exp_level <- exposure_summary_abs_df %>% 
    filter(severity_customers != "none") %>%
    group_by(severity_customers) %>% 
    summarise(n = sum(count)) %>% 
    arrange(desc(n)) %>% 
    slice(1) %>% 
    pull(severity_customers)

resp_case_days_sev <- exposure_summary_abs_df %>% 
    filter(severity_customers == highest_encounters_exp_level & OOI == "resp") %>% 
    mutate(n = sum(count)) %>% 
    pull(n) %>% unique()

cvd_case_days_sev <- exposure_summary_abs_df %>%
    filter(severity_customers == highest_encounters_exp_level & OOI == "cardio") %>% 
    mutate(n = sum(count)) %>% 
    pull(n) %>% unique()

# respiratory absolute metric OR: XXX, 95\CI: XXX, XXX
resp_abs_or <- results_abs_df %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere") %>% 
    pull(OR)
resp_abs_ci_low <- results_abs_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Lower)
resp_abs_ci_high <- results_abs_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Upper)

# respiratory hybrid metric OR: XXX, 95\CI: XXX, XXX
resp_hyb_or <- results_hyb_df %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere") %>% 
    pull(OR)
resp_hyb_ci_low <- results_hyb_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Lower)
resp_hyb_ci_high <- results_hyb_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Upper)
    
# respiratory interaction term absolute metric OR: XXX, 95\CI: XXX, XXX
## NOTE MAY NEED TO CHANGE THIS TO THE INTERACTION TERM
resp_int_abs_or <- results_abs_df %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(OR)
resp_int_abs_ci_low <- results_abs_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(CI_Lower)
resp_int_abs_ci_high <- results_abs_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(CI_Upper)

# respiratory interaction term hybrid metric OR: XXX, 95\CI: XXX, XXX
resp_int_hyb_or <- results_hyb_df %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(OR)
resp_int_hyb_ci_low <- results_hyb_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(CI_Lower)
resp_int_hyb_ci_high <- results_hyb_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(CI_Upper)

# copd severe outage absolute metric OR: XXX, 95\CI: XXX, XXX
copd_abs_or <- results_abs_df %>% 
    filter(Cause == "COPD" & Exposure == "severity_customersSevere") %>% 
    pull(OR)
copd_abs_ci_low <- results_abs_df %>%
    filter(Cause == "COPD" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Lower)
copd_abs_ci_high <- results_abs_df %>%
    filter(Cause == "COPD" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Upper)

# copd severe outage hybrid metric OR: XXX, 95\CI: XXX, XXX
copd_hyb_or <- results_hyb_df %>% 
    filter(Cause == "COPD" & Exposure == "severity_customersSevere") %>% 
    pull(OR)
copd_hyb_ci_low <- results_hyb_df %>%
    filter(Cause == "COPD" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Lower)
copd_hyb_ci_high <- results_hyb_df %>%
    filter(Cause == "COPD" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Upper)


# write the numbers to a file -----------------------
all_vars <- ls()

# Function to format all numeric columns with commas and handle decimals appropriately
val_to_tex <- sapply(all_vars, function(var_name) {
  var_value <- get(var_name)
  # Only include numeric values
  if (is.numeric(var_value) && length(var_value) == 1) {
    # Check if the number is an integer (no decimal part)
    is_integer_like <- (var_value %% 1 == 0)
    
    if (is_integer_like) {
      # For integers, just add commas without decimal places
      formatted_value <- format(var_value, big.mark = ",", scientific = FALSE, nsmall = 0)
    } else {
      # For decimals, round to one decimal place
      rounded_value <- round(var_value, 1)
      formatted_value <- format(rounded_value, big.mark = ",", scientific = FALSE, nsmall = 1)
    }
    
    paste0("\\newcommand{\\", var_name, "}{", formatted_value, "}")
  } else {
    NULL
  }
})

values <- val_to_tex[!sapply(val_to_tex, is.null)]
write_lines(values, "analysis-values.tex")