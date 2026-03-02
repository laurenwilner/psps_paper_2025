#-------------------------------------------------
# PSPS: Paper number plugging
# June 2025 (updated Feb 2026 for lag-specific results)
#-------------------------------------------------

# setup -------------------------------------------------
rm(list = ls()) # important in this script to get rid of existing objects! 
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(dplyr, readr, sf)

# Bootstrap: source paths.R (edit path in paths.R when moving machines)
args0 <- commandArgs(trailingOnly = FALSE)
file0 <- grep("^--file=", args0, value = TRUE)
if (length(file0) > 0) {
  source(file.path(dirname(normalizePath(sub("^--file=", "", file0))), "paths.R"))
} else {
  source(file.path(path.expand("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025"), "code", "paths.R"))
}

# analysis-values.tex goes to project root
out_dir <- project_root

# load functions -------------------------------------------------
source(file.path(code_dir, "00_helper_functions.R"))

# load data -------------------------------------------------
psps_exp_df <- read_csv(file.path(data_dir, "ca_ZIP_daily_psps_no_washout_wf_classified_2013-2022.csv"), show_col_types = FALSE)
wf_exp_df <- read_csv(file.path(exp_dir, "zip_wfpm20132019.csv"), show_col_types = FALSE)

# Load results - now with lag_type and age_group columns
results_abs_df <- read_csv(file.path(jan2026_results_dir, "all_results_abs_jan2026.csv"), show_col_types = FALSE)
results_hyb_df <- read_csv(file.path(jan2026_results_dir, "all_results_hyb_jan2026.csv"), show_col_types = FALSE)
cov_matrices <- readRDS(file.path(jan2026_results_dir, "cov_matrices_jan2026.rds"))

zip_shp <- st_read(file.path(exp_dir, "ca_zip.geojson"), quiet = TRUE) %>% 
            rename(zip_code = ZIP_CODE) %>%
            select(c("zip_code", "geometry"))
ca_zips <- zip_shp$zip_code %>% unique()
combined_exp_df <- read_csv(file.path(exp_dir, "zip_daily_psps_wf_exposure.csv"), show_col_types = FALSE)
zips_in_analysis <- read.csv(file.path(jan2026_results_dir, "zipcodes_in_analysis_by_endpoint.csv"))

# Load lag-specific exposure summaries
exp_summary_same_day <- read_csv(file.path(jan2026_results_dir, "psps_among_casedays_same_day.csv"), show_col_types = FALSE)
exp_summary_lag4 <- read_csv(file.path(jan2026_results_dir, "psps_among_casedays_lag4.csv"), show_col_types = FALSE) %>%
  rename(
    severity_customers = severity_customers_lag4,
    severity_customers_N = severity_customers_lag4_N,
    severity_hybrid = severity_hybrid_lag4,
    severity_hybrid_N = severity_hybrid_lag4_N
  )

ha_ed_table_df <- read_csv(file.path(jan2026_results_dir, "summary of events across data cleaning process_all_years.csv"), show_col_types = FALSE)

# Filter results for age 20 and older (main analysis)
results_abs_same_day <- results_abs_df %>% filter(lag_type == "same_day" & age_group == "age 20 and older")
results_abs_lag4 <- results_abs_df %>% filter(lag_type == "lag4" & age_group == "age 20 and older")
results_hyb_same_day <- results_hyb_df %>% filter(lag_type == "same_day" & age_group == "age 20 and older")
results_hyb_lag4 <- results_hyb_df %>% filter(lag_type == "lag4" & age_group == "age 20 and older")

# run the process results function to get combined ORs for both lag types
severe_df_abs_same_day <- process_results("Severe", results_abs_same_day, "abs", cov_matrices, lag_type = "same_day", age_group = "age 20 and older")
severe_df_abs_lag4 <- process_results("Severe", results_abs_lag4, "abs", cov_matrices, lag_type = "lag4", age_group = "age 20 and older")
severe_df_hyb_same_day <- process_results("Severe", results_hyb_same_day, "hyb", cov_matrices, lag_type = "same_day", age_group = "age 20 and older")
severe_df_hyb_lag4 <- process_results("Severe", results_hyb_lag4, "hyb", cov_matrices, lag_type = "lag4", age_group = "age 20 and older")


# create each number to plug as a var -----------------------
# ============================================================
# SECTION 1: NUMBERS THAT DON'T CHANGE BY LAG TYPE
# ============================================================

# we included XXXX PSPS events in this study
npspsevents <- length(unique(psps_exp_df$psps_event_id))

# median duration of a PSPS event was XX hours
medianduration <- median(psps_exp_df$duration, na.rm = TRUE) %>% round(0)

# XXXX zip code -days in our 7-year study period that experienced PSPS events
zipdays <- combined_exp_df %>% 
  filter(psps_event == 1) %>% 
  group_by(zip_code) %>% 
  summarise(n_days = n()) %>% 
  pull(n_days) %>% sum()

# each zip code experienced, on average, XXXX events
zipevents <- combined_exp_df %>% 
  filter(psps_event == 1) %>%
  group_by(zip_code) %>% 
  summarise(n_events = n()) %>% 
  summarise(mean_events = mean(n_events, na.rm = TRUE)) %>% 
  pull(mean_events) %>% round(0)

# XXXX {severity level} was the most common when we used our absolute metric
abs_severity_df <- psps_exp_df %>% 
  group_by(severity_customers) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
mostcommonsevabs <- abs_severity_df[1,]$severity_customers %>% tolower()
secondmostcommonsevabs <- abs_severity_df[2,]$severity_customers %>% tolower()

# XXXX {severity level} was most common according to our relative metric
hyb_severity_df <- psps_exp_df %>% 
  group_by(severity_hybrid) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

mostcommonsevhyb <- hyb_severity_df[1,]$severity_hybrid %>% tolower()
secondmostcommonsevhyb <- hyb_severity_df[2,]$severity_hybrid %>% tolower()

# XXXX of the YYYY zip codes in California experienced at least one PSPS event
zipcodeswithevents <- combined_exp_df %>% 
  filter(psps_event == 1) %>%
  group_by(zip_code) %>% 
  summarise(n_events = n()) %>% 
  filter(n_events > 0) %>% 
  nrow()

cazipcodes <- length(ca_zips)

# XXX zip code-days of wildfire smoke \PM
zipcodedayswf <- wf_exp_df %>% 
  filter(mean_lag05_per10 > 0) %>%
  group_by(zip_code) %>% 
  summarise(n_days = n()) %>% pull(n_days) %>% sum()
zipdays_tot <- wf_exp_df %>% group_by(zip_code) %>% summarise(n_days = n()) %>% pull(n_days) %>% sum()

# percent zip code-days with wildfire smoke \PM
percentzipcodedayswf <- round((zipcodedayswf / zipdays_tot) * 100, 1)

# YYY zip code-days of co-occurring wildfire smoke
zipdaysdualexp <- combined_exp_df %>% 
    filter(wf > 0 & psps_event > 0) %>%
    group_by(zip_code, date) %>%
    summarise(n_days = n(), .groups = "drop") %>%
    ungroup() %>%
    mutate(total_days = sum(n_days, na.rm = TRUE)) %>%
    pull(total_days) %>% unique()

# mean wildfire smoke \PM across the study period was XXXX
meanwfpm <- wf_exp_df %>% 
  filter(mean_lag05_per10 > 0) %>% 
  summarise(mean_pm = mean(mean_lag05_per10, na.rm = TRUE)) %>% 
  pull(mean_pm)

# Of note, XX\% of outages were over 8 hours, so there was little concern about an overabundance of short outages.
percentlongoutages <- ((nrow(psps_exp_df %>% filter(duration > 8))/nrow(psps_exp_df)) * 100 )%>% round(1)

# number of zip codes in analysis
nzipcodesinanalysis <- length(unique(zips_in_analysis$ZIP_CODE))

# zips by endpoint
nzipcodesresp <- nzipcodesinanalysis - sum(zips_in_analysis$respmissing == "Yes", na.rm = TRUE)
nzipcodescopd <- nzipcodesinanalysis - sum(zips_in_analysis$copdmissing == "Yes", na.rm = TRUE)
nzipcodescardio <- nzipcodesinanalysis - sum(zips_in_analysis$cardiomissing == "Yes", na.rm = TRUE)
nzipcodespsych <- nzipcodesinanalysis - sum(zips_in_analysis$psychmissing == "Yes", na.rm = TRUE)

# X\% of overall visits were ED and Y\% were hospital admissions
ha_ed_table_calc <- ha_ed_table_df %>% 
    mutate(total_ed = sum(ed_final, na.rm = TRUE)) %>% 
    mutate(total_ha = sum(pdd_final, na.rm = TRUE))
edperc <- ha_ed_table_calc %>% 
    mutate(ed_perc = (total_ed / (total_ed + total_ha)) * 100) %>% 
    pull(ed_perc) %>% unique() %>% round(1)
haperc <- ha_ed_table_calc %>% 
    mutate(ha_perc = (total_ha / (total_ed + total_ha)) * 100) %>% 
    pull(ha_perc) %>% unique() %>% round(1)

# When a patient's zip code was missing, we used the zip code of the hospital (n=XX cases)
missingzipcases <- ha_ed_table_df %>% 
    mutate(total_missing_zip_pdd = sum(pdd_dif, na.rm = TRUE),
            total_missing_zip_ed = sum(ed_dif, na.rm = TRUE),
            total_missing_zip = total_missing_zip_pdd + total_missing_zip_ed) %>% 
    pull(total_missing_zip) %>% unique()
overalltotal <- ha_ed_table_df %>% 
    mutate(total_nonmissing_zip_pdd = sum(pdd_final, na.rm = TRUE),
            total_nonmissing_zip_ed = sum(ed_final, na.rm = TRUE),
            total_nonmissing_zip = total_nonmissing_zip_pdd + total_nonmissing_zip_ed) %>% 
    pull(total_nonmissing_zip) %>% unique()

percmissingzip <- round((missingzipcases / overalltotal) * 100, 2)

# The mean daily wildfire \PM concentration across the study period was \meanwfpm{} 
# mean wf during psps event, mean wf during no psps event
meanwfduringpsps <- combined_exp_df %>% 
    filter(psps_event == 1) %>% 
    summarise(meanwf = mean(wf, na.rm = TRUE)) %>% 
    pull(meanwf)

meanwfduringnopsps <- combined_exp_df %>% 
    filter(psps_event == 0) %>% 
    summarise(meanwf = mean(wf, na.rm = TRUE)) %>% 
    pull(meanwf)


# ============================================================
# SECTION 2: LAG-SPECIFIC NUMBERS (SAME DAY)
# ============================================================

# --- Case days by exposure (SAME DAY) ---
# XXX case-days (absolute, all severities)
casedaysabs_same_day <- exp_summary_same_day %>% 
    filter(severity_customers != 'none') %>%
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

# XXX case-days (hybrid, all severities)
casedayshyb_same_day <- exp_summary_same_day %>% 
    filter(severity_hybrid != 'none') %>%
    mutate(n = sum(severity_hybrid_N)) %>% 
    pull(n) %>% unique()

# XXXX cardiovascular case-days during a severe outage and XXXX respiratory case-days
casedaysabssevcvd_same_day <- exp_summary_same_day %>% 
    filter(severity_customers == "severe" & outcome == "cardio") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

casedaysabssevresp_same_day <- exp_summary_same_day %>%
    filter(severity_customers == "severe" & outcome == "resp") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

# greatest number of encounters associated with the XXX exposure level
highestencountersexplevel_same_day <- exp_summary_same_day %>% 
    filter(severity_customers != "none") %>%
    group_by(severity_customers) %>% 
    summarise(n = sum(severity_customers_N)) %>% 
    arrange(desc(n)) %>% 
    slice(1) %>% 
    pull(severity_customers)

respcasedayssev_same_day <- exp_summary_same_day %>% 
    filter(severity_customers == highestencountersexplevel_same_day & outcome == "resp") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

cvdcasedayssev_same_day <- exp_summary_same_day %>%
    filter(severity_customers == highestencountersexplevel_same_day & outcome == "cardio") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

psychcasedayssev_same_day <- exp_summary_same_day %>%
    filter(severity_customers == highestencountersexplevel_same_day & outcome == "psych") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

# PSPS exposed index days by outcome (SAME DAY)
pspsexpresp_same_day <- exp_summary_same_day %>% 
    filter(severity_customers != "none" & outcome == "resp") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()
pspsexpcopd_same_day <- exp_summary_same_day %>% 
    filter(severity_customers != "none" & outcome == "copd") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()
pspsexpcardio_same_day <- exp_summary_same_day %>% 
    filter(severity_customers != "none" & outcome == "cardio") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()
pspsexppsych_same_day <- exp_summary_same_day %>% 
    filter(severity_customers != "none" & outcome == "psych") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

# --- RESULTS (SAME DAY) ---
## RESP RESULTS (SAME DAY)
# respiratory absolute metric OR
respabsor_same_day <- results_abs_same_day %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_customerssevere") %>% 
    pull(OR)
respabscilow_same_day <- results_abs_same_day %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customerssevere") %>% 
    pull(CI_Lower)
respabscihigh_same_day <- results_abs_same_day %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customerssevere") %>% 
    pull(CI_Upper)

# respiratory hybrid metric OR
resphybor_same_day <- results_hyb_same_day %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_hybridsevere") %>% 
    pull(OR)
resphybcilow_same_day <- results_hyb_same_day %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybridsevere") %>% 
    pull(CI_Lower)
resphybcihigh_same_day <- results_hyb_same_day %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybridsevere") %>% 
    pull(CI_Upper)
    
# respiratory interaction term absolute metric OR
respintabsor_same_day <- results_abs_same_day %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_customerssevere.wf_pm25_per10") %>% 
    pull(OR)
respintabscilow_same_day <- results_abs_same_day %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customerssevere.wf_pm25_per10") %>% 
    pull(CI_Lower)
respintabscihigh_same_day <- results_abs_same_day %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customerssevere.wf_pm25_per10") %>% 
    pull(CI_Upper)

# respiratory interaction term hybrid metric OR
respinthybor_same_day <- results_hyb_same_day %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_hybridsevere.wf_pm25_per10") %>% 
    pull(OR)
respinthybcilow_same_day <- results_hyb_same_day %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybridsevere.wf_pm25_per10") %>% 
    pull(CI_Lower)
respinthybcihigh_same_day <- results_hyb_same_day %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybridsevere.wf_pm25_per10") %>% 
    pull(CI_Upper)

# respiratory combined effect absolute metric OR
respcombabsor_same_day <- severe_df_abs_same_day %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
respcombabsperc_same_day <- round((respcombabsor_same_day - 1) * 100, 0)
respcombabslow_same_day <- severe_df_abs_same_day %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
respcombabshigh_same_day <- severe_df_abs_same_day %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

# respiratory combined effect hybrid metric OR
respcombhybor_same_day <- severe_df_hyb_same_day %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
respcombhybperc_same_day <- round((respcombhybor_same_day - 1) * 100, 0)
respcombhyblow_same_day <- severe_df_hyb_same_day %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
respcombhybhigh_same_day <- severe_df_hyb_same_day %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

## COPD RESULTS (SAME DAY)
copdabsor_same_day <- results_abs_same_day %>% 
    filter(Cause == "COPD" & Exposure == "severity_customerssevere") %>% 
    pull(OR)
copdabscilow_same_day <- results_abs_same_day %>%
    filter(Cause == "COPD" & Exposure == "severity_customerssevere") %>% 
    pull(CI_Lower)
copdabscihigh_same_day <- results_abs_same_day %>%
    filter(Cause == "COPD" & Exposure == "severity_customerssevere") %>% 
    pull(CI_Upper)

copdhybor_same_day <- results_hyb_same_day %>% 
    filter(Cause == "COPD" & Exposure == "severity_hybridsevere") %>% 
    pull(OR)
copdhybcilow_same_day <- results_hyb_same_day %>%
    filter(Cause == "COPD" & Exposure == "severity_hybridsevere") %>% 
    pull(CI_Lower)
copdhybcihigh_same_day <- results_hyb_same_day %>%
    filter(Cause == "COPD" & Exposure == "severity_hybridsevere") %>% 
    pull(CI_Upper)

copdintabsor_same_day <- results_abs_same_day %>% 
    filter(Cause == "COPD" & Exposure == "severity_customerssevere.wf_pm25_per10") %>% 
    pull(OR)
copdintabscilow_same_day <- results_abs_same_day %>%
    filter(Cause == "COPD" & Exposure == "severity_customerssevere.wf_pm25_per10") %>% 
    pull(CI_Lower)
copdintabscihigh_same_day <- results_abs_same_day %>%
    filter(Cause == "COPD" & Exposure == "severity_customerssevere.wf_pm25_per10") %>% 
    pull(CI_Upper)

copdinthybor_same_day <- results_hyb_same_day %>% 
    filter(Cause == "COPD" & Exposure == "severity_hybridsevere.wf_pm25_per10") %>% 
    pull(OR)
copdinthybcilow_same_day <- results_hyb_same_day %>%
    filter(Cause == "COPD" & Exposure == "severity_hybridsevere.wf_pm25_per10") %>% 
    pull(CI_Lower)
copdinthybcihigh_same_day <- results_hyb_same_day %>%
    filter(Cause == "COPD" & Exposure == "severity_hybridsevere.wf_pm25_per10") %>% 
    pull(CI_Upper)

copdcombabsor_same_day <- severe_df_abs_same_day %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
copdcombabslow_same_day <- severe_df_abs_same_day %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
copdcombabshigh_same_day <- severe_df_abs_same_day %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

copdcombhybor_same_day <- severe_df_hyb_same_day %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
copdcombhyblow_same_day <- severe_df_hyb_same_day %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
copdcombhybhigh_same_day <- severe_df_hyb_same_day %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

# WFS absolute metric OR (resp) (SAME DAY)
wfsabsor_same_day <- results_abs_same_day %>%
    filter(Cause == "Respiratory" & Exposure == "wf_pm25_per10") %>% 
    pull(OR)
wfsabsperc_same_day <- round((wfsabsor_same_day - 1) * 100, 0)
wfsabscilow_same_day <- results_abs_same_day %>%
    filter(Cause == "Respiratory" & Exposure == "wf_pm25_per10") %>% 
    pull(CI_Lower)
wfsabscihigh_same_day <- results_abs_same_day %>%
    filter(Cause == "Respiratory" & Exposure == "wf_pm25_per10") %>% 
    pull(CI_Upper)


# ============================================================
# SECTION 3: LAG-SPECIFIC NUMBERS (4-DAY LAG)
# ============================================================

# --- Case days by exposure (LAG4) ---
casedaysabs_lag4 <- exp_summary_lag4 %>% 
    filter(severity_customers != 'none') %>%
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

casedayshyb_lag4 <- exp_summary_lag4 %>% 
    filter(severity_hybrid != 'none') %>%
    mutate(n = sum(severity_hybrid_N)) %>% 
    pull(n) %>% unique()

casedaysabssevcvd_lag4 <- exp_summary_lag4 %>% 
    filter(severity_customers == "severe" & outcome == "cardio") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

casedaysabssevresp_lag4 <- exp_summary_lag4 %>%
    filter(severity_customers == "severe" & outcome == "resp") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

highestencountersexplevel_lag4 <- exp_summary_lag4 %>% 
    filter(severity_customers != "none") %>%
    group_by(severity_customers) %>% 
    summarise(n = sum(severity_customers_N)) %>% 
    arrange(desc(n)) %>% 
    slice(1) %>% 
    pull(severity_customers)

respcasedayssev_lag4 <- exp_summary_lag4 %>% 
    filter(severity_customers == highestencountersexplevel_lag4 & outcome == "resp") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

cvdcasedayssev_lag4 <- exp_summary_lag4 %>%
    filter(severity_customers == highestencountersexplevel_lag4 & outcome == "cardio") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

psychcasedayssev_lag4 <- exp_summary_lag4 %>%
    filter(severity_customers == highestencountersexplevel_lag4 & outcome == "psych") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

# PSPS exposed index days by outcome (LAG4)
pspsexpresp_lag4 <- exp_summary_lag4 %>% 
    filter(severity_customers != "none" & outcome == "resp") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()
pspsexpcopd_lag4 <- exp_summary_lag4 %>% 
    filter(severity_customers != "none" & outcome == "copd") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()
pspsexpcardio_lag4 <- exp_summary_lag4 %>% 
    filter(severity_customers != "none" & outcome == "cardio") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()
pspsexppsych_lag4 <- exp_summary_lag4 %>% 
    filter(severity_customers != "none" & outcome == "psych") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

# --- RESULTS (LAG4) ---
## RESP RESULTS (LAG4)
respabsor_lag4 <- results_abs_lag4 %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_customers_lag4severe") %>% 
    pull(OR)
respabscilow_lag4 <- results_abs_lag4 %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customers_lag4severe") %>% 
    pull(CI_Lower)
respabscihigh_lag4 <- results_abs_lag4 %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customers_lag4severe") %>% 
    pull(CI_Upper)

resphybor_lag4 <- results_hyb_lag4 %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_hybrid_lag4severe") %>% 
    pull(OR)
resphybcilow_lag4 <- results_hyb_lag4 %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybrid_lag4severe") %>% 
    pull(CI_Lower)
resphybcihigh_lag4 <- results_hyb_lag4 %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybrid_lag4severe") %>% 
    pull(CI_Upper)
    
respintabsor_lag4 <- results_abs_lag4 %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_customers_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(OR)
respintabscilow_lag4 <- results_abs_lag4 %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customers_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(CI_Lower)
respintabscihigh_lag4 <- results_abs_lag4 %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customers_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(CI_Upper)

respinthybor_lag4 <- results_hyb_lag4 %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_hybrid_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(OR)
respinthybcilow_lag4 <- results_hyb_lag4 %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybrid_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(CI_Lower)
respinthybcihigh_lag4 <- results_hyb_lag4 %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybrid_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(CI_Upper)

respcombabsor_lag4 <- severe_df_abs_lag4 %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
respcombabsperc_lag4 <- round((respcombabsor_lag4 - 1) * 100, 0)
respcombabslow_lag4 <- severe_df_abs_lag4 %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
respcombabshigh_lag4 <- severe_df_abs_lag4 %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

respcombhybor_lag4 <- severe_df_hyb_lag4 %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
respcombhybperc_lag4 <- round((respcombhybor_lag4 - 1) * 100, 0)
respcombhyblow_lag4 <- severe_df_hyb_lag4 %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
respcombhybhigh_lag4 <- severe_df_hyb_lag4 %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

## COPD RESULTS (LAG4)
copdabsor_lag4 <- results_abs_lag4 %>% 
    filter(Cause == "COPD" & Exposure == "severity_customers_lag4severe") %>% 
    pull(OR)
copdabscilow_lag4 <- results_abs_lag4 %>%
    filter(Cause == "COPD" & Exposure == "severity_customers_lag4severe") %>% 
    pull(CI_Lower)
copdabscihigh_lag4 <- results_abs_lag4 %>%
    filter(Cause == "COPD" & Exposure == "severity_customers_lag4severe") %>% 
    pull(CI_Upper)

copdhybor_lag4 <- results_hyb_lag4 %>% 
    filter(Cause == "COPD" & Exposure == "severity_hybrid_lag4severe") %>% 
    pull(OR)
copdhybcilow_lag4 <- results_hyb_lag4 %>%
    filter(Cause == "COPD" & Exposure == "severity_hybrid_lag4severe") %>% 
    pull(CI_Lower)
copdhybcihigh_lag4 <- results_hyb_lag4 %>%
    filter(Cause == "COPD" & Exposure == "severity_hybrid_lag4severe") %>% 
    pull(CI_Upper)

copdintabsor_lag4 <- results_abs_lag4 %>% 
    filter(Cause == "COPD" & Exposure == "severity_customers_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(OR)
copdintabscilow_lag4 <- results_abs_lag4 %>%
    filter(Cause == "COPD" & Exposure == "severity_customers_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(CI_Lower)
copdintabscihigh_lag4 <- results_abs_lag4 %>%
    filter(Cause == "COPD" & Exposure == "severity_customers_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(CI_Upper)

copdinthybor_lag4 <- results_hyb_lag4 %>% 
    filter(Cause == "COPD" & Exposure == "severity_hybrid_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(OR)
copdinthybcilow_lag4 <- results_hyb_lag4 %>%
    filter(Cause == "COPD" & Exposure == "severity_hybrid_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(CI_Lower)
copdinthybcihigh_lag4 <- results_hyb_lag4 %>%
    filter(Cause == "COPD" & Exposure == "severity_hybrid_lag4severe.mean_lag0_lag3_per10") %>% 
    pull(CI_Upper)

copdcombabsor_lag4 <- severe_df_abs_lag4 %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
copdcombabslow_lag4 <- severe_df_abs_lag4 %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
copdcombabshigh_lag4 <- severe_df_abs_lag4 %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

copdcombhybor_lag4 <- severe_df_hyb_lag4 %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
copdcombhyblow_lag4 <- severe_df_hyb_lag4 %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
copdcombhybhigh_lag4 <- severe_df_hyb_lag4 %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

# WFS absolute metric OR (resp) (LAG4)
wfsabsor_lag4 <- results_abs_lag4 %>%
    filter(Cause == "Respiratory" & Exposure == "mean_lag0_lag3_per10") %>% 
    pull(OR)
wfsabsperc_lag4 <- round((wfsabsor_lag4 - 1) * 100, 0)
wfsabscilow_lag4 <- results_abs_lag4 %>%
    filter(Cause == "Respiratory" & Exposure == "mean_lag0_lag3_per10") %>% 
    pull(CI_Lower)
wfsabscihigh_lag4 <- results_abs_lag4 %>%
    filter(Cause == "Respiratory" & Exposure == "mean_lag0_lag3_per10") %>% 
    pull(CI_Upper)


# ============================================================
# SECTION 4: 1-WEEK DURATION SENSITIVITY ANALYSIS
# ============================================================

# Load 1-week duration results
results_resp_1week <- read_csv(file.path(jan2026_results_dir, "case_crossover_results", "results_resp_1week_duration_age 20 and older.csv"), show_col_types = FALSE)
results_copd_1week <- read_csv(file.path(jan2026_results_dir, "case_crossover_results", "results_copd_1week_duration_age 20 and older.csv"), show_col_types = FALSE)

# Respiratory 1-week duration interaction term
respintorOneWk <- results_resp_1week %>%
    filter(Exposure == "outage_hours_lag7_per8:mean_lag0_lag6_per10") %>%
    slice(1) %>%
    pull(OR)
respintcilowOneWk <- results_resp_1week %>%
    filter(Exposure == "outage_hours_lag7_per8:mean_lag0_lag6_per10") %>%
    slice(1) %>%
    pull(CI_Lower)
respintcihighOneWk <- results_resp_1week %>%
    filter(Exposure == "outage_hours_lag7_per8:mean_lag0_lag6_per10") %>%
    slice(1) %>%
    pull(CI_Upper)

# COPD 1-week duration interaction term
copdintorOneWk <- results_copd_1week %>%
    filter(Exposure == "outage_hours_lag7_per8:mean_lag0_lag6_per10") %>%
    slice(1) %>%
    pull(OR)
copdintcilowOneWk <- results_copd_1week %>%
    filter(Exposure == "outage_hours_lag7_per8:mean_lag0_lag6_per10") %>%
    slice(1) %>%
    pull(CI_Lower)
copdintcihighOneWk <- results_copd_1week %>%
    filter(Exposure == "outage_hours_lag7_per8:mean_lag0_lag6_per10") %>%
    slice(1) %>%
    pull(CI_Upper)


# ============================================================
# WRITE THE NUMBERS TO A FILE
# ============================================================
all_vars <- ls()

# filter out any variables that contain "dir" or "df" in their name
all_vars <- all_vars[!grepl("dir", all_vars, ignore.case = TRUE)]
all_vars <- all_vars[!grepl("project_root", all_vars, ignore.case = TRUE)]
all_vars <- all_vars[!grepl("_df$", all_vars, ignore.case = TRUE)]
all_vars <- all_vars[!grepl("^results_", all_vars, ignore.case = TRUE)]
all_vars <- all_vars[!grepl("^severe_df_", all_vars, ignore.case = TRUE)]
all_vars <- all_vars[!grepl("^exp_summary", all_vars, ignore.case = TRUE)]
all_vars <- all_vars[!grepl("_shp$", all_vars, ignore.case = TRUE)]
all_vars <- all_vars[!grepl("cov_matrices", all_vars, ignore.case = TRUE)]

# function to format all numeric columns with commas and handle decimals appropriately,
# and to also include character variables
val_to_tex <- sapply(all_vars, function(var_name) {
  var_value <- get(var_name)
  
  # handle numeric values
  if (is.numeric(var_value) && length(var_value) == 1) {
    # check if the number is an integer (no decimal part)
    is_integer_like <- (var_value %% 1 == 0)
    
    if (is_integer_like) {
      # for integers, just add commas without decimal places
      formatted_value <- format(var_value, big.mark = ",", scientific = FALSE, nsmall = 0)
    } else {
      # for decimals, round to one decimal place
      rounded_value <- round(var_value, 2)
      formatted_value <- format(rounded_value, big.mark = ",", scientific = FALSE, nsmall = 2)
    }

    # special handling for one percent value that i want just 1 decimal place for! 
    if (var_name == "percentlongoutages") {
      formatted_value <- rounded_value %>% round(1)
    }
    
    # Convert underscores to camelCase for LaTeX compatibility
    # e.g., respabsor_same_day -> respabsorSameDay
    latex_name <- gsub("_same_day", "SameDay", var_name)
    latex_name <- gsub("_lag4", "LagFour", latex_name)
    
    paste0("\\newcommand{\\", latex_name, "}{", formatted_value, "}")
  }
  # handle character values
  else if (is.character(var_value) && length(var_value) == 1) {
    # Convert underscores to camelCase for LaTeX compatibility
    latex_name <- gsub("_same_day", "SameDay", var_name)
    latex_name <- gsub("_lag4", "LagFour", latex_name)
    
    paste0("\\newcommand{\\", latex_name, "}{", var_value, "}")
  } else {
    NULL
  }
})

values <- val_to_tex[!sapply(val_to_tex, is.null)]
write_lines(values, file.path(out_dir, "analysis-values.tex"))

