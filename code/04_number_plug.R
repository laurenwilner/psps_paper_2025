#-------------------------------------------------
# PSPS: Paper number plugging
# June 2025
#-------------------------------------------------

# setup -------------------------------------------------
rm(list = ls()) # important in this script to get rid of existing objects! 
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(dplyr, readr, sf)

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/Results\ -\ June\ 2025/")
results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/oct_2025_results/case_crossover_results/")
exp_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/exposure_data/")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/")
data_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
analysis_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/data/")
code_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/code/")

# load functions -------------------------------------------------
source(paste0(code_dir, "00_helper_functions.R"))

# load data -------------------------------------------------
psps_exp_df <- read_csv(paste0(data_dir, 'ca_ZIP_daily_psps_no_washout_wf_classified_2013-2022.csv')) # updated
psps_exp_summary <- read_csv(paste0(analysis_dir, "daily_psps_binary.csv")) # updated
wf_exp_df <- read_csv(paste0(exp_dir, "zip_wfpm20132019.csv")) # no updates needed
results_abs_df <- read_csv(paste0(results_dir, "all_lag0_abs.csv")) # updated
results_hyb_df <- read_csv(paste0(results_dir, "all_lag0_hyb.csv")) # updated
cov_matrices <- readRDS("cov_matrices.rds") # updated
zip_shp <- st_read(paste0(exp_dir, "ca_zip.geojson")) %>% 
            rename(zip_code = ZIP_CODE) %>%
            select(c("zip_code", "geometry")) # no update needed 
ca_zips <- zip_shp$zip_code %>% unique() # no update needed
combined_exp_df <- read_csv(paste0(exp_dir, "zip_daily_psps_wf_exposure.csv")) # updated
zips_in_analysis <- read.csv(paste0(results_dir, "../zipcodes_in_analysis_by_endpoint.csv"))
# exposure_summary_abs_df <- read.csv(paste0(results_dir, "absexp_summary_byOOI.csv")) # updated
# exposure_summary_hybrid_df <- read.csv(paste0(results_dir, "hybexp_summary_byOOI.csv")) # updated
# exp_abs_sm_df <- read.csv(paste0(results_dir, "absexp_summary.csv")) # NEED TO UPDATE, USING OLD ONES FOR NOW
# exp_hyb_sm_df <- read.csv(paste0(results_dir, "hybexp_summary.csv")) # NEED TO UPDATE, USING OLD ONES FOR NOW
exp_summary <- read_csv(paste0(results_dir, "../psps_among_casedays.csv"))
ha_ed_table_df <- read_csv(paste0(results_dir, "../summary\ of\ events\ across\ data\ cleaning\ process_all_years.csv"))
wf_among_casedays <- read_csv(paste0(results_dir, "../wf_among_casedays.csv"))


# run the process results function to get combined ORs 
severe_df_abs <- process_results("Severe", results_abs_df, "abs", cov_matrices)
severe_df_hyb <- process_results("Severe", results_hyb_df, "hyb", cov_matrices)


# create each number to plug as a var -----------------------
# we included XXXX PSPS events in this study
npspsevents <- length(unique(psps_exp_df$psps_event_id))

# median duration of a PSPS event was XX hours
medianduration <- median(psps_exp_df$duration, na.rm = TRUE) %>% round(0)

# XXXX zip code -days in our 7-year study period that experienced PSPS events
zipdays <- nrow(psps_exp_summary)

# each zip code experienced, on average, XXXX events
zipevents <- psps_exp_summary %>% group_by(zip_code) %>% 
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
zipcodeswithevents <- psps_exp_summary %>% 
  group_by(zip_code) %>% 
  summarise(n_events = n()) %>% 
  filter(n_events > 0) %>% 
  nrow()

cazipcodes <- length(ca_zips)

# XXX zip code-days of wildfire smoke \PM
zipcodedayswf <- wf_exp_df %>% 
  filter(mean_lag05_per10 > 0) %>%
  group_by(zip_code) %>% 
  summarise(n_days = n()) %>% 
  summarise(total_days = sum(n_days, na.rm = TRUE)) %>% 
  pull(total_days)

# percent zip code-days with wildfire smoke \PM
percentzipcodedayswf <- (zipcodedayswf / zipdays) * 100

# YYY zip code-days of co-occurring wildfire smoke
zipdaysdualexp <- combined_exp_df %>% 
    filter(wf > 0 & psps_event > 0) %>%
    group_by(zip_code, date) %>%
    summarise(n_days = n()) %>%
    ungroup() %>%
    mutate(total_days = sum(n_days, na.rm = TRUE)) %>%
    pull(total_days) %>% unique()

# mean wildfire smoke \PM across the study period was XXXX
meanwfpm <- wf_exp_df %>% 
  filter(mean_lag05_per10 > 0) %>% 
  summarise(mean_pm = mean(mean_lag05_per10, na.rm = TRUE)) %>% 
  pull(mean_pm)
  
# XXX case-days (absolute, all severities)
casedaysabs <- exp_summary %>% 
    filter(severity_customers != 'none') %>%
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

# XXX control-days (absolute, all severities)
# controldaysabs <- exp_abs_sm_df %>% 
#     filter(case_indicator == 0 & severity_customers != 'none') %>%
#     mutate(n = sum(count)) %>% 
#     pull(n) %>% unique()

# XXX case-days (hybrid, all severities)
casedayshyb <- exp_summary %>% 
    filter(severity_hybrid != 'none') %>%
    mutate(n = sum(severity_hybrid_N)) %>% 
    pull(n) %>% unique()

# XXX control-days (hybrid, all severities)
# controldayshyb <- exp_hyb_sm_df %>% 
#     filter(case_indicator == 0 & severity_hybrid != 'none') %>%
#     mutate(n = sum(count)) %>% 
#     pull(n) %>% unique()

# XXXX cardiovascular case-days during a severe outage and XXXX respiratory case-days
casedaysabssevcvd <- exp_summary %>% 
    filter(severity_customers == "Severe" & outcome == "adult_cardio") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

casedaysabssevresp <- exp_summary %>%
    filter(severity_customers == "Severe" & outcome == "adult_resp") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

# greatest number of encounters associated with the XXX exposure level, with XXXX {cause} case-days during a severe outage and XXXX {cause} case-days
 highestencountersexplevel <- exp_summary %>% 
    filter(severity_customers != "none") %>%
    group_by(severity_customers) %>% 
    summarise(n = sum(severity_customers_N)) %>% 
    arrange(desc(n)) %>% 
    slice(1) %>% 
    pull(severity_customers)

respcasedayssev <- exp_summary %>% 
    filter(severity_customers == highestencountersexplevel & outcome == "adult_resp") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

cvdcasedayssev <- exp_summary %>%
    filter(severity_customers == highestencountersexplevel & outcome == "adult_cardio") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

psychcasedayssev <- exp_summary %>%
    filter(severity_customers == highestencountersexplevel & outcome == "adult_psych") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()


## RESP RESULTS
# respiratory absolute metric OR: XXX, 95\CI: XXX, XXX
respabsor <- results_abs_df %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere") %>% 
    pull(OR)
respabscilow <- results_abs_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Lower)
respabscihigh <- results_abs_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Upper)

# respiratory hybrid metric OR: XXX, 95\CI: XXX, XXX
resphybor <- results_hyb_df %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_hybridSevere") %>% 
    pull(OR)
resphybcilow <- results_hyb_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybridSevere") %>% 
    pull(CI_Lower)
resphybcihigh <- results_hyb_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybridSevere") %>% 
    pull(CI_Upper)
    
# respiratory interaction term absolute metric OR: XXX, 95\CI: XXX, XXX
respintabsor <- results_abs_df %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(OR)
respintabscilow <- results_abs_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(CI_Lower)
respintabscihigh <- results_abs_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(CI_Upper)

# respiratory interaction term hybrid metric OR: XXX, 95\CI: XXX, XXX
respinthybor <- results_hyb_df %>% 
    filter(Cause == "Respiratory" & Exposure == "severity_hybridSevere.mean_lag05_per10") %>% 
    pull(OR)
respinthybcilow <- results_hyb_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybridSevere.mean_lag05_per10") %>% 
    pull(CI_Lower)
respinthybcihigh <- results_hyb_df %>%
    filter(Cause == "Respiratory" & Exposure == "severity_hybridSevere.mean_lag05_per10") %>% 
    pull(CI_Upper)

# respiratory combined effect absolute metric OR: XXX, 95\CI: XXX, XXX
respcombabsor <- severe_df_abs %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
respcombabsperc <- round((respcombabsor - 1) * 100, 0) # convert to percent
respcombabslow <- severe_df_abs %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
respcombabshigh <- severe_df_abs %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

# respiratory combined effect hybrid metric OR: XXX, 95\CI: XXX, XXX
respcombhybor <- severe_df_hyb %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
respcombhybperc <- round((respcombhybor - 1) * 100, 0) # convert to percent
respcombhyblow <- severe_df_hyb %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
respcombhybhigh <- severe_df_hyb %>% 
    filter(Cause == "Respiratory" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

## COPD RESULTS
# copd severe outage absolute metric OR: XXX, 95\CI: XXX, XXX
copdabsor <- results_abs_df %>% 
    filter(Cause == "COPD" & Exposure == "severity_customersSevere") %>% 
    pull(OR)
copdabscilow <- results_abs_df %>%
    filter(Cause == "COPD" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Lower)
copdabscihigh <- results_abs_df %>%
    filter(Cause == "COPD" & Exposure == "severity_customersSevere") %>% 
    pull(CI_Upper)

# copd severe outage hybrid metric OR: XXX, 95\CI: XXX, XXX
copdhybor <- results_hyb_df %>% 
    filter(Cause == "COPD" & Exposure == "severity_hybridSevere") %>% 
    pull(OR)
copdhybcilow <- results_hyb_df %>%
    filter(Cause == "COPD" & Exposure == "severity_hybridSevere") %>% 
    pull(CI_Lower)
copdhybcihigh <- results_hyb_df %>%
    filter(Cause == "COPD" & Exposure == "severity_hybridSevere") %>% 
    pull(CI_Upper)

# copd interaction term absolute metric OR: XXX, 95\CI: XXX, XXX
copdintabsor <- results_abs_df %>% 
    filter(Cause == "COPD" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(OR)
copdintabscilow <- results_abs_df %>%
    filter(Cause == "COPD" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(CI_Lower)
copdintabscihigh <- results_abs_df %>%
    filter(Cause == "COPD" & Exposure == "severity_customersSevere.mean_lag05_per10") %>% 
    pull(CI_Upper)

# copd interaction term hybrid metric OR: XXX, 95\CI: XXX, XXX
copdinthybor <- results_hyb_df %>% 
    filter(Cause == "COPD" & Exposure == "severity_hybridSevere.mean_lag05_per10") %>% 
    pull(OR)
copdinthybcilow <- results_hyb_df %>%
    filter(Cause == "COPD" & Exposure == "severity_hybridSevere.mean_lag05_per10") %>% 
    pull(CI_Lower)
copdinthybcihigh <- results_hyb_df %>%
    filter(Cause == "COPD" & Exposure == "severity_hybridSevere.mean_lag05_per10") %>% 
    pull(CI_Upper)

# copd combined effect absolute metric OR: XXX, 95\CI: XXX, XXX
copdcombabsor <- severe_df_abs %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
copdcombabslow <- severe_df_abs %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
copdcombabshigh <- severe_df_abs %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

# copd combined effect hybrid metric OR: XXX, 95\CI: XXX, XXX
copdcombhybor <- severe_df_hyb %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(odds_ratio)
copdcombhyblow <- severe_df_hyb %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(lower_ci)
copdcombhybhigh <- severe_df_hyb %>% 
    filter(Cause == "COPD" & Exposure == "Severe PSPS event * WF smoke (combined)") %>% 
    pull(upper_ci)

# WFS absolute metric OR (resp, also doesnt matter re abs or hyb so just pulling one): XXX, 95\CI: XXX, XXX
wfsabsor <- results_abs_df %>%
    filter(Cause == "Respiratory" & Exposure == "mean_lag05_per10") %>% 
    pull(OR)
wfsabsperc <- round((wfsabsor - 1) * 100, 0) # convert to percent
wfsabscilow <- results_abs_df %>%
    filter(Cause == "Respiratory" & Exposure == "mean_lag05_per10") %>% 
    pull(CI_Lower)
wfsabscihigh <- results_abs_df %>%
    filter(Cause == "Respiratory" & Exposure == "mean_lag05_per10") %>% 
    pull(CI_Upper)

# Of note, XX\% of outages were over 8 hours, so there was little concern about an overabundance of short outages.
percentlongoutages <- ((nrow(psps_exp_df %>% filter(duration > 8))/nrow(psps_exp_df)) * 100 )%>% round(1)

# We identified \pspsexpresp respiratory, \pspsexpcopd COPD, \pspsexpcardio cardiovascular, \pspsexppsych psychiatric index days exposed to a PSPS event (mild, moderate, or severe) (table \ref{exposure_table}). 
pspsexpresp <- exp_summary %>% 
    filter(severity_customers != "none" & outcome == "adult_resp") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()
pspsexpcopd <- exp_summary %>% 
    filter(severity_customers != "none" & outcome == "adult_copd") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()
pspsexpcardio <- exp_summary %>% 
    filter(severity_customers != "none" & outcome == "adult_cardio") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()
pspsexppsych <- exp_summary %>% 
    filter(severity_customers != "none" & outcome == "adult_psych") %>% 
    mutate(n = sum(severity_customers_N)) %>% 
    pull(n) %>% unique()

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

# The mean daily wildfire \PM concentration across the study period was \meanwfpm{} \SI[per-mode=symbol]{10}{\micro\gram\per\cubic\metre}: 
# mean wf during psps event, mean wf during no psps event
meanwfduringpsps <- wf_exp_df %>% 
    left_join(psps_exp_summary, by = c("date", "zip_code")) %>% 
    mutate(wf = ifelse(is.na(wf), 0, wf)) %>% 
    select(c("psps_abs", "mean_lag05_per10", "date", "zip_code")) %>% 
    filter(psps_abs == 1) %>% 
    mutate(meanwfduringpsps = mean(mean_lag05_per10, na.rm = TRUE)) %>% 
    pull(meanwfduringpsps) %>% unique()

meanwfduringnopsps <- wf_exp_df %>% 
    left_join(psps_exp_summary, by = c("date", "zip_code")) %>% 
    mutate(wf = ifelse(is.na(wf), 0, wf)) %>% 
    select(c("psps_abs", "mean_lag05_per10", "date", "zip_code")) %>% 
    filter(is.na(psps_abs)) %>% 
    mutate(meanwfduringnopsps = mean(mean_lag05_per10, na.rm = TRUE)) %>% 
    pull(meanwfduringnopsps) %>% unique()



# write the numbers to a file -----------------------
all_vars <- ls()

# filter out any variables that contain "dir" in their name
all_vars <- all_vars[!grepl("dir", all_vars, ignore.case = TRUE)]

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
    
    paste0("\\newcommand{\\", var_name, "}{", formatted_value, "}")
  }
  # handle character values
  else if (is.character(var_value) && length(var_value) == 1) {
    paste0("\\newcommand{\\", var_name, "}{", var_value, "}")
  } else {
    NULL
  }
})

values <- val_to_tex[!sapply(val_to_tex, is.null)]
write_lines(values, "analysis-values.tex")

