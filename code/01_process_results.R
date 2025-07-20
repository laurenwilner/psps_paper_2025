#-------------------------------------------------
# PSPS: Process analytic results 
# March 2025
#-------------------------------------------------

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(tidyverse, ggforce, MetBrewer)

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/Results\ -\ June\ 2025/")
exp_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/exposure_data/")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/tables_figures/")
data_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
analysis_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/data/")

# read in and concat results to visualize -----------------------------
# pull directories of results and construct file names
files <- list.files(results_dir, full.names = TRUE)
files <- str_subset(files, "results_.*\\.csv$")

# read in and create a column for the file name
results <- data.frame()
for(f in files){
    print(f)
    df <- read_csv(f)

    # drop random row name cols
    df <- df %>% 
        select(c("Exposure", "OR", "CI_Lower", "CI_Upper", "p")) %>% 
        rename_all(tolower)

    # create names for the dfs
        # first remove everything before the slash
        f <- gsub(".*/", "", f)
        # remove the file extension
        f <- gsub(".csv", "", f)
        # remove results prefix 
        f <- gsub("results_", "", f)
    # pull out the cause group by taking the letters after the last underscore in f
        c <- gsub(".*_", "", f)

    df$model <- f
    df$cause <- c

    results <- rbind(results, df)
}

# make a plot of each model --------------------------------
# just making this for our own sanity to see the results
p <- ggplot(data = results, aes(x = exposure, y = or, color = cause)) + 
        geom_point() + 
        geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = cause)) + 
        geom_hline(yintercept = 1, linetype = "dashed") + 
        theme_minimal() + 
        theme(legend.position = "bottom") + 
        labs(x = "variable", y = "odds Ratio", color = "cause") + 
        facet_wrap(~cause, scales = "free") + 
        theme(axis.text.x = element_text(angle = 90, hjust = .5),   
            legend.position = "none") +
        scale_color_met_d("Hokusai3") + 
        scale_y_continuous()

# save the plot
pdf("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/psps_results_jun2025.pdf", height = 13, width = 10)
p
dev.off()

#######
####### Now lets process the results for the tables/figs/number plugging in the paper! 

# read in model results and covariance matrices ---------------------------------
# resp
resp_lag0 <- read.csv(paste0(results_dir, "results_adult_resp.csv")) %>% 
                mutate(across(everything(), ~str_replace_all(., ":", ".")))
resp_abs_cov <- read.csv(paste0(results_dir, "vcov_absmodel_adult_resp.csv")) %>% 
  mutate(across(everything(), ~str_replace_all(., ":", ".")))
resp_hyb_cov <- read.csv(paste0(results_dir, "vcov_hybmodel_adult_resp.csv")) %>% 
  mutate(across(everything(), ~str_replace_all(., ":", ".")))
# cardio
cardio_lag0 <- read.csv(paste0(results_dir, "results_adult_cardio.csv")) %>% 
                mutate(across(everything(), ~str_replace_all(., ":", ".")))
cardio_abs_cov <- read.csv(paste0(results_dir, "vcov_absmodel_adult_cardio.csv")) %>% 
  mutate(across(everything(), ~str_replace_all(., ":", ".")))
cardio_hyb_cov <- read.csv(paste0(results_dir, "vcov_hybmodel_adult_cardio.csv")) %>% 
                  mutate(across(everything(), ~str_replace_all(., ":", ".")))
# psych
psych_lag0 <- read.csv(paste0(results_dir, "results_adult_psych.csv")) %>% 
                mutate(across(everything(), ~str_replace_all(., ":", ".")))
psych_abs_cov <- read.csv(paste0(results_dir, "vcov_absmodel_adult_psych.csv")) %>% 
  mutate(across(everything(), ~str_replace_all(., ":", ".")))
psych_hyb_cov <- read.csv(paste0(results_dir, "vcov_hybmodel_adult_psych.csv")) %>% 
                 mutate(across(everything(), ~str_replace_all(., ":", ".")))
# copd
copd_lag0 <- read.csv(paste0(results_dir, "results_adult_copd.csv")) %>% 
                mutate(across(everything(), ~str_replace_all(., ":", ".")))
copd_abs_cov <- read.csv(paste0(results_dir, "vcov_absmodel_adult_copd.csv")) %>% 
  mutate(across(everything(), ~str_replace_all(., ":", ".")))
copd_hyb_cov <- read.csv(paste0(results_dir, "vcov_hybmodel_adult_copd.csv")) %>% 
                mutate(across(everything(), ~str_replace_all(., ":", ".")))

# store all covariance matrices
cov_matrices <- list(
  resp_abs_cov = resp_abs_cov,
  resp_hyb_cov = resp_hyb_cov,
  cardio_abs_cov = cardio_abs_cov,
  cardio_hyb_cov = cardio_hyb_cov,
  psych_abs_cov = psych_abs_cov,
  psych_hyb_cov = psych_hyb_cov,
  copd_abs_cov = copd_abs_cov,
  copd_hyb_cov = copd_hyb_cov
)

# save covariance matrices as an RDS file
saveRDS(cov_matrices, file = "cov_matrices.rds")

# compile all results data
resp_lag0_abs <- resp_lag0 %>% 
  select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
  mutate(Cause = "Respiratory")%>% 
  slice(1:10)     
resp_lag0_hyb <- resp_lag0 %>% 
  select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
  mutate(Cause = "Respiratory")%>% 
  slice(11:20)
cardio_lag0_abs <- cardio_lag0 %>%
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
    mutate(Cause = "Cardiovascular") %>% 
    slice(1:10)
cardio_lag0_hyb <- cardio_lag0 %>%
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
    mutate(Cause = "Cardiovascular") %>% 
    slice(11:20)
psych_lag0_abs <- psych_lag0 %>%
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
    mutate(Cause = "Psychiatric") %>% 
    slice(1:10) 
psych_lag0_hyb <- psych_lag0 %>%
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
    mutate(Cause = "Psychiatric") %>% 
    slice(11:20)
copd_lag0_abs <- copd_lag0 %>%
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
    mutate(Cause = "COPD") %>% 
    slice(1:10) 
copd_lag0_hyb <- copd_lag0 %>%
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
    mutate(Cause = "COPD") %>% 
    slice(11:20)

# combine data and write out
all_lag0_abs <- bind_rows(resp_lag0_abs, cardio_lag0_abs, psych_lag0_abs, copd_lag0_abs) %>% 
  # make or, ci_lower, ci_upper numeric
  mutate(across(c("OR", "CI_Lower", "CI_Upper"), as.numeric))
write.csv(all_lag0_abs, paste0(results_dir, "all_lag0_abs.csv"), row.names = FALSE)
all_lag0_hyb <- bind_rows(resp_lag0_hyb, cardio_lag0_hyb, psych_lag0_hyb, copd_lag0_hyb) %>% 
  # make or, ci_lower, ci_upper numeric
  mutate(across(c("OR", "CI_Lower", "CI_Upper"), as.numeric))
write.csv(all_lag0_hyb, paste0(results_dir, "all_lag0_hyb.csv"), row.names = FALSE)

# generate exposure dataset for fig2 -------------------------------
# we need the number of zip-days for PSPS exp, WF exp, and dual exp
    # a left join should be fine since wf is daily, but outer just in case

psps_exp_temp <- read.csv(paste0(analysis_dir, "daily_psps_binary.csv")) 
psps_exp <- psps_exp_temp %>% 
    mutate(date = as.Date(date, format = "%Y-%m-%d"),
           psps_event = ifelse(psps_abs == 1 | psps_hybrid == 1, 1, 0)) %>%
    select(c("date", "psps_event", "zip_code")) %>%
    group_by(zip_code, date) %>%
    reframe(psps_event = max(psps_event))
wf_exp <- read.csv(paste0(exp_dir, "zip_wfpm20132019.csv")) %>% 
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    select(c("date", "mean_lag05_per10", "zip_code"))

exp_data <- merge(wf_exp, psps_exp, by = c("date", "zip_code"), all = TRUE) %>% 
    mutate(psps_event = ifelse(is.na(psps_event), 0, psps_event)) %>%
    rename(wf = mean_lag05_per10)
write.csv(exp_data, paste0(exp_dir, "zip_daily_psps_wf_exposure.csv"), row.names = FALSE)
