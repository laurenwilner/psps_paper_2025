#-------------------------------------------------
# PSPS: helper functions
# June 2025
#-------------------------------------------------

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(ggforce, MetBrewer, dplyr, tidyr, knitr, gt, magick, pagedown,sf, tigris, patchwork, stringr, scales)

# functions --------------------------------------------------

### Covariance function
# function to pull all covariance for combining wfs beta, psps beta, and interaction beta 
get_all_covariances <- function(cause, metric, severity, cov_matrices) {
  metric_lab <- ifelse(metric == "abs", "customers", "hybrid")
  
  # select the appropriate covariance matrix
  cov_df <- switch(paste0(cause, "_", metric),
                   "resp_abs" = cov_matrices$resp_abs_cov,
                   "resp_hyb" = cov_matrices$resp_hyb_cov,
                   "cardio_abs" = cov_matrices$cardio_abs_cov,
                   "cardio_hyb" = cov_matrices$cardio_hyb_cov,
                   "psych_abs" = cov_matrices$psych_abs_cov,
                   "psych_hyb" = cov_matrices$psych_hyb_cov,
                   "copd_abs" = cov_matrices$copd_abs_cov,
                   "copd_hyb" = cov_matrices$copd_hyb_cov)
  
  # var names
  psps_var <- paste0("severity_", metric_lab, severity)
  wf_var <- "mean_lag05_per10"
  interaction_var <- paste0("severity_", metric_lab, severity, ".mean_lag05_per10")
  
  # extract all pairwise covariances
  cov_psps_wf <- cov_df %>%
    filter(X == psps_var) %>%
    pull(!!wf_var) %>%
    as.numeric()
  
  cov_psps_interaction <- cov_df %>%
    filter(X == psps_var) %>%
    pull(!!interaction_var) %>%
    as.numeric()
  
  cov_wf_interaction <- cov_df %>%
    filter(X == wf_var) %>%
    pull(!!interaction_var) %>%
    as.numeric()
  
  return(list(
    cov_psps_wf = cov_psps_wf,
    cov_psps_interaction = cov_psps_interaction,
    cov_wf_interaction = cov_wf_interaction
  ))
}

### Function to process results for all severity levels
# this function processes results for a given severity level, metric, and covariance matrices
# to prepare for plotting and number plugging
process_results <- function(severity_level, all_lag0, metric = "abs", cov_matrices) {
  causes <- c("resp", "cardio", "psych", "copd")
  
  processed_dfs <- lapply(causes, function(cause) {
    # get all covariance values for this cause
    cov_values <- get_all_covariances(cause, metric, severity_level, cov_matrices)
    
    # var patterns based on severity level
    if(metric == "abs"){
        interaction_var <- paste0("severity_customers", severity_level, ".mean_lag05_per10")
        main_effect_var <- paste0("severity_customers", severity_level)
    } else {
        interaction_var <- paste0("severity_hybrid", severity_level, ".mean_lag05_per10")
        main_effect_var <- paste0("severity_hybrid", severity_level)
    }
    wf_var <- "mean_lag05_per10"
    
    # map cause to display name
    cause_display <- switch(cause,
      "resp" = "Respiratory",
      "cardio" = "Cardiovascular", 
      "psych" = "Psychiatric",
      "copd" = "COPD"
    )
    
    # prep data for this severity level and cause
    prep_df <- all_lag0 %>% 
      filter(
        Exposure %in% c(interaction_var, main_effect_var, wf_var),
        Cause == cause_display
      ) %>%
      mutate(
        Exposure = case_when(
          Exposure == interaction_var ~ "Interaction",
          Exposure == main_effect_var ~ "PSPS event",
          Exposure == wf_var ~ "WF smoke"
        ),
        log_odds = log(OR),
        se = (log(CI_Upper) - log(CI_Lower)) / (2*1.96),
        variance = se^2,
        Severity = severity_level
      ) %>% 
      select(Exposure, log_odds, variance, Cause, Severity)
    
    # keep all individual effects (including interaction term)
    individual_effects <- prep_df %>%
      mutate(Exposure = case_when(
        Exposure == "PSPS event" ~ paste0(severity_level, " PSPS event"),
        Exposure == "Interaction" ~ paste0(severity_level, " PSPS event * WF smoke (interaction only)"),
        TRUE ~ Exposure  # WF smoke stays as is
      ))
    
    # calc the combined effect (psps + wfs + interaction)
    combined_effect <- prep_df %>%
      summarize(
        Exposure = paste0(severity_level, " PSPS event * WF smoke (combined)"),
        # sum all three log odds
        log_odds = sum(log_odds),
        # calc total variance including all pairwise covariances
        variance = sum(variance) + 
                   2 * cov_values$cov_psps_wf +
                   2 * cov_values$cov_psps_interaction +
                   2 * cov_values$cov_wf_interaction,
        Cause = first(Cause),
        Severity = first(Severity)
      )
    
    # combine all effects (3 individual + 1 combined = 4 total)
    processed_df <- bind_rows(individual_effects, combined_effect)
    
    return(processed_df)
  })
  
  # compile all processed dataframes into one
  final_df <- bind_rows(processed_dfs) %>%
    mutate(
      se = sqrt(variance),
      odds_ratio = exp(log_odds),
      lower_ci = exp(log_odds - 1.96 * se),
      upper_ci = exp(log_odds + 1.96 * se)
    ) %>% 
    select(-c("log_odds", "variance", "se")) %>% 
    mutate(
      Cause = factor(Cause, levels = c("Cardiovascular", "Psychiatric", "Respiratory", "COPD")),
      category = case_when(
        grepl("combined", Exposure) ~ "Combined",
        grepl("interaction only", Exposure) ~ "Interaction", 
        Exposure == "WF smoke" ~ "WF smoke",
        TRUE ~ "PSPS"
      )
    )
  
  return(final_df)
}