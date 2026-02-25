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
# Updated to handle lag_type and age_group for new data structure
get_all_covariances <- function(cause, metric, severity, cov_matrices, lag_type = "same_day", age_group = "age 20 and older") {
  metric_lab <- ifelse(metric == "abs", "customers", "hybrid")
  
  # Build covariance matrix name: {cause}_{lag_type}_{age_group}_{metric}_cov
  age_clean <- gsub(" ", "_", age_group)
  age_clean <- gsub("-", "_", age_clean)
  cov_name <- paste0(cause, "_", lag_type, "_", age_clean, "_", metric, "_cov")
  
  # Get the appropriate covariance matrix
  if (!is.null(cov_matrices) && cov_name %in% names(cov_matrices)) {
    cov_df <- cov_matrices[[cov_name]]
  } else {
    # Fallback: try old naming convention for backward compatibility
    old_name <- switch(paste0(cause, "_", metric),
                      "resp_abs" = "resp_abs_cov",
                      "resp_hyb" = "resp_hyb_cov",
                      "cardio_abs" = "cardio_abs_cov",
                      "cardio_hyb" = "cardio_hyb_cov",
                      "psych_abs" = "psych_abs_cov",
                      "psych_hyb" = "psych_hyb_cov",
                      "copd_abs" = "copd_abs_cov",
                      "copd_hyb" = "copd_hyb_cov")
    if (!is.null(old_name) && old_name %in% names(cov_matrices)) {
      cov_df <- cov_matrices[[old_name]]
    } else {
      warning(paste("Covariance matrix not found:", cov_name))
      return(list(cov_psps_wf = 0, cov_psps_interaction = 0, cov_wf_interaction = 0))
    }
  }
  
  # Variable names depend on lag_type
  # Note: same_day uses lowercase severity and "wf_pm25_per10" instead of "mean_lag05_per10"
  if (lag_type == "same_day") {
    psps_var <- paste0("severity_", metric_lab, tolower(severity))
    wf_var <- "wf_pm25_per10"
    interaction_var <- paste0("severity_", metric_lab, tolower(severity), ".wf_pm25_per10")
  } else { # lag4
    psps_var <- paste0("severity_", metric_lab, "_lag4", tolower(severity))
    wf_var <- "mean_lag0_lag3_per10"
    interaction_var <- paste0("severity_", metric_lab, "_lag4", tolower(severity), ".mean_lag0_lag3_per10")
  }
  
  # extract all pairwise covariances
  # Verify we get exactly one row for each variable
  psps_row <- cov_df %>% filter(X == psps_var)
  wf_row <- cov_df %>% filter(X == wf_var)
  
  if (nrow(psps_row) != 1) {
    stop(paste("Expected exactly 1 row for", psps_var, "but found", nrow(psps_row)))
  }
  if (nrow(wf_row) != 1) {
    stop(paste("Expected exactly 1 row for", wf_var, "but found", nrow(wf_row)))
  }
  
  cov_psps_wf <- as.numeric(psps_row[[wf_var]])
  cov_psps_interaction <- as.numeric(psps_row[[interaction_var]])
  cov_wf_interaction <- as.numeric(wf_row[[interaction_var]])
  
  return(list(
    cov_psps_wf = ifelse(is.na(cov_psps_wf), 0, cov_psps_wf),
    cov_psps_interaction = ifelse(is.na(cov_psps_interaction), 0, cov_psps_interaction),
    cov_wf_interaction = ifelse(is.na(cov_wf_interaction), 0, cov_wf_interaction)
  ))
}

### Function to process results for all severity levels
# this function processes results for a given severity level, metric, lag_type, age_group, and covariance matrices
# to prepare for plotting and number plugging
process_results <- function(severity_level, all_lag0, metric = "abs", cov_matrices, lag_type = "same_day", age_group = "age 20 and older") {
  causes <- c("resp", "cardio", "psych", "copd")
  
  processed_dfs <- lapply(causes, function(cause) {
    # get all covariance values for this cause
    cov_values <- get_all_covariances(cause, metric, severity_level, cov_matrices, lag_type, age_group)
    
    # var patterns based on severity level and lag_type
    # Note: lag4 variables have "_lag4" in the name (e.g., severity_customers_lag4mild)
    # Note: same_day variables use lowercase severity and "wf_pm25_per10" instead of "mean_lag05_per10"
    if(metric == "abs"){
      if(lag_type == "same_day") {
        interaction_var <- paste0("severity_customers", tolower(severity_level), ".wf_pm25_per10")
        main_effect_var <- paste0("severity_customers", tolower(severity_level))
        wf_var <- "wf_pm25_per10"
      } else { # lag4
        interaction_var <- paste0("severity_customers_lag4", tolower(severity_level), ".mean_lag0_lag3_per10")
        main_effect_var <- paste0("severity_customers_lag4", tolower(severity_level))
        wf_var <- "mean_lag0_lag3_per10"
      }
    } else {
      if(lag_type == "same_day") {
        interaction_var <- paste0("severity_hybrid", tolower(severity_level), ".wf_pm25_per10")
        main_effect_var <- paste0("severity_hybrid", tolower(severity_level))
        wf_var <- "wf_pm25_per10"
      } else { # lag4
        interaction_var <- paste0("severity_hybrid_lag4", tolower(severity_level), ".mean_lag0_lag3_per10")
        main_effect_var <- paste0("severity_hybrid_lag4", tolower(severity_level))
        wf_var <- "mean_lag0_lag3_per10"
      }
    }
    
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
        Severity = severity_level,
        lag_type = lag_type
      ) %>% 
      select(Exposure, log_odds, variance, Cause, Severity, lag_type)
    
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
        Severity = first(Severity),
        lag_type = first(lag_type)
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