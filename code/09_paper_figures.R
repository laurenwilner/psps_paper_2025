#-------------------------------------------------
# PSPS: Paper figures
# March 2025
#-------------------------------------------------
# question for joan 
# should we plot the act interaction term or the summed, actual effect? 

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(ggforce, MetBrewer, dplyr, tidyr, knitr, gt, magick, pagedown,sf, tigris, patchwork, stringr)
pal <- c( '#6f9969', '#efc86e',"#0f7ba2")

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/Results\ -\ Mar\ 2025/")
exp_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/exposure_data/")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/tables_figures/")

# load functions -------------------------------------------------
# function to get covariance value for a specific cause/metric/severity combination
get_covariance <- function(cause, metric, severity, cov_matrices) {
  metric_lab <- ifelse(metric == "abs", "customers", "hybrid")
  # Select the right covariance matrix
  cov_df <- switch(paste0(cause, "_", metric),
                   "resp_abs" = cov_matrices$resp_abs_cov,
                   "resp_hyb" = cov_matrices$resp_hyb_cov,
                   "cardio_abs" = cov_matrices$cardio_abs_cov,
                   "cardio_hyb" = cov_matrices$cardio_hyb_cov,
                   "psych_abs" = cov_matrices$psych_abs_cov,
                   "psych_hyb" = cov_matrices$psych_hyb_cov,
                   "copd_abs" = cov_matrices$copd_abs_cov,
                   "copd_hyb" = cov_matrices$copd_hyb_cov)
  
  # Construct the variable name
  var_name <- paste0("severity_", metric_lab, severity, ".mean_lag05_per10")
  
  # Get the covariance value using tidyverse functions and ensure it's numeric
  cov_value <- cov_df %>%
    filter(X == var_name) %>%
    pull(var_name) %>%
    as.numeric()
  
  return(cov_value)
}

# prep data for all severity levels for plotting! 
process_results <- function(severity_level, all_lag0, metric = "abs", cov_matrices) {
  # Process each cause separately and then combine
  causes <- c("resp", "cardio", "psych", "copd")
  
  processed_dfs <- lapply(causes, function(cause) {
    # Get the appropriate covariance value for this cause
    cov_value <- get_covariance(cause, metric, severity_level, cov_matrices)
    
    # Define variable patterns based on severity level
    interaction_var <- paste0("severity_customers", severity_level, ".mean_lag05_per10")
    main_effect_var <- paste0("severity_customers", severity_level)
    wf_var <- "mean_lag05_per10"
    
    # Map cause to display name
    cause_display <- switch(cause,
      "resp" = "Respiratory",
      "cardio" = "Cardiovascular",
      "psych" = "Psychiatric",
      "copd" = "COPD"
    )
    
    # Prepare the data for this severity level and cause
    prep_df <- all_lag0 %>% 
      filter(
        Exposure %in% c(interaction_var, main_effect_var, wf_var),
        Cause == cause_display
      ) %>%
      mutate(
        Exposure = case_when(
          Exposure == interaction_var ~ "Interaction",  # Temporary label
          Exposure == main_effect_var ~ paste0(severity_level, " PSPS event"),
          Exposure == wf_var ~ "WF smoke"
        ),
        log_odds = log(OR),
        se = (log(CI_Upper) - log(CI_Lower)) / (2*1.96),
        variance = se^2,
        Severity = severity_level  # Add a column to track severity level
      ) %>% 
      select(Exposure, log_odds, variance, Cause, Severity)
    
    # First, handle the main effects (PSPS and WF smoke separately)
    main_effects <- prep_df %>%
      filter(Exposure != "Interaction") %>%
      select(Exposure, log_odds, variance, Cause, Severity)
    
    # Then, calculate the interaction effect (PSPS + WF smoke + interaction term)
    interaction_effect <- prep_df %>%
      # filter to only wf and interaction since thats what we will add 
      filter(Exposure %in% c("WF smoke", "Interaction")) %>%
      summarize(
        Exposure = paste0(severity_level, " PSPS event * WF smoke"),
        # Sum the log odds of all components
        log_odds = sum(log_odds),
        # Calculate total variance including covariance
        variance = sum(variance) + 2*cov_value,
        Cause = first(Cause),
        Severity = first(Severity)
      )
    
    # Combine main effects and interaction effect
    processed_df <- bind_rows(main_effects, interaction_effect)
    
    return(processed_df)
  })
  
  # Combine all processed dataframes
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
      # make a new column for category for plotting color scheme!
      category = case_when(
        grepl("\\*", Exposure) ~ "Interaction",  # For rows with * in Exposure (interaction terms)
        Exposure == "WF smoke" ~ "WF smoke",     # For WF smoke rows
        TRUE ~ "PSPS"                           # For all other rows (PSPS events)
      )
    )
  
  return(final_df)
}

# plotting function for results figs 
create_results_fig <- function(data, severity, show_disease_labels = TRUE, show_severity = TRUE) {
  # reorder the data to ensure WF smoke appears first
  data <- data %>%
    mutate(Exposure = case_when(
      Exposure == "WF smoke" ~ "WFS",
      grepl("\\*", Exposure) ~ "PSPS * WFS",
      TRUE ~ "PSPS"
    )) %>%
    mutate(Exposure = factor(Exposure, levels = c("WFS", "PSPS", "PSPS * WFS")))
  
  # base plot
  p <- ggplot(data, aes(x = Exposure, y = odds_ratio, ymin = lower_ci, ymax = upper_ci)) +
    geom_point(aes(color = category), position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = category), 
                 width = 0.3,
                 position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 1, linetype = "dashed") + 
    scale_color_manual(values = pal) +
    labs(
      x = "",
      y = if(show_severity) substitute(atop(bold(sev), "Odds Ratio"), list(sev = severity)) else "Odds Ratio"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y = element_text(size = 14),
      legend.position = "none",
      strip.text = element_text(size = 12),
      axis.text = element_text(size = 12)
    ) + 
    scale_y_log10() +
    facet_wrap(~Cause, nrow=1)
  
  # remove disease labels if show_disease_labels is FALSE
  if (!show_disease_labels) {
    p <- p + theme(strip.text = element_blank())
  }
  
  return(p)
}

# read in model results and covariance matrices ---------------------------------
# resp
resp_lag0 <- read.csv(paste0(results_dir, "results_PSPS_wflag05_nstemp_resp_V2.csv")) %>% 
                mutate(across(everything(), ~str_replace_all(., ":", ".")))
# resp_abs_cov <- read.csv(paste0(results_dir, "vcov_absmod_resp.csv")) %>% 
  # mutate(across(everything(), ~str_replace_all(., ":", ".")))
resp_hyb_cov <- read.csv(paste0(results_dir, "vcov_hybmod_resp.csv")) %>% 
  mutate(across(everything(), ~str_replace_all(., ":", ".")))
# cardio
cardio_lag0 <- read.csv(paste0(results_dir, "results_PSPS_wflag05_nstemp_cardio_V2.csv")) %>% 
                mutate(across(everything(), ~str_replace_all(., ":", ".")))
# cardio_abs_cov <- read.csv(paste0(results_dir, "vcov_absmod_cardio.csv")) %>% 
  # mutate(across(everything(), ~str_replace_all(., ":", ".")))
cardio_hyb_cov <- read.csv(paste0(results_dir, "vcov_hybmod_cardio.csv")) %>% 
                  mutate(across(everything(), ~str_replace_all(., ":", ".")))
# psych
psych_lag0 <- read.csv(paste0(results_dir, "results_PSPS_wflag05_nstemp_psych_V2.csv")) %>% 
                mutate(across(everything(), ~str_replace_all(., ":", ".")))
# psych_abs_cov <- read.csv(paste0(results_dir, "vcov_absmod_psych.csv")) %>% 
  # mutate(across(everything(), ~str_replace_all(., ":", ".")))
psych_hyb_cov <- read.csv(paste0(results_dir, "vcov_hybmod_psych.csv")) %>% 
                 mutate(across(everything(), ~str_replace_all(., ":", ".")))
# copd
copd_lag0 <- read.csv(paste0(results_dir, "results_PSPS_wflag05_nstemp_copd_V2.csv")) %>% 
                mutate(across(everything(), ~str_replace_all(., ":", ".")))
# copd_abs_cov <- read.csv(paste0(results_dir, "vcov_absmod_copd.csv")) %>% 
  # mutate(across(everything(), ~str_replace_all(., ":", ".")))
copd_hyb_cov <- read.csv(paste0(results_dir, "vcov_hybmod_copd.csv")) %>% 
                mutate(across(everything(), ~str_replace_all(., ":", ".")))

# store all covariance matrices
cov_matrices <- list(
  # resp_abs_cov = resp_abs_cov,
  resp_hyb_cov = resp_hyb_cov,
  # cardio_abs_cov = cardio_abs_cov,
  cardio_hyb_cov = cardio_hyb_cov,
  # psych_abs_cov = psych_abs_cov,
  psych_hyb_cov = psych_hyb_cov,
  # copd_abs_cov = copd_abs_cov,
  copd_hyb_cov = copd_hyb_cov
)

# compile these data
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

# combine data
all_lag0_abs <- bind_rows(resp_lag0_abs, cardio_lag0_abs, psych_lag0_abs, copd_lag0_abs) %>% 
  # make or, ci_lower, ci_upper numeric
  mutate(across(c("OR", "CI_Lower", "CI_Upper"), as.numeric))
all_lag0_hyb <- bind_rows(resp_lag0_hyb, cardio_lag0_hyb, psych_lag0_hyb, copd_lag0_hyb) %>% 
  # make or, ci_lower, ci_upper numeric
  mutate(across(c("OR", "CI_Lower", "CI_Upper"), as.numeric))

# read in exp data -------------------------------------------------
exp_data <- read.csv(paste0(exp_dir, "ca_ZIP_daily_psps_no_washout_classified_2013-2022.csv"))

# Create exposure summary by expanding dates and counting exposures
exp_summary <- exp_data %>%
  # First expand out the dates for each PSPS event
  mutate(
    date = as.Date(date),
    wf_exposed = as.logical(wf_exposed),
    psps_event = as.logical(psps_event)
  ) %>%
  # Now we can count the different types of exposure days
  group_by(zcta, date) %>%
  summarize(
    wf_exposed = any(wf_exposed),
    psps_event = any(psps_event),
    .groups = "drop"
  ) %>%
  # Calculate exposure types
  mutate(
    exposure_type = case_when(
      wf_exposed & psps_event ~ "PSPS + WFS",
      wf_exposed ~ "WFS",
      psps_event ~ "PSPS",
      TRUE ~ "None"
    )
  ) %>%
  # Get counts by exposure type
  group_by(exposure_type) %>%
  summarize(
    n_days = n(),
    n_unique_zctas = n_distinct(zcta)
  ) %>%
  filter(exposure_type != "None") %>%
  # Calculate percentages and create labels
  mutate(
    pct = n_days / sum(n_days) * 100,
    label = sprintf("%d (%d ZCTAs, %.1f%%)", n_days, n_unique_zctas, pct),
    exposure_type = factor(exposure_type, levels = c("WFS", "PSPS", "PSPS + WFS"))
  )

# Print summary stats
print("Summary of exposure days:")
print(exp_summary)

# read in map data -------------------------------------------------
# load data -------------------------------------------------
zctas <- c(90001:90008, 90011:90041, 94102:94158) # FILL IN WITH ZCTAS FROM HCAI! 
ca_shp <- tigris::states(cb = TRUE, year = 2020) %>% 
    filter(STUSPS == "CA") %>% 
    select(geometry)  %>% 
    st_transform(epsg = 3310)
zcta_shp <- tigris::zctas(cb = TRUE, year = 2020) %>% 
    rename(zcta = ZCTA5CE20) %>% 
    st_transform(epsg = 3310) %>% 
    select(zcta, geometry) %>% 
    # filter to those that intersect with CA
    st_intersection(ca_shp) %>%
    select(zcta, geometry) %>% 
    mutate(fill_flag = zcta %in% zctas)


# make figs -------------------------------------------------

################
### FIGURE 1 ###
################
# Figure 1 is a map of the ZCTAs in California that were included in this analysis. 

# plot -------------------------------------------------
color_mapping <- c(`TRUE` = pal[3], `FALSE` = "white")

fig1 <- ggplot() +
  geom_sf(data = ca_shp, fill = "white", color = alpha("black", 0.2), stroke = 0.1) +
  geom_sf(data = zcta_shp, aes(fill = fill_flag), color = alpha("black", 0.2), stroke = 0.1) +
  scale_fill_manual(values = color_mapping) +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

################
### FIGURE 2 ###
################
# Figure 2a is a descriptive plot showing the number of zip-days included in this analysis for each exposure category.
# Figure 2b is a map of PSPS and WF events, exact details tbd. 

# notes from JAC
# - make a fig to accompany the bar chart fig that is a map of zctas and they are colored by count of days during which they are exposed to wf > 10 and psps event. for bar chart fig, use same colors as results. in zctas with only psps events, do the color of just psps. in zctas with only wf, do the color of the wf smoke. and then for the dual exposure use the interaction term color as a gradient. 
# - another idea: 3 panel map with the days of psps events in one, days of wf smoke in another, and days of dual exposure in the third. if the third one is boring, maybe reroute. each is a gradient of the color that it got in the results box plot situation. 
# - combine figs 2 & 3 into one figure.

# create fig 2a -------------------------------------------------
fig2a <- ggplot(exp_summary, aes(y = exposure_type, x = n_days)) +
  geom_col(fill = pal[1]) +
  geom_text(aes(label = label), hjust = -0.1) +
  labs(
    x = "ZCTA-Days Exposure",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

################
### FIGURE 3 ###
################
# Figure 3 is a plot of our results. 

# prep for figure 3 -------------------------------------------
# 4 panel plot with one panel per cause, log transform the y axis
# •	main effect alone
# •	effect for severe psps (add the main effect + interaction)
# •	do that for each disease category
# •	main analysis: absolute
# •	supplement: same plots for hybrid
# •	make this figure similar to that hell-ish forest plot where i had the point ests and CIs listed and then the bars for the pt est and CI 
# •	DO NOT INCLUDE LAGS 1 AND 2. ONLY LAG 0. so each plot is wf main effect, psps main effect, and interaction term effect. then in results section sum interaction and main effect. 

# subset to terms of interest and rename 
#  y = beta1*psps + beta2*wf + beta3*psps*wf + spline
# no wf + psps = beta1 
# psps + wf = beta3 + beta2
# no psps + wf = beta2

# process `severe` for fig 3
# severe_df <- process_results("Severe", all_lag0, "resp", "abs", cov_matrices)
severe_df <- process_results("Severe", all_lag0_hyb, "hyb", cov_matrices)
# severe_dfb <- process_results("Severe", all_lag0_abs, "abs", cov_matrices)

# make fig 3
fig3a <- create_results_fig(severe_df, "severe", show_severity = FALSE)
# fig3b <- create_results_fig(severe_dfb, "severe", show_severity = FALSE)

# fig3 <- fig3a / fig3b

#####################
### SUPP FIGURE 1 ###
#####################
# can use same function as fig 3 for the supp fig 1

# process mild/mod for supp fig 1
mild_df <- process_results("Mild", all_lag0_hyb, "hyb", cov_matrices)
moderate_df <- process_results("Moderate", all_lag0_hyb, "hyb", cov_matrices)

# mild_dfb <- process_results("Mild", all_lag0_abs, "abs", cov_matrices)
# moderate_dfb <- process_results("Moderate", all_lag0_abs, "abs", cov_matrices)

# make the three indiv figs
mild <- create_results_fig(mild_df, "Mild", show_disease_labels = TRUE, show_severity = TRUE)
mod <- create_results_fig(moderate_df, "Moderate", show_disease_labels = FALSE, show_severity = TRUE)
sev <- create_results_fig(severe_df, "Severe", show_disease_labels = FALSE, show_severity = TRUE)

# mildb <- create_results_fig(mild_dfb, "Mild", show_disease_labels = TRUE, show_severity = TRUE)
# modb <- create_results_fig(moderate_dfb, "Moderate", show_disease_labels = FALSE, show_severity = TRUE)
# sevb <- create_results_fig(severe_dfb, "Severe", show_disease_labels = FALSE, show_severity = TRUE)

# Create a layout with labels on the left side
supp_fig1a <- mild / mod / sev
# supp_fig1b <- mildb / modb / sevb

# save figs -------------------------------------------
ggsave(paste0(out_dir, "fig1.png"), fig1, width = 10, height = 5, dpi = 100)
ggsave(paste0(out_dir, "fig2a.png"), fig2a, width = 10, height = 3, dpi = 100)
# ggsave(paste0(out_dir, "fig3.png"), fig3, width = 10, height = 5, dpi = 100)
# ggsave(paste0(out_dir, "supp_fig1a.png"), supp_fig1a, width = 10, height = 15, dpi = 100)
# ggsave(paste0(out_dir, "supp_fig1b.png"), supp_fig1b, width = 10, height = 15, dpi = 100)


