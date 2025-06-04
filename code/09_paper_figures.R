#-------------------------------------------------
# PSPS: Paper figures
# March 2025
#-------------------------------------------------
# question for joan 
# should we plot the act interaction term or the summed, actual effect? 

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(ggforce, MetBrewer, dplyr, tidyr, knitr, gt, magick, pagedown,sf, tigris, patchwork, stringr, scales)
pal <- c( '#6f9969', '#efc86e',"#0f7ba2")
crs <- "EPSG:3310" # California Albers Equal Area Conic projection

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/Results\ -\ May\ 2025/")
exp_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/exposure_data/")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/tables_figures/")
data_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")

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
  
  # make the variable name
  var_name <- paste0("severity_", metric_lab, severity, ".mean_lag05_per10")
  
  # get the covariance value using tidyverse functions and ensure it's numeric
  cov_value <- cov_df %>%
    filter(X == var_name) %>%
    pull(var_name) %>%
    as.numeric()
  
  return(cov_value)
}

# prep data for all severity levels for plotting! 
process_results <- function(severity_level, all_lag0, metric = "abs", cov_matrices) {
  # lets process each cause separately and then combine
  causes <- c("resp", "cardio", "psych", "copd")
  
  processed_dfs <- lapply(causes, function(cause) {
    # pull the covariance value for this cause
    cov_value <- get_covariance(cause, metric, severity_level, cov_matrices)
    
    # var patterns based on severity level
    if(metric == "abs"){
        interaction_var <- paste0("severity_customers", severity_level, ".mean_lag05_per10")
        main_effect_var <- paste0("severity_customers", severity_level)
    }else{
        interaction_var <- paste0("severity_hybrid", severity_level, ".mean_lag05_per10")
        main_effect_var <- paste0("severity_hybrid", severity_level)
    }
    wf_var <- "mean_lag05_per10"
    
    # map of cause to display name
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
    
    # first, handle the main effects (PSPS and WF smoke separately)
    main_effects <- prep_df %>%
      filter(Exposure != "Interaction") %>%
      select(Exposure, log_odds, variance, Cause, Severity)
    
    # then, calculate the interaction effect (PSPS + WF smoke + interaction term)
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
    
    # combine main effects and interaction effect
    processed_df <- bind_rows(main_effects, interaction_effect)
    
    return(processed_df)
  })
  
  # combine all processed dataframes
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
create_results_fig_combined <- function(data_abs, data_hyb, severity, 
                                show_disease_labels = TRUE, 
                                show_severity = TRUE, 
                                show_legend = TRUE) {
  # process absolute data
  data_abs_processed <- data_abs %>%
    mutate(
      Exposure = case_when(
        Exposure == "WF smoke" ~ "WFS",
        grepl("\\*", Exposure) ~ "PSPS + WFS",
        TRUE ~ "PSPS"
      ),
      analysis_type = "Absolute"
    ) %>%
    mutate(Exposure = factor(Exposure, levels = c("WFS", "PSPS", "PSPS + WFS")))
  
  # process hybrid data
  data_hyb_processed <- data_hyb %>%
    mutate(
      Exposure = case_when(
        Exposure == "WF smoke" ~ "WFS",
        grepl("\\*", Exposure) ~ "PSPS + WFS",
        TRUE ~ "PSPS"
      ),
      analysis_type = "Hybrid"
    ) %>%
    mutate(Exposure = factor(Exposure, levels = c("WFS", "PSPS", "PSPS + WFS")))
  
  # combine the datasets
  combined_data <- bind_rows(data_abs_processed, data_hyb_processed) %>%
    mutate(analysis_type = factor(analysis_type, levels = c("Absolute", "Hybrid")))
  
  # get unique categories from the data in the correct order
  unique_categories <- unique(combined_data$category)
  
  # reorder colors to match exposure order: WFS (blue), PSPS (yellow), PSPS + WFS (green)
  color_order <- c(2, 3, 1)  
  ordered_colors <- pal[color_order[1:length(unique_categories)]]
  
  # create color mapping for categories
  category_colors <- setNames(ordered_colors, unique_categories)
  
  # base plot
  p <- ggplot(combined_data, aes(x = Exposure, y = odds_ratio, ymin = lower_ci, ymax = upper_ci)) +
    geom_point(aes(color = category, alpha = analysis_type, shape = analysis_type), 
               position = position_dodge(width = 0.6), 
               size = 3) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = category, alpha = analysis_type), 
                  width = 0.3,
                  position = position_dodge(width = 0.6)) +
    geom_hline(yintercept = 1, linetype = "dashed") + 
    scale_color_manual(values = category_colors, guide = "none") +
    scale_alpha_manual(values = c("Absolute" = 1.0, "Hybrid" = 0.6), 
                       name = "", 
                       labels = c("Absolute", "Hybrid")) +
    scale_shape_manual(values = c("Absolute" = 16, "Hybrid" = 17),
                       name = "",
                       labels = c("Absolute", "Hybrid")) +
    labs(
      x = "",
      y = if(show_severity) substitute(atop(bold(sev), "Odds Ratio"), list(sev = severity)) else "Odds Ratio"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y = element_text(size = 16),
      legend.position = "bottom",
      strip.text = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14)
    ) + 
    scale_y_log10(breaks = c(0.8, 0.9, 1.0, 1.1, 1.2, 1.5, 2.0, 3.0)) +
    facet_wrap(~Cause, nrow = 1)
  
  # remove disease labels if show_disease_labels is FALSE
  if (!show_disease_labels) {
    p <- p + theme(strip.text = element_blank())
  }

  # remove legend if show_legend is FALSE
  if(!show_legend){
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}


# read in model results and covariance matrices ---------------------------------
# these were processed and created in the 07_process_results.R script
cov_matrices <- readRDS("cov_matrices.rds")
all_lag0_abs <- read.csv(paste0(results_dir, "all_lag0_abs.csv"))
all_lag0_hyb <- read.csv(paste0(results_dir, "all_lag0_hyb.csv"))


# # read in exp data -------------------------------------------------
og_psps_dataset <- read.csv(paste0(data_dir, "ca_ZIP_daily_psps_no_washout_classified_2013-2022.csv"))

# read in exposure dataset for fig2
# we need the number of zip-days for PSPS exp, WF exp, and dual exp
exp_data <- read.csv(paste0(exp_dir, "zip_daily_psps_wf_exposure.csv"))

# read in map data -------------------------------------------------
# load data -------------------------------------------------
zctas <- c(90001:90008, 90011:90041, 94102:94158) # FILL IN WITH ZCTAS FROM HCAI! 
ca_shp <- tigris::states(cb = TRUE, year = 2020) %>% 
  filter(NAME == "California") %>% 
  st_transform(epsg = 3310)
zcta_shp <- tigris::zctas(cb = TRUE, year = 2020) %>% 
    rename(zcta = ZCTA5CE20) %>% 
    st_transform(epsg = 3310) %>% 
    select(zcta, geometry) %>% 
    # filter to those that intersect with CA
    st_intersection(ca_shp) %>%
    select(zcta, geometry) %>% 
    mutate(fill_flag = zcta %in% zctas)
zip_shp <- st_read(paste0(exp_dir, "ca_zip.geojson")) %>% 
            rename(zip_code = ZIP_CODE) %>%
            select(c("zip_code", "geometry"))
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


    # QUESTION FOR JOAN: do we want mild/mod/sev psps or just binary? 
    # QUESTION FOR JOAN: DO WE WANT TO SHOW 'NO EXPOSURE' IN BAR CHART? 
    # QUESTION FOR JOAN: do we want to filter to wf over 10? look at caitlins code together.

# fig 2 -------------------------------------------------
# get the number of zipcode-days for each exposure type
exp_summary <- exp_data %>% 
    mutate(year = lubridate::year(date),
        zip_code = as.character(zip_code),
        exposure_type = case_when(psps_event>0 & wf<=0 ~ "PSPS event only", 
                            psps_event<=0 & wf>0 ~ "WF smoke only",
                            psps_event>0 & wf>0 ~ "WF smoke + PSPS event", 
                            TRUE ~ "No exposure")) %>% 
    group_by(zip_code, exposure_type, year) %>% 
    summarise(
        n_days = n()) %>% 
    filter(year >= 2013 & year <= 2019)

exp_sum_shp <- exp_summary %>% 
    filter(exposure_type != "No exposure") %>%
    left_join(zip_shp, by = "zip_code") %>% 
    st_as_sf() %>%
    filter(!st_is_empty(geometry)) %>%
    st_transform(., crs) %>% 
    group_by(zip_code, exposure_type, geometry) %>% 
    summarise(n_days = sum(n_days))



# ggplot map faceted by exp type
exposure_types <- c("WF smoke only", "PSPS event only", "WF smoke + PSPS event")

# Create lists to store individual plots
map_plots <- list()
violin_plots <- list()

# Create a separate plot for each exposure type
for (i in seq_along(exposure_types)) {
  exp_type <- exposure_types[i]
  exp_color <- ifelse(exp_type == "WF smoke + PSPS event", pal[1], 
                      ifelse(exp_type == "PSPS event only", pal[2], pal[3]))
  
  # Filter data for this exposure type
  exp_sum_shp_temp <- exp_sum_shp %>% 
    filter(exposure_type == exp_type)

  # Create the map
  map_plot <- ggplot() +
    geom_sf(data = exp_sum_shp_temp, aes(fill = n_days)) +
    geom_sf(data = zip_shp, fill = NA, color = alpha("grey", 0.5), size = 0.5) +
    scale_fill_gradient(
      low = "#fef7e6",  # Very light yellow
      high = exp_color,  # Original yellow
      name = "Number of Days",
      labels = scales::comma_format(accuracy = 1)
    ) +
    theme_minimal() +
    labs(title = exp_type) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.position = "bottom",
      legend.key.width = unit(0.175, "npc"),  # Make legend wider
      legend.box.just = "center",  # Center the legend
      legend.box.margin = margin(0, 0, 0, 0)  # Remove extra margin
    )
  
  # Create the violin plot
  violin_plot <- ggplot(exp_sum_shp_temp, aes(x = n_days, y = 1)) +
    geom_violin(fill = NA, color = exp_color, size = 0.5) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
      aspect.ratio = 0.2  # Make the plot shorter
    ) +
    scale_x_continuous(labels = scales::comma_format(accuracy = 1))
  
  # Store plots in lists
  map_plots[[i]] <- map_plot
  violin_plots[[i]] <- violin_plot
}

# Create each panel separately
fig2_panel1 <- map_plots[[1]] / violin_plots[[1]]  & 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
fig2_panel2 <- map_plots[[2]] / violin_plots[[2]] & 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
fig2_panel3 <- map_plots[[3]] / violin_plots[[3]] & 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

# Combine panels side by side
# fig2 <- panel3 + panel2 + panel1 + 
#   plot_layout(ncol=3)

################
### FIGURE 3 ###
################
# Figure 3 is a plot of our results. 

# prep for figure 3 -------------------------------------------

# subset to terms of interest and rename 
#  y = beta1*psps + beta2*wf + beta3*psps*wf + spline
# no wf + psps = beta1 
# psps + wf = beta3 + beta2
# no psps + wf = beta2

# process `severe` for fig 3
# severe_df <- process_results("Severe", all_lag0, "resp", "abs", cov_matrices)
severe_df_hyb <- process_results("Severe", all_lag0_hyb, "hyb", cov_matrices)
severe_df_abs <- process_results("Severe", all_lag0_abs, "abs", cov_matrices)

# make fig 3
fig3 <- create_results_fig_combined(severe_df_abs, severe_df_hyb, "severe", show_severity = FALSE)

#####################
### SUPP FIGURE 1 ###
#####################
# can use same function as fig 3 for the supp fig 1

# process mild/mod for supp fig 1
mild_df_hyb <- process_results("Mild", all_lag0_hyb, "hyb", cov_matrices)
moderate_df_hyb <- process_results("Moderate", all_lag0_hyb, "hyb", cov_matrices)

mild_df_abs <- process_results("Mild", all_lag0_abs, "abs", cov_matrices)
moderate_df_abs <- process_results("Moderate", all_lag0_abs, "abs", cov_matrices)

# make the three indiv figs
mild <- create_results_fig_combined(mild_df_hyb, mild_df_abs, "mild", show_disease_labels = TRUE, show_severity = TRUE, show_legend = FALSE)
mod <- create_results_fig_combined(moderate_df_hyb, moderate_df_abs, "moderate", show_disease_labels = FALSE, show_severity = TRUE, show_legend = FALSE)
sev <- create_results_fig_combined(severe_df_hyb, severe_df_abs, "severe", show_disease_labels = FALSE, show_severity = TRUE)

# Create a layout with labels on the left side
supp_fig1 <- mild / mod / sev

########################
### Duration for Joan ###
########################
# This plot is just a plot of PSPS durations for Joan 
pal_hist <- met.brewer("Derain")
duration_hist <- og_psps_dataset %>% 
  select(c("duration", "severity_customers")) %>% 
  mutate(severity_customers = factor(severity_customers, 
                                   levels = c("Severe", "Moderate", "Mild"))) %>%
  filter(duration < 250) %>%
  ggplot(aes(x = duration, fill = severity_customers)) +
  scale_fill_manual(values = pal_hist) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  geom_vline(xintercept = 8, linetype = "dashed", color = pal_hist[6], size = 1.5) +
  labs(x = "PSPS duration (hours)", 
       y = "count", 
       title = "PSPS duration distribution",
       fill = "Severity") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))


########################
### SAVE ALL FIGURES ###
########################
ggsave(paste0(out_dir, "fig1.pdf"), fig1, width = 10, height = 5, dpi = 100)
ggsave(paste0(out_dir, "fig2_panel1.pdf"), fig2_panel1, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "fig2_panel2.pdf"), fig2_panel2, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "fig2_panel3.pdf"), fig2_panel3, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "fig3.pdf"), fig3, width = 10, height = 15, dpi = 100)
ggsave(paste0(out_dir, "supp_fig1.pdf"), supp_fig1, width = 10, height = 15, dpi = 100)

ggsave(paste0(out_dir, "fig1.png"), fig1, width = 10, height = 5, dpi = 100)
ggsave(paste0(out_dir, "fig2_panel1.png"), fig2_panel1, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "fig2_panel2.png"), fig2_panel2, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "fig2_panel3.png"), fig2_panel3, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "fig3.png"), fig3, width = 10, height = 10, dpi = 100)
ggsave(paste0(out_dir, "supp_fig1.png"), supp_fig1, width = 10, height = 15, dpi = 100)

ggsave(paste0(out_dir, "duration_hist.pdf"), duration_hist, width = 15, height = 7, dpi = 100)

## NOTE: INCREASE DPIS WHEN WE ARE DONE! 