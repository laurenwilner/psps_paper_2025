#-------------------------------------------------
# PSPS: Paper figures
# March 2025
#-------------------------------------------------

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(ggforce, MetBrewer, dplyr, tidyr, knitr, gt, magick, pagedown,sf, tigris, patchwork, stringr, scales)
pal <- c( '#6f9969', '#efc86e',"#0f7ba2")
crs <- "EPSG:3310" # California Albers Equal Area Conic projection

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/Results\ -\ June\ 2025/")
exp_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/exposure_data/")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/tables_figures/")
data_dir <- ("~/Desktop/Desktop/epidemiology_PhD/01_data/clean/")
code_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/code/")

# load functions -------------------------------------------------
source(paste0(code_dir, "00_helper_functions.R"))

# plotting function for results figs 
create_results_fig_combined <- function(data_abs = NULL, data_hyb = NULL, severity, 
                                show_disease_labels = TRUE, 
                                show_severity = TRUE, 
                                show_legend = TRUE) {
  
  # Check which datasets are provided
  datasets_provided <- c(!is.null(data_abs), !is.null(data_hyb))
  
  # Process absolute data if provided
  if (!is.null(data_abs)) {
    data_abs_processed <- data_abs %>%
      mutate(
        Exposure = case_when(
          Exposure == "WF smoke" ~ "WFS (per 10 μg/m³)",
          grepl("combined", Exposure) ~ "Additive interaction",
          grepl("interaction only", Exposure) ~ "Multiplicative interaction",
          TRUE ~ "PSPS"
        ),
        analysis_type = "Absolute"
      ) %>%
      mutate(Exposure = factor(Exposure, levels = c("PSPS", "WFS (per 10 μg/m³)", "Multiplicative interaction", "Additive interaction")))
  }
  
  # Process Relative data if provided
  if (!is.null(data_hyb)) {
    data_hyb_processed <- data_hyb %>%
      mutate(
        Exposure = case_when(
          Exposure == "WF smoke" ~ "WFS (per 10 μg/m³)",
          grepl("combined", Exposure) ~ "Additive interaction",
          grepl("interaction only", Exposure) ~ "Multiplicative interaction",
          TRUE ~ "PSPS"
        ),
        analysis_type = "Relative"
      ) %>%
      mutate(Exposure = factor(Exposure, levels = c("PSPS", "WFS (per 10 μg/m³)", "Multiplicative interaction", "Additive interaction")))
  }
  
  # Combine the datasets based on what's provided
  if (all(datasets_provided)) {
    # Both datasets provided
    combined_data <- bind_rows(data_abs_processed, data_hyb_processed) %>%
      mutate(analysis_type = factor(analysis_type, levels = c("Absolute", "Relative")),
             Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric")))
    
    # Set up scales for both types
    alpha_scale <- scale_alpha_manual(values = c("Absolute" = 1.0, "Relative" = 0.6), 
                                     name = "", 
                                     labels = c("Absolute", "Relative"))
    shape_scale <- scale_shape_manual(values = c("Absolute" = 16, "Relative" = 17),
                                     name = "",
                                     labels = c("Absolute", "Relative"))
  } else if (datasets_provided[1]) {
    # Only absolute data provided
    combined_data <- data_abs_processed %>%
      mutate(Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric")))
    
    # No need for alpha or shape scales
    alpha_scale <- NULL
    shape_scale <- NULL
  } else if (datasets_provided[2]) {
    # Only relative data provided
    combined_data <- data_hyb_processed %>%
      mutate(Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric")))
    
    # No need for alpha or shape scales
    alpha_scale <- NULL
    shape_scale <- NULL
  } else {
    stop("At least one dataset (data_abs or data_hyb) must be provided")
  }
  
  # color mapping
  exposure_colors <- c(
    "WFS (per 10 μg/m³)" = pal[3],           # blue
    "PSPS" = pal[2],          # yellow  
    "Multiplicative interaction" = pal[1],    # green
    "Additive interaction" = "#013220"    
  )
  
  # base plot - conditional aesthetics based on number of datasets
  if (all(datasets_provided)) {
    # Both datasets - use alpha and shape
    p <- ggplot(combined_data, aes(x = Exposure, y = odds_ratio, ymin = lower_ci, ymax = upper_ci)) +
      geom_point(aes(color = Exposure, alpha = analysis_type, shape = analysis_type), 
                 position = position_dodge(width = 0.6), 
                 size = 3) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = Exposure, alpha = analysis_type), 
                    width = 0.3,
                    position = position_dodge(width = 0.6)) +
      geom_hline(yintercept = 1, linetype = "dashed") + 
      scale_color_manual(values = exposure_colors, name = "Exposure Type") +
      alpha_scale +
      shape_scale +
      labs(
        x = "",
        y = if(show_severity) substitute(atop(bold(sev), "Odds Ratio"), list(sev = severity)) else "Odds Ratio"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.spacing.y = unit(0.3, "cm"),
        strip.text = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14)
      ) + 
      scale_y_log10(breaks = c(0.8, 0.9, 1.0, 1.1, 1.2, 1.5, 2.0, 3.0)) +
      facet_wrap(~Cause, nrow = 1)
  } else {
    # Single dataset - no alpha or shape
    p <- ggplot(combined_data, aes(x = Exposure, y = odds_ratio, ymin = lower_ci, ymax = upper_ci)) +
      geom_point(aes(color = Exposure), 
                 position = position_dodge(width = 0.6), 
                 size = 3) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = Exposure), 
                    width = 0.3,
                    position = position_dodge(width = 0.6)) +
      geom_hline(yintercept = 1, linetype = "dashed") + 
      scale_color_manual(values = exposure_colors, name = "Exposure Type") +
      labs(
        x = "",
        y = if(show_severity) substitute(atop(bold(sev), "Odds Ratio"), list(sev = severity)) else "Odds Ratio"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.spacing.y = unit(0.3, "cm"),
        strip.text = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14)
      ) + 
      scale_y_log10(breaks = c(0.8, 0.9, 1.0, 1.1, 1.2, 1.5, 2.0, 3.0)) +
      facet_wrap(~Cause, nrow = 1)
  }
  
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
og_psps_dataset <- read.csv(paste0(data_dir, "ca_ZIP_daily_psps_no_washout_wf_classified_2013-2022.csv"))

# read in exposure dataset for fig2
# we need the number of zip-days for PSPS exp, WF exp, and dual exp
exp_data <- read.csv(paste0(exp_dir, "zip_daily_psps_wf_exposure.csv"))

# read in map data -------------------------------------------------
# load data -------------------------------------------------
zctas <- read.csv(paste0(results_dir, "list_zcta_in_analysis.csv")) %>% pull(x) 
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

###################################
### MAP AND VIOLIN PANELLED PLOT ###
###################################
# This is a map + violin plot of the number of zip-days for each exposure type. 

# fig 1 -------------------------------------------------
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

# create lists to store individual plots
map_plots <- list()
violin_plots <- list()

# create a separate plot for each exposure type
for (i in seq_along(exposure_types)) {
  exp_type <- exposure_types[i]
  exp_color <- ifelse(exp_type == "WF smoke + PSPS event", pal[1], 
                      ifelse(exp_type == "PSPS event only", pal[2], pal[3]))
  
  # filter data for this exposure type
  exp_sum_shp_temp <- exp_sum_shp %>% 
    filter(exposure_type == exp_type)

  # create the map
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
  
  # create the violin plot
  violin_plot <- ggplot(exp_sum_shp_temp, aes(x = n_days, y = 1)) +
    geom_violin(fill = NA, color = exp_color, size = 0.5) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
      aspect.ratio = 0.2  # make the plot shorter
    ) +
    scale_x_continuous(labels = scales::comma_format(accuracy = 1))
  
  # store plots in lists
  map_plots[[i]] <- map_plot
  violin_plots[[i]] <- violin_plot
}

# make each panel separately
map_violin_panel1 <- map_plots[[1]] / violin_plots[[1]]  & 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
map_violin_panel2 <- map_plots[[2]] / violin_plots[[2]] & 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
map_violin_panel3 <- map_plots[[3]] / violin_plots[[3]] & 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

# combine panels side by side
# map_violin_panel <- map_violin_panel3 + map_violin_panel2 + map_violin_panel1 + 
#   plot_layout(ncol=3)
# DOING THIS IN LATEX, IT WAS TOO HARD TO DO HERE! 

########################
### SEASONALITY FIG ###
########################
# PSPS seasonality plot -------------------------------------------------
# Create monthly summary of PSPS events (collapsed across years)
monthly_summary <- exp_data %>%
  mutate(
    date = as.Date(date),
    month = lubridate::month(date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(month) %>%
  summarise(
    n_events = sum(psps_event),
    .groups = "drop"
  )

# Create seasonality plot
seasonality_plot <- ggplot(monthly_summary, aes(x = month, y = n_events)) +
  geom_col(fill = pal[2], alpha = 0.8, width = 1) +
  geom_text(aes(label = n_events), vjust = -0.5, size = 4) + 
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.text.x = element_blank(),  # Remove x-axis labels from top plot
    axis.title = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.5),
    plot.title = element_text(size = 18, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(20, 20, 5, 20)  # Reduce bottom margin
  ) +
  labs(
    x = "",
    y = "Number of zip code-event days"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    breaks = scales::pretty_breaks(n = 6)
  ) 

# WFS seasonality plot -------------------------------------------------
# Create monthly summary of WFS events (collapsed across years)
monthly_summary_wf <- exp_data %>%
  mutate(
    date = as.Date(date)
  ) %>%
  group_by(date) %>%
  summarise(mean_wf = mean(wf, na.rm=TRUE)) %>%
  mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE)) %>%
  group_by(month) %>%
  summarise(
    mean_wf = mean(mean_wf, na.rm=TRUE)
  )

# Create seasonality plot
seasonality_plot_wf <- ggplot(monthly_summary_wf, aes(x = month, y = mean_wf)) +
  geom_col(fill = pal[3], alpha = 0.8, width = 1) +
  geom_text(aes(label = round(mean_wf, 4)), vjust = -0.5, size = 4) + 
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.5),
    plot.title = element_text(size = 18, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    x = "",
    y = "Mean WFS"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    breaks = scales::pretty_breaks(n = 6)
  ) 

# patchwork with common x axis and common title 
seasonality_plot_combined <- seasonality_plot / seasonality_plot_wf + 
  plot_layout(ncol = 1)
  
####################    
### RESULTS FIG ###
####################
# This is a plot of our results. 

# prep for results figure -------------------------------------------
severe_df_hyb <- process_results("Severe", all_lag0_hyb, "hyb", cov_matrices)
severe_df_abs <- process_results("Severe", all_lag0_abs, "abs", cov_matrices)

# make results figure -------------------------------------------
results_fig <- create_results_fig_combined(severe_df_abs, NULL, "Severe", show_severity = FALSE)


# make results figure for supplement -------------------------------------------
# results_fig_supplement <- create_results_fig_combined(severe_df_abs, severe_df_hyb, "severe", show_severity = FALSE)



##########################################
### Map of ZCTAS included in analysis ###
##########################################
# This is a map of the ZCTAs in California that were included in this analysis. 

# plot -------------------------------------------------
color_mapping <- c(`TRUE` = pal[3], `FALSE` = "white")

zctas_included_map <- ggplot() +
  geom_sf(data = ca_shp, fill = "white", color = alpha("black", 0.2), stroke = 0.1) +
  geom_sf(data = zcta_shp, aes(fill = fill_flag), color = alpha("black", 0.2), stroke = 0.1) +
  scale_fill_manual(values = color_mapping) +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

########################
### SUPP RESULTS FIG ###
########################
# can use same function as fig 2 for the supp fig 2

# process mild/mod for supp fig 2
mild_df_hyb <- process_results("Mild", all_lag0_hyb, "hyb", cov_matrices)
moderate_df_hyb <- process_results("Moderate", all_lag0_hyb, "hyb", cov_matrices)

mild_df_abs <- process_results("Mild", all_lag0_abs, "abs", cov_matrices)
moderate_df_abs <- process_results("Moderate", all_lag0_abs, "abs", cov_matrices)

# make the three indiv figs
mild <- create_results_fig_combined(mild_df_hyb, mild_df_abs, "Mild", show_disease_labels = TRUE, show_severity = TRUE, show_legend = FALSE)
mod <- create_results_fig_combined(moderate_df_hyb, moderate_df_abs, "Moderate", show_disease_labels = FALSE, show_severity = TRUE, show_legend = FALSE)
sev <- create_results_fig_combined(severe_df_hyb, severe_df_abs, "Severe", show_disease_labels = FALSE, show_severity = TRUE)

# Create a layout with labels on the left side
results_fig_supplement <- mild / mod / sev

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
ggsave(paste0(out_dir, "zctas_included_map.pdf"), zctas_included_map, width = 4, height = 6, dpi = 100)
ggsave(paste0(out_dir, "map_violin_panel1.pdf"), map_violin_panel1, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "map_violin_panel2.pdf"), map_violin_panel2, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "map_violin_panel3.pdf"), map_violin_panel3, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "results_fig.pdf"), results_fig, width = 10, height = 15, dpi = 100)
ggsave(paste0(out_dir, "results_fig_supplement.pdf"), results_fig_supplement, width = 10, height = 15, dpi = 100)
ggsave(paste0(out_dir, "duration_hist.pdf"), duration_hist, width = 15, height = 7, dpi = 100)
ggsave(paste0(out_dir, "seasonality_plot.pdf"), seasonality_plot_combined, width = 15, height = 7, dpi = 100)

ggsave(paste0(out_dir, "zctas_included_map.png"), zctas_included_map, width = 4, height = 5, dpi = 100)
ggsave(paste0(out_dir, "map_violin_panel1.png"), map_violin_panel1, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "map_violin_panel2.png"), map_violin_panel2, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "map_violin_panel3.png"), map_violin_panel3, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(paste0(out_dir, "results_fig.png"), results_fig, width = 10, height = 10, dpi = 100)
ggsave(paste0(out_dir, "results_fig_supplement.png"), results_fig_supplement, width = 10, height = 15, dpi = 100)
ggsave(paste0(out_dir, "seasonality_plot.png"), seasonality_plot_combined, width = 15, height = 7, dpi = 100)


## NOTE: INCREASE DPIS WHEN WE ARE DONE! 