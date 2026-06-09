#-------------------------------------------------
# PSPS: Paper figures
# March 2025
#-------------------------------------------------

# Bootstrap: source paths.R (edit path in paths.R when moving machines)
args0 <- commandArgs(trailingOnly = FALSE)
file0 <- grep("^--file=", args0, value = TRUE)
if (length(file0) > 0) {
  source(file.path(dirname(normalizePath(sub("^--file=", "", file0))), "paths.R"))
} else {
  source(file.path(path.expand("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025"), "code", "paths.R"))
}

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(ggforce, MetBrewer, dplyr, tidyr, knitr, gt, magick, pagedown, sf, tigris, patchwork, stringr, scales, ggplot2)
pal <- c( '#6f9969', '#efc86e',"#0f7ba2")
crs <- "EPSG:3310" # California Albers Equal Area Conic projection

# load functions -------------------------------------------------
source(file.path(code_dir, "00_helper_functions.R"))

# Helper to load processed results and prepare for plotting (calls process_results for each lag_type)
# metric_type: "abs" or "hyb"; age_group: e.g. "age 20 and older"
load_and_prepare_data <- function(metric_type = "abs", age_group = "age 20 and older", severity_level, cov_matrices) {
  # Load the combined results file
  if (metric_type == "abs") {
    all_data <- read.csv(file.path(results_dir, "all_results_abs_jan2026.csv"))
  } else {
    all_data <- read.csv(file.path(results_dir, "all_results_hyb_jan2026.csv"))
  }
  
  # Filter by age group
  all_data <- all_data %>%
    filter(age_group == !!age_group)
  
  # Split by lag_type
  same_day_data <- all_data %>%
    filter(lag_type == "same_day") %>%
    select(Exposure, OR, CI_Lower, CI_Upper, Cause)
  
  lag4_data <- all_data %>%
    filter(lag_type == "lag4") %>%
    select(Exposure, OR, CI_Lower, CI_Upper, Cause)
  
  # Process each through process_results (now handles lag_type and age_group)
  same_day_processed <- if (nrow(same_day_data) > 0) {
    process_results(severity_level, same_day_data, metric = metric_type, cov_matrices, lag_type = "same_day", age_group = age_group)
  } else {
    NULL
  }
  
  lag4_processed <- if (nrow(lag4_data) > 0) {
    process_results(severity_level, lag4_data, metric = metric_type, cov_matrices, lag_type = "lag4", age_group = age_group)
  } else {
    NULL
  }
  
  return(list(same_day = same_day_processed, lag4 = lag4_processed))
}


# plotting function for results figs 
# Now takes same_day and lag4 data instead of abs/hyb
create_results_fig_combined <- function(data_same_day = NULL, data_lag4 = NULL, 
                                show_disease_labels = TRUE, 
                                show_legend = TRUE,
                                panel_label = NULL) {
  
  # Define label constants (avoids Unicode parsing issues)
  wf_pm_label <- "WF PM\u2082.\u2085 (per 10 \u03BCg/m\u00B3)"
  exp_levels <- c("PSPS", wf_pm_label, "Multiplicative interaction*", "Joint effect")
  
  # Check which datasets are provided
  datasets_provided <- c(!is.null(data_same_day), !is.null(data_lag4))
  
  # Process same_day data if provided
  if (!is.null(data_same_day)) {
    data_same_day_processed <- data_same_day %>%
      mutate(
        Exposure = case_when(
          Exposure == "WF smoke" ~ wf_pm_label,
          grepl("combined", Exposure) ~ "Joint effect",
          grepl("interaction only", Exposure) ~ "Multiplicative interaction*",
          TRUE ~ "PSPS"
        ),
        lag_type = "Same day"
      ) %>%
      mutate(Exposure = factor(Exposure, levels = exp_levels))
  }
  
  # Process lag4 data if provided
  if (!is.null(data_lag4)) {
    data_lag4_processed <- data_lag4 %>%
      mutate(
        Exposure = case_when(
          Exposure == "WF smoke" ~ wf_pm_label,
          grepl("combined", Exposure) ~ "Joint effect",
          grepl("interaction only", Exposure) ~ "Multiplicative interaction*",
          TRUE ~ "PSPS"
        ),
        lag_type = "lag\u2080\u208B\u2083"
      ) %>%
      mutate(Exposure = factor(Exposure, levels = exp_levels))
  }
  
  # Combine the datasets based on what's provided
  if (all(datasets_provided)) {
    # Both datasets provided
    combined_data <- bind_rows(data_same_day_processed, data_lag4_processed) %>%
      mutate(lag_type = factor(lag_type, levels = c("Same day", "lag\u2080\u208B\u2083")),
             Cause = case_when(
               Cause == "Respiratory" ~ "All-cause respiratory",
               TRUE ~ Cause
             ),
             Cause = factor(Cause, levels = c("All-cause respiratory", "COPD", "Cardiovascular", "Psychiatric")))
    
    # Set up scales for both types (matching supplemental style)
    alpha_scale <- scale_alpha_manual(values = c("Same day" = 1.0, "lag\u2080\u208B\u2083" = 0.6), 
                                     name = "", 
                                     labels = list("Same day", expression(lag["0-3"])))
    shape_scale <- scale_shape_manual(values = c("Same day" = 16, "lag\u2080\u208B\u2083" = 17),
                                     name = "",
                                     labels = list("Same day", expression(lag["0-3"])))
  } else if (datasets_provided[1]) {
    # Only same_day data provided
    combined_data <- data_same_day_processed %>%
      mutate(Cause = case_when(
               Cause == "Respiratory" ~ "All-cause respiratory",
               TRUE ~ Cause
             ),
             Cause = factor(Cause, levels = c("All-cause respiratory", "COPD", "Cardiovascular", "Psychiatric")))
    
    # No need for alpha or shape scales
    alpha_scale <- NULL
    shape_scale <- NULL
  } else if (datasets_provided[2]) {
    # Only lag4 data provided
    combined_data <- data_lag4_processed %>%
      mutate(Cause = case_when(
               Cause == "Respiratory" ~ "All-cause respiratory",
               TRUE ~ Cause
             ),
             Cause = factor(Cause, levels = c("All-cause respiratory", "COPD", "Cardiovascular", "Psychiatric")))
    
    # No need for alpha or shape scales
    alpha_scale <- NULL
    shape_scale <- NULL
  } else {
    stop("At least one dataset (data_same_day or data_lag4) must be provided")
  }
  
  # color mapping (use variable for Unicode string)
  exposure_colors <- setNames(
    c(pal[3], pal[2], pal[1], "#013220"),
    c(wf_pm_label, "PSPS", "Multiplicative interaction*", "Joint effect")
  )
  
  # Define labels for axes and legends
  wf_expr_label <- expression("WF PM"["2.5"]*" (per 10 "*mu*"g/m"^3*")")
  exposure_labels <- setNames(
    c("PSPS", wf_expr_label, "Multiplicative interaction*", "Joint effect"),
    c("PSPS", wf_pm_label, "Multiplicative interaction*", "Joint effect")
  )
  
  
  # base plot - conditional aesthetics based on number of datasets
  if (all(datasets_provided)) {
    # Both datasets - use alpha and shape
    p <- ggplot(combined_data, aes(x = Exposure, y = odds_ratio, ymin = lower_ci, ymax = upper_ci)) +
      geom_point(aes(color = Exposure, alpha = lag_type, shape = lag_type), 
                 position = position_dodge(width = 0.6), 
                 size = 3) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = Exposure, alpha = lag_type), 
                    width = 0.3,
                    position = position_dodge(width = 0.6)) +
      geom_hline(yintercept = 1, linetype = "dashed") + 
      scale_color_manual(values = exposure_colors, name = NULL, labels = exposure_labels) +
      scale_x_discrete(labels = exposure_labels) +
      alpha_scale +
      shape_scale +
      labs(
        x = "",
        y = if (!is.null(panel_label)) substitute(atop(bold(lbl), "Odds Ratio"), list(lbl = panel_label)) else "Odds Ratio"
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
      scale_y_log10(breaks = c(0.75, 1.0, 1.25, 1.5, 2.0, 3.0)) +
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
      scale_color_manual(values = exposure_colors, name = NULL, labels = exposure_labels) +
      scale_x_discrete(labels = exposure_labels) +
      labs(
        x = "",
        y = if (!is.null(panel_label)) substitute(atop(bold(lbl), "Odds Ratio"), list(lbl = panel_label)) else "Odds Ratio"
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
      scale_y_log10(breaks = c(0.75, 1.0, 1.25, 1.5, 2.0, 3.0)) +
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

#-------------------------------------------------
# 1-week duration sensitivity analysis figures
#-------------------------------------------------

# Helper to load 1-week duration results for a given age group
# (Single set of results - no abs/hyb split; file has duplicate rows so we take first 7)
load_1week_results <- function(age_group, results_dir = results_data_dir) {
  causes <- c(resp = "Respiratory", copd = "COPD", cardio = "Cardiovascular", psych = "Psychiatric")
  exposures_keep <- c(
    "outage_hours_lag7_per8",
    "mean_lag0_lag6_per10",
    "outage_hours_lag7_per8:mean_lag0_lag6_per10"
  )
  
  all_data <- list()
  for (i in seq_along(causes)) {
    cause_code <- names(causes)[i]
    cause_display <- causes[i]
    f <- file.path(results_dir, paste0("results_", cause_code, "_1week_duration_", age_group, ".csv"))
    if (!file.exists(f)) next
    df <- read.csv(f)
    df <- df[1:7, ] %>%  # First 7 rows only (one set of results)
      filter(Exposure %in% exposures_keep) %>%
      mutate(
        Cause = cause_display,
        odds_ratio = as.numeric(OR),
        lower_ci = as.numeric(CI_Lower),
        upper_ci = as.numeric(CI_Upper)
      ) %>%
      select(Exposure, odds_ratio, lower_ci, upper_ci, Cause)
    all_data[[length(all_data) + 1]] <- df
  }
  
  if (length(all_data) == 0) return(NULL)
  bind_rows(all_data)
}

# Plotting function for 1-week duration results - single line with circles
create_results_fig_1week <- function(data, 
                                    show_disease_labels = TRUE, 
                                    show_legend = TRUE,
                                    panel_label = NULL) {
  
  wf_pm_label <- "WF PM\u2082.\u2085 (per 10 \u03BCg/m\u00B3)"
  exp_levels <- c("PSPS (per 8 hr)", wf_pm_label, "Multiplicative interaction*")
  
  plot_data <- data %>%
    mutate(
      Exposure = case_when(
        Exposure == "outage_hours_lag7_per8" ~ "PSPS (per 8 hr)",
        Exposure == "mean_lag0_lag6_per10" ~ wf_pm_label,
        Exposure == "outage_hours_lag7_per8:mean_lag0_lag6_per10" ~ "Multiplicative interaction*",
        TRUE ~ Exposure
      ),
      Exposure = factor(Exposure, levels = exp_levels),
      Cause = case_when(
        Cause == "Respiratory" ~ "All-cause respiratory",
        TRUE ~ Cause
      ),
      Cause = factor(Cause, levels = c("All-cause respiratory", "COPD", "Cardiovascular", "Psychiatric"))
    )
  
  exposure_colors <- setNames(
    c(pal[2], pal[3], pal[1]),
    exp_levels
  )
  
  wf_expr_label <- expression("WF PM"["2.5"]*" (per 10 "*mu*"g/m"^3*")")
  exposure_labels <- setNames(
    c("PSPS (per 8 hr)", wf_expr_label, "Multiplicative interaction*"),
    exp_levels
  )
  
  p <- ggplot(plot_data, aes(x = Exposure, y = odds_ratio, ymin = lower_ci, ymax = upper_ci)) +
    geom_point(aes(color = Exposure), size = 3, shape = 16) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = Exposure), width = 0.3) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_color_manual(values = exposure_colors, name = NULL, labels = exposure_labels) +
    scale_x_discrete(labels = exposure_labels) +
    labs(
      x = "",
      y = if (!is.null(panel_label)) substitute(atop(bold(lbl), "Odds Ratio"), list(lbl = panel_label)) else "Odds Ratio"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y = element_text(size = 16),
      axis.text.y = element_text(size = 14),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.spacing.y = unit(0.3, "cm"),
      strip.text = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "gray85")
    ) +
    # scale_y_log10(
    #   breaks = c(0.8, 0.9, 1.0, 1.1, 1.2, 1.5, 2.0, 3.0),
    #   limits = c(0.78, 2.2)
    # ) +
    facet_wrap(~Cause, nrow = 1)
  
  if (!show_disease_labels) {
    p <- p + theme(strip.text = element_blank())
  }
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}

#-------------------------------------------------
# Continuous PSPS exposure sensitivity analysis figures (June 2026 results)
#-------------------------------------------------

load_continuous_psps_results <- function(lag_type, results_dir = jun2026_results_dir) {
  causes <- c(resp = "Respiratory", copd = "COPD", cardio = "Cardiovascular", psych = "Psychiatric")
  psps_per_n_customers <- 1000L
  rescale_or <- function(x, n) exp(log(x) * n)

  if (lag_type == "same_day") {
    lag_suffix <- "same_day"
    psps_exp <- "total_customers_impacted"
    wf_exp <- "wf_pm25_per10"
    int_exp <- "total_customers_impacted:wf_pm25_per10"
  } else if (lag_type == "lag4") {
    lag_suffix <- "lag4"
    psps_exp <- "total_customers_impacted_lag4"
    wf_exp <- "mean_lag0_lag3_per10"
    int_exp <- "total_customers_impacted_lag4:mean_lag0_lag3_per10"
  } else {
    stop("lag_type must be 'same_day' or 'lag4'")
  }

  exposures_keep <- c(psps_exp, wf_exp, int_exp)
  all_data <- list()

  for (i in seq_along(causes)) {
    cause_code <- names(causes)[i]
    cause_display <- causes[i]
    f <- file.path(results_dir, paste0("results_", cause_code, "_", lag_suffix, "_linear_age 20 and older.csv"))
    if (!file.exists(f)) next
    df <- read.csv(f) %>%
      filter(Exposure %in% exposures_keep) %>%
      mutate(
        Cause = cause_display,
        odds_ratio = as.numeric(OR),
        lower_ci = as.numeric(CI_Lower),
        upper_ci = as.numeric(CI_Upper),
        psps_term = grepl("total_customers_impacted", Exposure),
        odds_ratio = ifelse(psps_term, rescale_or(odds_ratio, psps_per_n_customers), odds_ratio),
        lower_ci = ifelse(psps_term, rescale_or(lower_ci, psps_per_n_customers), lower_ci),
        upper_ci = ifelse(psps_term, rescale_or(upper_ci, psps_per_n_customers), upper_ci)
      ) %>%
      select(Exposure, odds_ratio, lower_ci, upper_ci, Cause)
    all_data[[length(all_data) + 1]] <- df
  }

  if (length(all_data) == 0) return(NULL)
  bind_rows(all_data)
}

create_results_fig_continuous_psps <- function(data_same_day = NULL,
                                               data_lag4 = NULL,
                                               show_disease_labels = TRUE,
                                               show_legend = TRUE) {
  wf_pm_label <- "WF PM\u2082.\u2085 (per 10 \u03BCg/m\u00B3)"
  psps_label <- "PSPS (per 1,000 customers)"
  exp_levels <- c(psps_label, wf_pm_label, "Multiplicative interaction*")

  relabel_exposure <- function(df, lag_label) {
    df %>%
      mutate(
        Exposure = case_when(
          grepl("total_customers_impacted", Exposure) & grepl(":", Exposure) ~ "Multiplicative interaction*",
          grepl("total_customers_impacted", Exposure) ~ psps_label,
          TRUE ~ wf_pm_label
        ),
        Exposure = factor(Exposure, levels = exp_levels),
        lag_type = lag_label
      )
  }

  datasets_provided <- c(!is.null(data_same_day), !is.null(data_lag4))
  if (!any(datasets_provided)) stop("At least one dataset must be provided")

  combined_data <- bind_rows(
    if (datasets_provided[1]) relabel_exposure(data_same_day, "Same day") else NULL,
    if (datasets_provided[2]) relabel_exposure(data_lag4, "lag\u2080\u208B\u2083") else NULL
  ) %>%
    mutate(
      lag_type = if (all(datasets_provided)) {
        factor(lag_type, levels = c("Same day", "lag\u2080\u208B\u2083"))
      } else {
        lag_type
      },
      Cause = case_when(
        Cause == "Respiratory" ~ "All-cause respiratory",
        TRUE ~ Cause
      ),
      Cause = factor(Cause, levels = c("All-cause respiratory", "COPD", "Cardiovascular", "Psychiatric"))
    )

  exposure_colors <- setNames(
    c(pal[2], pal[3], pal[1]),
    exp_levels
  )
  wf_expr_label <- expression("WF PM"["2.5"]*" (per 10 "*mu*"g/m"^3*")")
  exposure_labels <- setNames(
    c("PSPS (per 1,000 customers)", wf_expr_label, "Multiplicative interaction*"),
    exp_levels
  )

  p <- ggplot(combined_data, aes(x = Exposure, y = odds_ratio, ymin = lower_ci, ymax = upper_ci))

  if (all(datasets_provided)) {
    p <- p +
      geom_point(aes(color = Exposure, alpha = lag_type, shape = lag_type),
                 position = position_dodge(width = 0.6), size = 3) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = Exposure, alpha = lag_type),
                    width = 0.3, position = position_dodge(width = 0.6)) +
      scale_alpha_manual(values = c("Same day" = 1.0, "lag\u2080\u208B\u2083" = 0.6),
                         name = "",
                         labels = list("Same day", expression(lag["0-3"]))) +
      scale_shape_manual(values = c("Same day" = 16, "lag\u2080\u208B\u2083" = 17),
                         name = "",
                         labels = list("Same day", expression(lag["0-3"])))
  } else {
    p <- p +
      geom_point(aes(color = Exposure), position = position_dodge(width = 0.6), size = 3) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = Exposure),
                    width = 0.3, position = position_dodge(width = 0.6))
  }

  p <- p +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_color_manual(values = exposure_colors, name = NULL, labels = exposure_labels) +
    scale_x_discrete(labels = exposure_labels) +
    labs(x = "", y = "Odds Ratio") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y = element_text(size = 16),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.spacing.y = unit(0.3, "cm"),
      strip.text = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "gray85")
    ) +
    facet_wrap(~Cause, nrow = 1)

  if (!show_disease_labels) p <- p + theme(strip.text = element_blank())
  if (!show_legend) p <- p + theme(legend.position = "none")

  p
}

# read in model results and covariance matrices ---------------------------------
# these were processed and created in the 01_process_results.R script
cov_matrices <- readRDS(file.path(results_dir, "cov_matrices_jan2026.rds"))

# Set parameters for main analysis
metric_type_main <- "abs"  # or "hyb" for relative
age_group_main <- "age 20 and older"  # "all ages"


# # read in exp data -------------------------------------------------
og_psps_dataset <- read.csv(file.path(data_dir, "ca_ZIP_daily_psps_no_washout_wf_classified_2013-2022.csv"))

# read in exposure dataset for fig2
# we need the number of zip-days for PSPS exp, WF exp, and dual exp
exp_data <- read.csv(file.path(exp_dir, "zip_daily_psps_wf_exposure.csv"))

# read in map data -------------------------------------------------
# load data -------------------------------------------------
zips_all <- read.csv(file.path(results_dir, "zipcodes_in_analysis_by_endpoint.csv"))
zips <- zips_all$ZIP_CODE

ca_shp <- st_read(paste0("~/Desktop/Desktop/epidemiology_PhD/01_data/raw/census_tiger/tl_2024_us_state.shp")) %>% 
  filter(STUSPS == "CA") %>% 
  st_transform(crs = 3310)
# ca_shp <- tigris::states(cb = TRUE, year = 2020) %>% 
#   filter(NAME == "California") %>% 
#   st_transform(epsg = 3310)
# zcta_shp <- tigris::zctas(cb = TRUE, year = 2020) %>% 
#     rename(zcta = ZCTA5CE20) %>% 
#     st_transform(crs = 3310) %>% 
#     select(zcta, geometry) %>% 
#     # filter to those that intersect with CA
#     st_intersection(ca_shp) %>%
#     select(zcta, geometry) %>% 
#     mutate(fill_flag = zcta %in% zctas)
zip_shp <- st_read(file.path(exp_dir, "ca_zip.geojson")) %>% 
            rename(zip_code = ZIP_CODE) %>%
            select(c("zip_code", "geometry")) %>% 
    st_transform(crs = 3310) %>% 
    select(zip_code, geometry) %>% 
    # filter to those that intersect with CA
    st_intersection(ca_shp) %>%
    select(zip_code, geometry) %>% 
    mutate(fill_flag = zip_code %in% zips)
# get rid of the water geometries
ca_shp <- ca_shp %>% st_intersection(zip_shp)

# make figs -------------------------------------------------

###################################
### MAP AND VIOLIN PANELLED PLOT ###
###################################
# This is a map + violin plot of the number of zip-days for each exposure type. 

# fig 1 -------------------------------------------------
# get the number of zipcode-days for each exposure type
exp_summary <- exp_data %>% 
    filter(zip_code %in% zips) %>% 
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
    labs(title = switch(exp_type,
                      "WF smoke only" = expression("B. Wildfire "~PM["2.5"]~"only"),
                      "PSPS event only" = "A. PSPS event only", 
                      "WF smoke + PSPS event" = expression("C. Wildfire "~PM["2.5"]~"+ PSPS event"))) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 16, hjust = 0),
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

# make each panel separately with labels
map_violin_panel1 <- map_plots[[1]] / violin_plots[[1]] + 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
map_violin_panel2 <- map_plots[[2]] / violin_plots[[2]] + 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
map_violin_panel3 <- map_plots[[3]] / violin_plots[[3]] + 
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
# PSPS seasonality with co-exposure ------------------------------------
monthly_summary <- exp_data %>%
  mutate(
    date = as.Date(date),
    month = lubridate::month(date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(month) %>%
  summarise(
    n_coexposure = sum(psps_event == 1 & wf > 0, na.rm = TRUE),
    n_psps_only = sum(psps_event == 1, na.rm = TRUE) - n_coexposure,
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(n_coexposure, n_psps_only),
    names_to = "type",
    values_to = "n_events"
  ) %>%
  mutate(type = factor(type, levels = c("n_psps_only", "n_coexposure")))

monthly_totals <- monthly_summary %>%
  group_by(month) %>%
  summarise(total = sum(n_events))

seasonality_plot <- ggplot(monthly_summary, aes(x = month, y = n_events, fill = type)) +
  geom_col(alpha = 0.8, width = 1) +
  geom_text(data = monthly_totals, aes(x = month, y = total, label = total), 
            vjust = -0.5, size = 6, inherit.aes = FALSE) + 
  scale_fill_manual(
    values = c("n_psps_only" = pal[2], "n_coexposure" = pal[3]),
    labels = c("PSPS only", expression("PSPS + WF PM"["2.5"]*" co-exposure"))
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.5),
    plot.margin = margin(20, 20, 5, 20),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) +
  labs(x = "", y = "Number of zip code-event days") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = scales::pretty_breaks(n = 6))

# WFS seasonality with co-exposure -------------------------------------
monthly_summary_wf <- exp_data %>%
  mutate(
    date = as.Date(date),
    month = lubridate::month(date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(date, month) %>%
  summarise(
    mean_wf_with_psps = mean(ifelse(psps_event == 1, wf, 0), na.rm = TRUE),
    mean_wf_only = mean(ifelse(psps_event == 0 | is.na(psps_event), wf, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(month) %>%
  summarise(
    wf_with_psps = mean(mean_wf_with_psps, na.rm = TRUE),
    wf_only = mean(mean_wf_only, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(wf_with_psps, wf_only),
    names_to = "type",
    values_to = "mean_wf"
  ) %>%
  mutate(type = factor(type, levels = c("wf_only", "wf_with_psps")))

monthly_totals_wf <- monthly_summary_wf %>%
  group_by(month) %>%
  summarise(total = sum(mean_wf))

seasonality_plot_wf <- ggplot(monthly_summary_wf, aes(x = month, y = mean_wf, fill = type)) +
  geom_col(alpha = 0.8, width = 1) +
  geom_text(data = monthly_totals_wf, aes(x = month, y = total, label = round(total, 2)), 
            vjust = -0.5, size = 6, inherit.aes = FALSE) + 
  scale_fill_manual(
    values = c("wf_only" = pal[3], "wf_with_psps" = pal[2]),
    labels = c(expression("WF PM"["2.5"]*" only"), expression("WF PM"["2.5"]*" + PSPS co-exposure"))
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) +
  labs(x = "", y = expression("Mean wildfire PM"["2.5"]* " ("*mu*"g/m"^3*")")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = scales::pretty_breaks(n = 6))


# patchwork with common x axis and common title 
seasonality_plot_combined <- seasonality_plot / seasonality_plot_wf + 
  plot_layout(ncol = 1)


# 2. Figure E1: make these stacked bar charts so that both show the overall number of days of psps and wf, respecitvely, but also a subset of the bars show the amount with co-exposure. so the psps panel shows the overall number of psps days but also the number of psps days with cooccurring wf as the bottom of the stacked bar. 

  
####################    
### RESULTS FIG ###
####################
# This is a plot of our results. 

# prep for results figure -------------------------------------------
# Load and prepare data for main results (severe only, all ages)
severe_data <- load_and_prepare_data(metric_type = metric_type_main, 
                                     age_group = age_group_main, 
                                     severity_level = "Severe", 
                                     cov_matrices = cov_matrices)

# make results figure -------------------------------------------
# Main results: severe only, showing same_day and lag4
results_fig <- create_results_fig_combined(severe_data$same_day, severe_data$lag4)

# write out numbers for table 
if (!is.null(severe_data$same_day)) {
  write.csv(severe_data$same_day, file.path(results_dir, paste0("severe_df_same_day_", metric_type_main, ".csv")), row.names = FALSE)
}

##########################################
### Map of ZCTAS included in analysis ###
##########################################
# This is a map of the ZCTAs in California that were included in this analysis. 

# plot -------------------------------------------------
color_mapping <- c(`TRUE` = pal[3], `FALSE` = "white")

zips_included_map <- ggplot() +
  geom_sf(data = ca_shp, fill = "white", color = alpha("black", 0.2), stroke = 0.1) +
  geom_sf(data = zip_shp, aes(fill = fill_flag), color = alpha("black", 0.2), stroke = 0.1) +
  scale_fill_manual(values = color_mapping) +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

########################
### AGE-STRATIFIED SUPP RESULTS FIGS ###
########################
# Three panels = three age groups (all, 20-64, 65+); one file per severity x metric combo
# Files: supp_results_fig_mild_abs, supp_results_fig_mild_hyb, supp_results_fig_mod_abs, etc.

age_groups <- list(
  all = list(id = "age 20 and older", label = "All ages (20+)"),
  young = list(id = "20-64 years", label = "20-64 years"),
  old = list(id = "65 and older", label = "65 and older")
)

severity_metric_combos <- list(
  list(severity = "Mild", metric = "abs", file = "supp_results_fig_mild_abs"),
  list(severity = "Mild", metric = "hyb", file = "supp_results_fig_mild_hyb"),
  list(severity = "Moderate", metric = "abs", file = "supp_results_fig_mod_abs"),
  list(severity = "Moderate", metric = "hyb", file = "supp_results_fig_mod_hyb"),
  list(severity = "Severe", metric = "abs", file = "supp_results_fig_sev_abs"),
  list(severity = "Severe", metric = "hyb", file = "supp_results_fig_sev_hyb")
)

supp_results_figs <- lapply(severity_metric_combos, function(sm) {
  plots <- lapply(seq_along(age_groups), function(i) {
    ag <- age_groups[[i]]
    data_list <- load_and_prepare_data(
      metric_type = sm$metric,
      age_group = ag$id,
      severity_level = sm$severity,
      cov_matrices = cov_matrices
    )
    create_results_fig_combined(
      data_list$same_day, data_list$lag4,
      show_disease_labels = (i == 1),
      show_legend = (i == length(age_groups)),
      panel_label = ag$label
    )
  })
  combined <- plots[[1]] / plots[[2]] / plots[[3]]
  list(fig = combined, file = sm$file)
})


########################
### 1-WEEK DURATION SENSITIVITY FIGS ###
########################

# Create 1-week duration figure: three panels (one per age group), like supp results figs
age_groups_1week <- list(
  list(id = "age 20 and older", label = "All ages (20+)"),
  list(id = "20-64 years", label = "20-64 years"),
  list(id = "65 and older", label = "65 and older")
)
results_fig_1week_plots <- lapply(seq_along(age_groups_1week), function(i) {
  ag <- age_groups_1week[[i]]
  create_results_fig_1week(
    load_1week_results(ag$id),
    show_disease_labels = (i == 1),
    show_legend = (i == length(age_groups_1week)),
    panel_label = ag$label
  )
})
results_fig_1week <- (results_fig_1week_plots[[1]] / results_fig_1week_plots[[2]] / results_fig_1week_plots[[3]]) +
  plot_layout(axes = "collect_y") 


########################
### CONTINUOUS PSPS EXPOSURE SENSITIVITY FIG ###
########################

continuous_psps_same_day <- load_continuous_psps_results("same_day")
continuous_psps_lag4 <- load_continuous_psps_results("lag4")
results_fig_continuous_psps <- create_results_fig_continuous_psps(
  continuous_psps_same_day,
  continuous_psps_lag4
)


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
ggsave(file.path(out_dir, "zips_included_map.pdf"), zips_included_map, width = 8, height = 10, dpi = 100)
ggsave(file.path(out_dir, "map_violin_panel1.pdf"), map_violin_panel1, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(file.path(out_dir, "map_violin_panel2.pdf"), map_violin_panel2, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(file.path(out_dir, "map_violin_panel3.pdf"), map_violin_panel3, width = 5, height = 10, dpi = 100, bg="transparent")
ggsave(file.path(out_dir, "results_fig.pdf"), results_fig, width = 10, height = 10, dpi = 100, device = cairo_pdf)
ggsave(file.path(out_dir, "duration_hist.pdf"), duration_hist, width = 15, height = 7, dpi = 100)
ggsave(file.path(out_dir, "seasonality_plot.pdf"), seasonality_plot_combined, width = 15, height = 11, dpi = 100, device = cairo_pdf)

# Save age-stratified supp results figures (3 panels = age groups, 6 files)
for (i in seq_along(supp_results_figs)) {
  ggsave(file.path(out_dir, paste0(supp_results_figs[[i]]$file, ".pdf")),
         supp_results_figs[[i]]$fig, width = 10, height = 13, dpi = 100, device = cairo_pdf)
}

# Save 1-week duration figure (3 panels = age groups)
ggsave(file.path(out_dir, "results_fig_1week.pdf"), results_fig_1week, width = 10, height = 13, dpi = 100, device = cairo_pdf)

# Save continuous PSPS exposure sensitivity figure (all ages 20+, absolute metric only)
ggsave(file.path(out_dir, "supp_results_fig_continuous_psps.pdf"), results_fig_continuous_psps, width = 10, height = 10, dpi = 100, device = cairo_pdf)
