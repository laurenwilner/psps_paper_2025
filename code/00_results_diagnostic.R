#-------------------------------------------------
# PSPS: Process analytic results - January 2026
# New results with lag and age group metadata
#-------------------------------------------------

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(tidyverse, ggforce, MetBrewer)

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/jan_2026_results/case_crossover_results")
plots_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/jan_2026_results/plots/")

# Create plots directory if it doesn't exist
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

# read in and concat results to visualize -----------------------------
# pull directories of results and construct file names
files <- list.files(results_dir, full.names = TRUE, recursive = TRUE)
files <- str_subset(files, "results_.*\\.csv$")
# Exclude 1week_duration files
files <- str_subset(files, "1week_duration", negate = TRUE)

# Function to parse file name and extract metadata
parse_filename <- function(filename) {
  # Remove path and extension
  f <- gsub(".*/", "", filename)
  f <- gsub(".csv", "", f)
  f <- gsub("results_", "", f)
  
  # Extract cause (first part before underscore)
  cause <- str_extract(f, "^[^_]+")
  
  # Extract lag type (same_day or lag4)
  lag_type <- ifelse(grepl("same_day", f), "same_day", 
                     ifelse(grepl("lag4", f), "lag4", NA))
  
  # Extract age group
  age_group <- ifelse(grepl("20-64 years", f), "20-64 years",
                      ifelse(grepl("65 and older", f), "65 and older",
                             ifelse(grepl("age 20 and older", f), "age 20 and older", NA)))
  
  # Create simplified labels
  lag_label <- case_when(
    lag_type == "same_day" ~ "Same day",
    lag_type == "lag4" ~ "4-day lag",
    TRUE ~ lag_type
  )
  
  age_label <- case_when(
    age_group == "20-64 years" ~ "Young (20-64)",
    age_group == "65 and older" ~ "Old (65+)",
    age_group == "age 20 and older" ~ "All ages (20+)",
    TRUE ~ age_group
  )
  
  # Map cause to display name
  cause_display <- case_when(
    cause == "cardio" ~ "Cardiovascular",
    cause == "copd" ~ "COPD",
    cause == "psych" ~ "Psychiatric",
    cause == "resp" ~ "Respiratory",
    TRUE ~ cause
  )
  
  return(list(
    cause = cause_display,
    lag_type = lag_type,
    lag_label = lag_label,
    age_group = age_group,
    age_label = age_label
  ))
}

# read in and create columns for metadata
results <- data.frame()
for(f in files){
  print(f)
  df <- read_csv(f, show_col_types = FALSE)
  
  # drop random row name cols
  df <- df %>% 
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper", "p")) %>% 
    rename_all(tolower)
  
  # Parse filename to get metadata
  metadata <- parse_filename(f)
  
  # Add metadata columns
  df$cause <- metadata$cause
  df$lag_type <- metadata$lag_type
  df$lag_label <- metadata$lag_label
  df$age_group <- metadata$age_group
  df$age_label <- metadata$age_label
  
  results <- rbind(results, df)
}

# Filter out spline terms (ns() terms)
results <- results %>%
  filter(!grepl("^ns\\(|^ns\\.", exposure, ignore.case = TRUE)) %>%
  # Add alpha column: 1.0 for severe variables, 0.3 for others
  mutate(alpha_val = ifelse(grepl("severe", exposure, ignore.case = TRUE), 1.0, 0.3))

# make plots for each combination --------------------------------
# Create individual plots for each cause, faceted by temporality and age
for(cause_name in unique(results$cause)) {
  plot_data <- results %>%
    filter(cause == cause_name) %>%
    # Create temporality variable and clean exposure names for stacking
    mutate(
      temporality = ifelse(lag_type == "lag4", "lag4", "same_day"),
      # Remove "_lag4" or "lag4" from exposure names so they stack with same_day versions
      # This makes severity_customers_lag4mild -> severity_customersmild to match same_day
      # Handle both underscore and no-underscore cases
      exposure_clean = gsub("_lag4", "", exposure),
      exposure_clean = gsub("lag4", "", exposure_clean)
    )
  
  p3 <- plot_data %>%
    ggplot(aes(x = exposure_clean, y = or, color = age_label)) + 
    geom_point(aes(alpha = alpha_val), size = 2.5) + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = age_label, alpha = alpha_val), 
                  width = 0.5, linewidth = 0.8) + 
    geom_hline(yintercept = 1, linetype = "dashed") + 
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    labs(x = "Variable", y = "Odds Ratio", 
         color = "Age Group",
         title = paste("Results for", cause_name)) + 
    facet_grid(temporality ~ age_label, scales = "free_x", drop = TRUE) + 
    theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5),   
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    scale_color_met_d("Hokusai3") + 
    scale_alpha_identity(guide = "none") +
    scale_y_continuous()
  
  # Save individual plots
  cause_file <- tolower(gsub(" ", "_", cause_name))
  ggsave(paste0(plots_dir, "results_jan2026_", cause_file, ".pdf"), p3, height = 10, width = 15)
}

# Print summary of what was processed
cat("\nProcessed", length(files), "result files\n")
cat("Causes:", paste(unique(results$cause), collapse = ", "), "\n")
cat("Lag types:", paste(unique(results$lag_label), collapse = ", "), "\n")
cat("Age groups:", paste(unique(results$age_label), collapse = ", "), "\n")

#-------------------------------------------------
# Process 1week_duration files --------------------------------
#-------------------------------------------------

# Get 1week_duration files
files_1week <- list.files(results_dir, full.names = TRUE, recursive = TRUE)
files_1week <- str_subset(files_1week, "results_.*\\.csv$")
files_1week <- str_subset(files_1week, "1week_duration")

# Read in and process 1week_duration files
results_1week <- data.frame()
for(f in files_1week){
  print(f)
  df <- read_csv(f, show_col_types = FALSE)
  
  # drop random row name cols
  df <- df %>% 
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper", "p")) %>% 
    rename_all(tolower)
  
  # Parse filename to get metadata
  metadata <- parse_filename(f)
  
  # Add metadata columns
  df$cause <- metadata$cause
  df$age_group <- metadata$age_group
  df$age_label <- metadata$age_label
  
  results_1week <- rbind(results_1week, df)
}

# Filter out spline terms
results_1week <- results_1week %>%
  filter(!grepl("^ns\\(|^ns\\.", exposure, ignore.case = TRUE))

# Create plots for 1week_duration - one per cause, faceted by age only
for(cause_name in unique(results_1week$cause)) {
  p_1week <- results_1week %>%
    filter(cause == cause_name) %>%
    ggplot(aes(x = exposure, y = or, color = age_label)) + 
    geom_point(size = 2.5) + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = age_label), 
                  width = 0.5, linewidth = 0.8) + 
    geom_hline(yintercept = 1, linetype = "dashed") + 
    theme_minimal() + 
    theme(legend.position = "bottom") + 
    labs(x = "Variable", y = "Odds Ratio", 
         color = "Age Group",
         title = paste("Results for", cause_name, "- 1 week duration")) + 
    facet_wrap(~age_label, scales = "free_x", drop = TRUE) + 
    theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5),   
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    scale_color_met_d("Hokusai3") + 
    scale_y_continuous()
  
  # Save individual plots
  cause_file <- tolower(gsub(" ", "_", cause_name))
  ggsave(paste0(plots_dir, "results_jan2026_1week_", cause_file, ".pdf"), p_1week, height = 8, width = 15)
}

# Print summary of 1week files processed
cat("\nProcessed", length(files_1week), "1week_duration result files\n")
cat("Causes:", paste(unique(results_1week$cause), collapse = ", "), "\n")
cat("Age groups:", paste(unique(results_1week$age_label), collapse = ", "), "\n")
