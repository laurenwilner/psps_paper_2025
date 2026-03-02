#-------------------------------------------------
# PSPS: Paper tables 
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
pacman::p_load(ggforce, MetBrewer, dplyr, tidyr, knitr, gt, magick, pagedown, readxl, readr, webshot2)

pal <- met.brewer(name = "Hokusai2", n=2)

# load functions -------------------------------------------------
source(file.path(code_dir, "00_helper_functions.R"))

# Load data - using jan_2026_results for lag-specific data
exp_summary_same_day <- read_csv(file.path(jan2026_results_dir, "psps_among_casedays_same_day.csv"), show_col_types = FALSE)
exp_summary_lag4 <- read_csv(file.path(jan2026_results_dir, "psps_among_casedays_lag4.csv"), show_col_types = FALSE)

# Standardize column names for lag4 data (it uses _lag4 suffix)
exp_summary_lag4 <- exp_summary_lag4 %>%
  rename(
    severity_customers = severity_customers_lag4,
    severity_customers_N = severity_customers_lag4_N,
    severity_hybrid = severity_hybrid_lag4,
    severity_hybrid_N = severity_hybrid_lag4_N
  )
wf_among_casedays_same_day <- read_csv(file.path(jan2026_results_dir, "wf_tmax_among_casedays_same_day.csv"), show_col_types = FALSE)
wf_among_casedays_lag4 <- read_csv(file.path(jan2026_results_dir, "wf_tmax_among_casedays_lag4.csv"), show_col_types = FALSE)

# Standardize column names for lag4 WF data (it uses mean_lag0_lag3_mean instead of wf_pm25_mean)
wf_among_casedays_lag4 <- wf_among_casedays_lag4 %>%
  rename(
    wf_pm25_mean = mean_lag0_lag3_mean,
    wf_pm25_SD = mean_lag0_lag3_SD
  )

# Load data that hasn't changed (copied to jan_2026_results)
ha_ed_table_df <- read_csv(file.path(jan2026_results_dir, "summary of events across data cleaning process_all_years.csv"), show_col_types = FALSE)
table1s_df_updated <- read_csv(file.path(jan2026_results_dir, "final dataset events by subgroup_all_years.csv"), show_col_types = FALSE) %>% 
    mutate(out = paste0(format(events, big.mark = ","), " (", proportion, "%)")) %>% 
    # remake race categories 
    mutate(category = ifelse(category == "female", "Female", category),
        category = ifelse(category == "male", "Male", category),
        category = ifelse(category == "other, invalid, unknown or missing", "Unknown, missing, or invalid", category),
        category = ifelse(category == "black", "Non-Hispanic Black",
          ifelse(category == "hispanic", "Hispanic",
          ifelse(category == "white", "Non-Hispanic White",
          ifelse(category == "asian", "Non-Hispanic Asian",
          ifelse(category == "Asian / Pacific Islander / Native Hawaiian", "Native Hawaiian or Other Pacific Islander",
          ifelse(category == "Native American / American Indian/Alaska Native/ Eskimo / Aleut", "American Indian or Alaska Native",
          ifelse(category == "other + multiracial", "Other or Multiracial",
          ifelse(category == "unknown, invalid, missing", "Unknown", category))))))))
        )
        

## NOTE: table of icd codes is made in the appendix in latex. 

# Helper function to process exposure summaries
process_exp_summary <- function(exp_data) {
  abs_table <- exp_data %>%
    select(c("severity_customers", "severity_customers_N", "outcome")) %>%
    group_by(severity_customers, outcome) %>%
    summarize(
      count = sum(severity_customers_N),
      .groups = "drop"
    ) %>%
    rename(
      Cause = outcome,
      Exposure = severity_customers
    ) %>%
    mutate(case_indicator = "Index days") %>%
    pivot_wider(
      names_from = case_indicator,
      values_from = count
    ) %>% 
    # rename causes (new data uses: resp, copd, cardio, psych; old data uses: adult_resp, etc.)
    mutate(Cause = ifelse(
      Cause == "cardio" | Cause == "adult_cardio", "Cardiovascular",
      ifelse(Cause == "resp" | Cause == "adult_resp", "Respiratory",
        ifelse(Cause == "psych" | Cause == "adult_psych", "Psychiatric",
          "COPD"))), 
      Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric")),
      Exposure = case_when(
        Exposure == "none" ~ "None",
        Exposure == "mild" ~ "Mild",
        Exposure == "moderate" ~ "Moderate",
        Exposure == "severe" ~ "Severe",
        TRUE ~ Exposure
      )
    ) %>%
    ungroup() %>%
    distinct()  %>% 
    mutate(Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric"))) %>%
    mutate(Exposure = factor(Exposure, levels = c("Mild", "Moderate", "Severe", "None"))) %>%
    arrange(Cause, Exposure)
  
  hyb_table <- exp_data %>%
    select(c("severity_hybrid", "severity_hybrid_N", "outcome")) %>%
    group_by(severity_hybrid, outcome) %>%
    summarize(
      count = sum(severity_hybrid_N),
      .groups = "drop"
    ) %>%
    rename(
      Cause = outcome,
      Exposure = severity_hybrid
    ) %>%
    mutate(case_indicator = "Index days") %>%
    pivot_wider(
      names_from = case_indicator,
      values_from = count
    ) %>% 
    # rename causes (new data uses: resp, copd, cardio, psych; old data uses: adult_resp, etc.)
    mutate(Cause = ifelse(
      Cause == "cardio" | Cause == "adult_cardio", "Cardiovascular",
      ifelse(Cause == "resp" | Cause == "adult_resp", "Respiratory",
        ifelse(Cause == "psych" | Cause == "adult_psych", "Psychiatric",
          "COPD"))), 
      Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric")),
      Exposure = case_when(
        Exposure == "none" ~ "None",
        Exposure == "mild" ~ "Mild",
        Exposure == "moderate" ~ "Moderate",
        Exposure == "severe" ~ "Severe",
        TRUE ~ Exposure
      )
    ) %>%
    ungroup() %>%
    distinct()  %>% 
    mutate(Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric"))) %>%
    mutate(Exposure = factor(Exposure, levels = c("Mild", "Moderate", "Severe", "None"))) %>%
    arrange(Cause, Exposure)
  
  return(list(abs = abs_table, hyb = hyb_table))
}

# Process exposure summaries for both lag types
exp_same_day <- process_exp_summary(exp_summary_same_day)
abs_table_same_day <- exp_same_day$abs
hyb_table_same_day <- exp_same_day$hyb

exp_lag4 <- process_exp_summary(exp_summary_lag4)
abs_table_lag4 <- exp_lag4$abs
hyb_table_lag4 <- exp_lag4$hyb

####################
### MAIN TABLES ####
####################
# Helper function to create exposure table
create_exposure_table <- function(abs_table_data, lag_label) {
  # Add "Exposure (PSPS)" rows under each cause for the single-metric table
  abs_table_with_exposure <- abs_table_data %>%
    # Add "Exposure (PSPS)" rows under each cause
    group_by(Cause) %>%
    group_modify(~ {
      # Get the first row for this cause
      first_row <- .x[1, ]
      # Create the "Exposure (PSPS)" row with NA values
      exposure_row <- first_row %>%
        mutate(Exposure = "Exposure (PSPS)",
               `Index days` = NA_real_)
      # Combine the exposure row with the original data
      bind_rows(exposure_row, .x)
    }) %>%
    ungroup()
  
  # pretty table
  pretty_exposure_table <- abs_table_with_exposure %>%
  # Add indentation to the Exposure values (but not for "Exposure (PSPS)" rows)
  mutate(Exposure = ifelse(Exposure == "Exposure (PSPS)", 
                           "Exposure (PSPS)", 
                           paste0("\u00A0\u00A0\u00A0\u00A0", Exposure))) %>%  # Add 2 spaces for indentation
  gt() %>%
  fmt_number(
    columns = c("Index days"),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  # Format missing values as blank instead of "NA"
  fmt_missing(
    columns = c("Index days"),
    missing_text = ""
  ) %>%
  # make first col wider
  cols_width(
    Exposure ~ px(200)  # adjust the pixel value as needed
  ) %>%
  # add spanner headers
  tab_spanner(
    label = "Absolute (primary)",
    columns = c("Index days")
  ) %>%
  # create row groups in REVERSE order
  tab_row_group(
    label = "Psychiatric",
    rows = Cause == "Psychiatric"
  ) %>%
  tab_row_group(
    label = "Cardiovascular",
    rows = Cause == "Cardiovascular"
  ) %>%
    tab_row_group(
    label = "COPD",
    rows = Cause == "COPD"
  ) %>%
  tab_row_group(
    label = "All-cause respiratory",
    rows = Cause == "Respiratory"
  ) %>%
  # hide the original Cause column
  cols_hide(columns = Cause) %>%
  cols_label(
    Exposure = "Disease endpoint",
    "Index days" = "Index days"
  ) %>%
  tab_options(
    row_group.font.weight = "bold",
    row_group.background.color = "#f7f7f7",
    # table border options
    table.border.top.color = "black",
    table.border.bottom.color = "black", 
    # Header border options
    heading.border.bottom.color = "black",
    # column labels border options
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    # body border options
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    # table body border top and bottom color
    table_body.border.top.color = "black",
    table_body.border.bottom.color = "black",
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    heading.border.bottom.width = px(1),
    column_labels.border.top.width = px(1),
    column_labels.border.bottom.width = px(1),
    row_group.border.top.width = px(1),
    row_group.border.bottom.width = px(1),
    table_body.border.top.width = px(1),
    table_body.border.bottom.width = px(1)
  ) %>% 
  # style options for row groups
  tab_options(
    row_group.font.weight = "bold",
    row_group.background.color = "#f7f7f7"
  ) %>%
  # style the header (column labels)
  tab_style(
    style = list(
      cell_fill(color = pal[1]),
      cell_text(weight = "bold")
    ),
    locations = list(
      cells_column_labels(),
      cells_column_spanners()
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = pal[1]),
      cell_text(weight = "bold")
    ),
    locations = cells_stubhead()
  ) %>%
    # Style the "Exposure (PSPS)" rows to match the indented rows
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = Exposure == "Exposure (PSPS)")
    )
  
  return(pretty_exposure_table)
}

# Create exposure tables for both lag types
pretty_exposure_table_same_day <- create_exposure_table(abs_table_same_day, "Same day")
pretty_exposure_table_lag4 <- create_exposure_table(abs_table_lag4, "4-day lag")

# Save same_day exposure table (PNG for color; PDF does not render correctly)
options(chromote.headless = "new")
pretty_exposure_table_same_day %>% 
  as_raw_html() %>% 
  cat(file = file.path(out_dir, "exposure_table_same_day.html"))
webshot2::webshot(
  url = file.path(out_dir, "exposure_table_same_day.html"),
  file = file.path(out_dir, "exposure_table_same_day.png"),
  zoom = 7,
  selector = "table",
  expand = c(1, 10, 1, 1)
)

# Save lag4 exposure table (PNG for color)
pretty_exposure_table_lag4 %>% 
  as_raw_html() %>% 
  cat(file = file.path(out_dir, "exposure_table_lag4.html"))
webshot2::webshot(
  url = file.path(out_dir, "exposure_table_lag4.html"),
  file = file.path(out_dir, "exposure_table_lag4.png"),
  zoom = 7,
  selector = "table",
  expand = c(1, 10, 1, 1)
)


#-------------------------------------------------
# make table of ha and ed counts by endpoint -------------------------------------------
ha_ed_table <- ha_ed_table_df %>%
  select(c("ed_final", "pdd_final", "outcome"))

# Helper function to format count and percentage
format_count_pct <- function(count, total) {
  paste0(format(count, big.mark = ","), " (", round(count / total * 100, 1), "%)")
}

# Create the table structure
pretty_ha_ed_table <- ha_ed_table %>%
  # Calculate totals for each outcome
  mutate(total = ed_final + pdd_final) %>%
  # Create formatted strings for each outcome
  mutate(
    ed_formatted = format_count_pct(ed_final, total),
    ha_formatted = format_count_pct(pdd_final, total)
  ) %>%
  # Pivot to wide format
  select(outcome, ed_formatted, ha_formatted) %>%
  pivot_wider(names_from = outcome, values_from = c(ed_formatted, ha_formatted)) %>%
  # Create final table structure - extract values properly
  {data.frame(
    row_name = c("Emergency department visit", "Hospital admission"),
    Respiratory = c(.$ed_formatted_resp, .$ha_formatted_resp),
    COPD = c(.$ed_formatted_copd, .$ha_formatted_copd),
    Cardiovascular = c(.$ed_formatted_cardio, .$ha_formatted_cardio),
    Psychiatric = c(.$ed_formatted_psych, .$ha_formatted_psych)
  )} %>%
  gt(rowname_col = "row_name") %>%
  # Set column labels with n (%) underneath
  cols_label(
    Respiratory = md("All-cause respiratory<br><span style='font-style: italic; font-weight: normal;'>n (%)</span>"),
    COPD = md("COPD<br><span style='font-style: italic; font-weight: normal;'>n (%)</span>"),
    Cardiovascular = md("Cardiovascular<br><span style='font-style: italic; font-weight: normal;'>n (%)</span>"), 
    Psychiatric = md("Psychiatric<br><span style='font-style: italic; font-weight: normal;'>n (%)</span>")
  ) %>%
  # Set column widths
  cols_width(
    Respiratory ~ px(200),
    COPD ~ px(200),
    Cardiovascular ~ px(200),
    Psychiatric ~ px(200)
  ) %>%
  # Make the table wider overall
  tab_options(
    table.width = pct(100),
    container.width = pct(100)
  ) %>%
  # Style the table
  tab_style(
    style = cell_fill(color = pal[1]),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  # Add borders
  tab_options(
    table.border.top.style = "solid",
    table.border.top.width = px(1),
    table.border.top.color = "black",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(1),
    table.border.bottom.color = "black",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black"
  )

# save table 
  # this is something webshot needs to work...
  options(chromote.headless = "new")

  # save the table as html using cat 
   pretty_ha_ed_table %>% 
    as_raw_html() %>% 
    cat(file = file.path(out_dir, "ha_ed_table.html"))
  
  # save the table as png (PDF does not show color correctly)
  webshot2::webshot(
    url = file.path(out_dir, "ha_ed_table.html"),
    file = file.path(out_dir, "ha_ed_table.png"),
    zoom = 7,
    selector = "table"
  )



# Helper function to create WF by outcome table
create_wf_by_outcome_table <- function(wf_data) {
  # Order: resp, copd, cardio, psych
  pretty_wf_by_outcome <- data.frame(
    row_name = "Mean (SD) wildfire PM\u2082.\u2085, \u03BCg/m\u00B3",
    Respiratory = paste0(round(wf_data$wf_pm25_mean[wf_data$outcome == "resp"], 2), " (", round(wf_data$wf_pm25_SD[wf_data$outcome == "resp"], 2), ")"),
    COPD = paste0(round(wf_data$wf_pm25_mean[wf_data$outcome == "copd"], 2), " (", round(wf_data$wf_pm25_SD[wf_data$outcome == "copd"], 2), ")"),
    Cardiovascular = paste0(round(wf_data$wf_pm25_mean[wf_data$outcome == "cardio"], 2), " (", round(wf_data$wf_pm25_SD[wf_data$outcome == "cardio"], 2), ")"),
    Psychiatric = paste0(round(wf_data$wf_pm25_mean[wf_data$outcome == "psych"], 2), " (", round(wf_data$wf_pm25_SD[wf_data$outcome == "psych"], 2), ")")
  ) %>%
    gt(rowname_col = "row_name") %>%
    # Set column labels
    cols_label(
      Respiratory = "All-cause respiratory",
      COPD = "COPD",
      Cardiovascular = "Cardiovascular", 
      Psychiatric = "Psychiatric"
    ) %>%
    # Set column widths
    cols_width(
      stub() ~ px(200),
      Respiratory ~ px(175),
      COPD ~ px(175),
      Cardiovascular ~ px(175),
      Psychiatric ~ px(175)
    ) %>%
    # Make the table wider overall
    tab_options(
      table.width = pct(100),
      container.width = pct(100)
    ) %>%
    # Style the table
    tab_style(
      style = cell_fill(color = pal[1]),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = cells_column_labels(columns = everything())
    ) %>%
    # Add borders
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = px(1),
      table.border.top.color = "black",
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(1),
      table.border.bottom.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black"
    )
  
  return(pretty_wf_by_outcome)
}

# Create WF by outcome tables for both lag types
pretty_wf_by_outcome_same_day <- create_wf_by_outcome_table(wf_among_casedays_same_day)
pretty_wf_by_outcome_lag4 <- create_wf_by_outcome_table(wf_among_casedays_lag4)

# Save same_day WF table
pretty_wf_by_outcome_same_day %>% 
  as_raw_html() %>% 
  cat(file = file.path(out_dir, "wf_by_outcome_same_day.html"))
pretty_wf_by_outcome_same_day %>% 
  gt::gtsave(filename = "wf_by_outcome_same_day.png", path = out_dir)

# Save lag4 WF table
pretty_wf_by_outcome_lag4 %>% 
  as_raw_html() %>% 
  cat(file = file.path(out_dir, "wf_by_outcome_lag4.html"))
pretty_wf_by_outcome_lag4 %>% 
  gt::gtsave(filename = "wf_by_outcome_lag4.png", path = out_dir)



#-------------------------------------------------
# make results table for just absolute results
# Updated to include both same_day and lag4

# Load data and covariance matrices
cov_matrices <- readRDS(file.path(jan2026_results_dir, "cov_matrices_jan2026.rds"))
all_results_abs <- read.csv(file.path(jan2026_results_dir, "all_results_abs_jan2026.csv"))

# Filter for age 20 and older
all_results_abs <- all_results_abs %>%
  filter(age_group == "age 20 and older")

# Process same_day data
same_day_data <- all_results_abs %>%
  filter(lag_type == "same_day") %>%
  select(Exposure, OR, CI_Lower, CI_Upper, Cause)

severe_df_same_day <- process_results("Severe", same_day_data, "abs", cov_matrices, lag_type = "same_day", age_group = "age 20 and older")

# Process lag4 data
lag4_data <- all_results_abs %>%
  filter(lag_type == "lag4") %>%
  select(Exposure, OR, CI_Lower, CI_Upper, Cause)

severe_df_lag4 <- process_results("Severe", lag4_data, "abs", cov_matrices, lag_type = "lag4", age_group = "age 20 and older")

# Combine both datasets
severe_df_abs <- bind_rows(
  severe_df_same_day %>% mutate(lag = "Same day"),
  severe_df_lag4 %>% mutate(lag = "4-day lag")
) %>%
  select(c("Exposure", "odds_ratio", "lower_ci", "upper_ci", "Cause", "lag")) %>%
  mutate(
    OR_CI = paste0(
      sprintf("%.2f", odds_ratio), 
      " (", 
      sprintf("%.2f", lower_ci), 
      ", ", 
      sprintf("%.2f", upper_ci), 
      ")"
    ),
    Cause = case_when(
      Cause == "Respiratory" ~ "All-cause respiratory",
      Cause == "COPD" ~ "COPD",
      Cause == "Cardiovascular" ~ "Cardiovascular",
      Cause == "Psychiatric" ~ "Psychiatric",
      TRUE ~ Cause
    ),
    Exposure = case_when(
      Exposure == "WF smoke" ~ "WF_PM25",
      grepl("combined", Exposure) ~ "Joint_effect",
      grepl("interaction only", Exposure) ~ "Interaction",
      TRUE ~ "PSPS"
    )
  ) %>%
  mutate(
    Cause = factor(Cause, levels = c("All-cause respiratory", "COPD", "Cardiovascular", "Psychiatric")),
    lag = factor(lag, levels = c("Same day", "4-day lag"))
  ) %>%
  select(Cause, lag, Exposure, OR_CI) %>%
  pivot_wider(names_from = Exposure, values_from = OR_CI) %>%
  arrange(Cause, lag)

# Create table with row grouping
pretty_severe_df_abs <- severe_df_abs %>%
  gt(groupname_col = "Cause") %>%
  tab_stubhead(label = "Outcome") %>%
  cols_label(
    lag = "",
    PSPS = "PSPS",
    WF_PM25 = html("WF PM<sub>2.5</sub><br>(per 10 \u03BCg/m\u00B3)"),
    Joint_effect = "Joint effect",
    Interaction = "Multiplicative interaction*"
  ) %>%
  tab_spanner(
    label = "Effect estimates, OR (95% CI)",
    columns = c(PSPS, WF_PM25, Joint_effect, Interaction)
  ) %>%
  sub_missing(columns = everything(), missing_text = "") %>%
  cols_hide(columns = Cause) %>%
  cols_width(
    lag ~ px(140),        # slightly wider to push PSPS over
    PSPS ~ px(150),
    WF_PM25 ~ px(200),
    Joint_effect ~ px(150),
    Interaction ~ px(200)
  ) %>%
  cols_align(align = "left", columns = lag) %>%
  cols_align(align = "center", columns = c(PSPS, WF_PM25, Joint_effect, Interaction)) %>%
  tab_options(
    table.width = pct(100),
    container.width = pct(100),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1)
  ) %>%
  tab_style(
    style = list(cell_fill(color = pal[1]), cell_text(weight = "bold")),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(cell_fill(color = pal[1]), cell_text(weight = "bold"), cell_borders(sides = "bottom", color = "black", weight = px(1))),
    locations = cells_column_spanners()
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = list(cell_fill(color = pal[1]), cell_text(weight = "bold")),
    locations = cells_stubhead()
  )

# save table 
  # this is something webshot needs to work...
  options(chromote.headless = "new")

  # save the table as html using cat 
   pretty_severe_df_abs %>% 
    as_raw_html() %>% 
    cat(file = file.path(out_dir, "results_table.html"))
  
  # save the table as png (PDF does not show color correctly)
  webshot2::webshot(
    url = file.path(out_dir, "results_table.html"),
    file = file.path(out_dir, "results_table.png"),
    zoom = 7,
    selector = "table"
  )


###########################
### SUPPLEMENTAL TABLES ###
###########################

#-------------------------------------------------
# make traditional table 1 for supplement -------------------------------------------
create_traditional_table1_updated <- function(data) {
  # Debug: print column names
  print("Available columns:")
  print(colnames(data))
  
  # First, reshape the data to wide format
  wide_data <- data %>%
    select(outcome, subgroup, category, out) %>%
    # Pivot to wide format
    pivot_wider(names_from = outcome, values_from = out, values_fill = "0 (0.0%)")
  
  # Create the table structure
  combined_data <- data.frame(
    Group = character(0),
    Respiratory = character(0),
    COPD = character(0),
    Cardiovascular = character(0),
    Psychiatric = character(0),
    stringsAsFactors = FALSE,
    row_type = character(0)
  )
  
  # Get unique subgroups
  subgroups <- unique(wide_data$subgroup)
  
  # Process each subgroup
  for (subgrp in subgroups) {
    subgrp_data <- wide_data %>% filter(subgroup == subgrp)
    
    # Add header row for subgroup
    header_row <- data.frame(
      Group = case_when(
        subgrp == "agecat" ~ "Age Group",
        subgrp == "sex" ~ "Sex", 
        subgrp == "race_grp" ~ "Race/Ethnicity"
      ),
      Respiratory = "",
      COPD = "",
      Cardiovascular = "",
      Psychiatric = "",
      stringsAsFactors = FALSE,
      row_type = "header"
    )
    
    # Create rows for categories within this subgroup
    category_rows <- data.frame(
      Group = subgrp_data$category,
      Respiratory = subgrp_data$resp %||% "0 (0.0%)",
      COPD = subgrp_data$copd %||% "0 (0.0%)", 
      Cardiovascular = subgrp_data$cardio %||% "0 (0.0%)",
      Psychiatric = subgrp_data$psych %||% "0 (0.0%)",
      stringsAsFactors = FALSE,
      row_type = "subgroup"
    )
    
    # Combine header and category rows
    section <- rbind(header_row, category_rows)
    combined_data <- rbind(combined_data, section)
  }
  
  # Create gt table
  visible_data <- combined_data %>% select(-row_type)
  gt_table <- gt(visible_data)
  
  # Get row indices for styling
  header_rows <- which(combined_data$row_type == "header")
  subgroup_rows <- which(combined_data$row_type == "subgroup")
  
  gt_table <- gt_table %>%
    cols_label(
      Group = "",
      Respiratory = "All-cause respiratory",
      COPD = "COPD", 
      Cardiovascular = "Cardiovascular",
      Psychiatric = "Psychiatric"
    ) %>%
    # Style header rows
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = "Group", rows = header_rows)
    ) %>%
    # Indent subgroup rows
    tab_style(
      style = cell_text(indent = px(15)),
      locations = cells_body(columns = "Group", rows = subgroup_rows)
    ) %>%
    # Style column headers
    tab_style(
      style = cell_fill(color = pal[1]),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = cells_column_labels(columns = everything())
    ) %>%
    # Add borders
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "black", 
        weight = px(1),
        style = "solid"
      ),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.width = px(1),
      table.border.top.color = "black",
      table.border.bottom.style = "solid", 
      table.border.bottom.width = px(1),
      table.border.bottom.color = "black",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      # Make table wider
      table.width = pct(100),
      container.width = pct(100)
    ) %>%
    # Set column widths to ensure everything fits on one line
    cols_width(
      Group ~ px(280),
      Respiratory ~ px(170),
      COPD ~ px(150), 
      Cardiovascular ~ px(150),
      Psychiatric ~ px(150)
    ) %>%
    # Right-align numeric columns
    cols_align(align = "right", columns = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric")) %>%
    # Left-align group column
    cols_align(align = "left", columns = "Group")
  
  return(gt_table)
}

# create and display the table
pretty_traditional_table1 <- create_traditional_table1_updated(table1s_df_updated)

# save table 
  # this is something webshot needs to work...
  options(chromote.headless = "new")

  # save the table as html using cat 
   pretty_traditional_table1 %>% 
    as_raw_html() %>% 
    cat(file = file.path(out_dir, "traditional_table1.html"))
  
  # save the table as png (PDF does not show color correctly)
  webshot2::webshot(
    url = file.path(out_dir, "traditional_table1.html"),
    file = file.path(out_dir, "traditional_table1.png"),
    zoom = 7,
    selector = "table"
  )



#-------------------------------------------------
# Helper function to create supplement exposure table (absolute and relative combined)
create_supp_exposure_table <- function(abs_table_data, hyb_table_data) {
  # rename columns in both tables to avoid conflicts when joining
  abs_table_mod <- abs_table_data %>%
    rename(
      Abs_Case = `Index days`
    )
  hyb_table_mod <- hyb_table_data %>%
    rename(
      Hyb_Case = `Index days`
    )
  # join the tables
  supp_combined_table <- abs_table_mod %>%
    full_join(hyb_table_mod, by = c("Cause", "Exposure")) %>%
    mutate(Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric"))) %>%
    mutate(Exposure = factor(Exposure, levels = c("Mild", "Moderate", "Severe", "None"))) %>%
    arrange(Cause, Exposure) %>%
    # Add "Exposure (PSPS)" rows under each cause
    group_by(Cause) %>%
    group_modify(~ {
      # Get the first row for this cause
      first_row <- .x[1, ]
      # Create the "Exposure (PSPS)" row with NA values (not empty strings)
      exposure_row <- first_row %>%
        mutate(Exposure = "Exposure (PSPS)",
               Abs_Case = NA_real_,
               Hyb_Case = NA_real_)
      # Combine the exposure row with the original data
      bind_rows(exposure_row, .x)
    }) %>%
    ungroup()
  
  # pretty table
  supp_pretty_exposure_table <- supp_combined_table %>%
  # Add indentation to the Exposure values (but not for "Exposure (PSPS)" rows)
  mutate(Exposure = ifelse(Exposure == "Exposure (PSPS)", 
                           "Exposure (PSPS)", 
                           paste0("\u00A0\u00A0\u00A0\u00A0", Exposure))) %>%  # Add 2 spaces for indentation
  gt() %>%
  fmt_number(
    columns = c("Abs_Case", "Hyb_Case"),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  # Format missing values as blank instead of "NA"
  fmt_missing(
    columns = c("Abs_Case", "Hyb_Case"),
    missing_text = ""
  ) %>%
  # make first col wider
  cols_width(
    Exposure ~ px(200)  # adjust the pixel value as needed
  ) %>%
  # Create the hierarchical header structure - only sub-spanners
  tab_spanner(
    label = "Absolute (primary)",
    columns = c(Abs_Case)
  ) %>%
  tab_spanner(
    label = "Relative (secondary)",
    columns = c(Hyb_Case)
  ) %>%
  # create row groups in REVERSE order
  tab_row_group(
    label = "Psychiatric",
    rows = Cause == "Psychiatric"
  ) %>%
  tab_row_group(
    label = "Cardiovascular",
    rows = Cause == "Cardiovascular"
  ) %>%
    tab_row_group(
    label = "COPD",
    rows = Cause == "COPD"
  ) %>%
  tab_row_group(
    label = "All-cause respiratory",
    rows = Cause == "Respiratory"
  ) %>%
  cols_label(
    Exposure = "Disease endpoint",
    Abs_Case = "Index days",
    Hyb_Case = "Index days"
  ) %>%
  # hide the original Cause column
  cols_hide(columns = Cause) %>%
  tab_options(
    row_group.font.weight = "bold",
    row_group.background.color = "#f7f7f7",
    # table border options
    table.border.top.color = "black",
    table.border.bottom.color = "black", 
    # Header border options
    heading.border.bottom.color = "black",
    # column labels border options
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    # body border options
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    # table body border top and bottom color
    table_body.border.top.color = "black",
    table_body.border.bottom.color = "black",
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    heading.border.bottom.width = px(1),
    column_labels.border.top.width = px(1),
    column_labels.border.bottom.width = px(1),
    row_group.border.top.width = px(1),
    row_group.border.bottom.width = px(1),
    table_body.border.top.width = px(1),
    table_body.border.bottom.width = px(1)
  ) %>% 
  # style options for row groups
  tab_options(
    row_group.font.weight = "bold",
    row_group.background.color = "#f7f7f7"
  ) %>%
  # style the header (column labels)
  tab_style(
    style = list(
      cell_fill(color = pal[1]),
      cell_text(weight = "bold")
    ),
    locations = list(
      cells_column_labels(),
      cells_column_spanners()
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = pal[1]),
      cell_text(weight = "bold")
    ),
    locations = cells_stubhead()
  ) %>%
    # Style the "Exposure (PSPS)" rows to match the indented rows
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = Exposure == "Exposure (PSPS)")
    )
  
  return(supp_pretty_exposure_table)
}

# Create supplement exposure tables for both lag types
supp_pretty_exposure_table_same_day <- create_supp_exposure_table(abs_table_same_day, hyb_table_same_day)
supp_pretty_exposure_table_lag4 <- create_supp_exposure_table(abs_table_lag4, hyb_table_lag4)

# Save same_day supplement exposure table
options(chromote.headless = "new")
supp_pretty_exposure_table_same_day %>% 
  as_raw_html() %>% 
  cat(file = file.path(out_dir, "supp_exposure_table_same_day.html"))
webshot2::webshot(
  url = file.path(out_dir, "supp_exposure_table_same_day.html"),
  file = file.path(out_dir, "supp_exposure_table_same_day.png"),
  zoom = 7,
  selector = "table",
  expand = c(1,10,1,1)
)

# Save lag4 supplement exposure table
supp_pretty_exposure_table_lag4 %>% 
  as_raw_html() %>% 
  cat(file = file.path(out_dir, "supp_exposure_table_lag4.html"))
webshot2::webshot(
  url = file.path(out_dir, "supp_exposure_table_lag4.html"),
  file = file.path(out_dir, "supp_exposure_table_lag4.png"),
  zoom = 7,
  selector = "table",
  expand = c(1,10,1,1)
)

