#-------------------------------------------------
# PSPS: Paper tables 
# March 2025
#-------------------------------------------------

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(ggforce, MetBrewer, dplyr, tidyr, knitr, gt, magick, pagedown, readxl, gt, readr)

pal <- met.brewer(name = "Hokusai2", n=2)

# results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/Results\ -\ June\ 2025/")
# exposure_summary_abs_by_ooi <- read.csv(paste0(results_dir, "absexp_summary_byOOI.csv")) %>% 
#     mutate(severity_customers = ifelse(severity_customers == "none", "None", severity_customers))
# exposure_summary_hybrid_by_ooi <- read.csv(paste0(results_dir, "hybexp_summary_byOOI.csv")) %>% 
#     mutate(severity_hybrid = ifelse(severity_hybrid == "none", "None", severity_hybrid))
# table1s_df <- read_excel(paste0(results_dir, "PSPSTable1_demo_V3.xlsx"), sheet = "Sheet2")

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/oct_2025_results")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/tables_figures/")

exp_summary <- read_csv(paste0(results_dir, "psps_among_casedays.csv"))
ha_ed_table_df <- read_csv(paste0(results_dir, "/summary\ of\ events\ across\ data\ cleaning\ process_all_years.csv"))
table1s_df_updated <- read_csv(paste0(results_dir, "final\ dataset\ events\ by\ subgroup_all_years.csv")) %>% 
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
wf_among_casedays <- read_csv(paste0(results_dir, "wf_among_casedays.csv"))
        
        

## NOTE: table of icd codes is made in the appendix in latex. 

# process exposure summaries -------------------------------------------
abs_table <- exp_summary %>%
    select(c("severity_customers", "severity_customers_N", "outcome")) %>%
    group_by(severity_customers, outcome) %>%
    summarize(
        count = sum(severity_customers_N)
    ) %>%
    rename(
        Cause = outcome,
        Exposure = severity_customers
    ) %>%
    mutate(case_indicator = "Index days") %>%
    pivot_wider(
        names_from = case_indicator,
        values_from = count) %>% 
    # rename causes 
    mutate(Cause = ifelse(
        Cause == "adult_cardio", "Cardiovascular",
        ifelse(Cause == "adult_resp", "Respiratory",
        ifelse(Cause == "adult_psych", "Psychiatric",
        "COPD"))), 
        Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric")),
        Exposure = ifelse(Exposure == "none", "None", Exposure)) %>%
    ungroup() %>%
    distinct()  %>% 
    mutate(Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric"))) %>%
    mutate(Exposure = factor(Exposure, levels = c("Mild", "Moderate", "Severe", "None"))) %>%
    arrange(Cause, Exposure)
hyb_table <- exp_summary %>%
    select(c("severity_hybrid", "severity_hybrid_N", "outcome")) %>%
    group_by(severity_hybrid, outcome) %>%
    summarize(
        count = sum(severity_hybrid_N)
    ) %>%
    rename(
        Cause = outcome,
        Exposure = severity_hybrid
    ) %>%
    mutate(case_indicator = "Index days") %>%
    pivot_wider(
        names_from = case_indicator,
        values_from = count) %>% 
    # rename causes 
    mutate(Cause = ifelse(
        Cause == "adult_cardio", "Cardiovascular",
        ifelse(Cause == "adult_resp", "Respiratory",
        ifelse(Cause == "adult_psych", "Psychiatric",
        "COPD"))), 
        Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric")),
        Exposure = ifelse(Exposure == "none", "None", Exposure)) %>%
    ungroup() %>%
    distinct()  %>% 
    mutate(Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric"))) %>%
    mutate(Exposure = factor(Exposure, levels = c("Mild", "Moderate", "Severe", "None"))) %>%
    arrange(Cause, Exposure)

####################
### MAIN TABLES ####
####################
# exposure table just for absolute -------------------------------------------
# pretty table 2
pretty_exposure_table <- abs_table %>%
  # Add indentation to the Exposure values
  mutate(Exposure = paste0("\u00A0\u00A0\u00A0\u00A0", Exposure)) %>%  # Add 2 spaces for indentation
  gt() %>%
  fmt_number(
    columns = c(`Index days`),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  # make first col wider
  cols_width(
    Exposure ~ px(200)  # adjust the pixel value as needed
  ) %>%
  # add spanner headers
  tab_spanner(
    label = "Absolute (primary)",
    columns = c(`Index days`)
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
    label = "Respiratory",
    rows = Cause == "Respiratory"
  ) %>%
  # hide the original Cause column
  cols_hide(columns = Cause) %>%
  cols_label(
    Exposure = "Disease endpoint\nExposure (PSPS)", # can't do an enter and PSPS goes onto the line above so have to do psps after
    `Index days` = "Index days"
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
  )

  # this is something webshot needs to work...
  options(chromote.headless = "new")

  # save the table as html using cat 
   pretty_exposure_table %>% 
    as_raw_html() %>% 
    cat(file = paste0(out_dir, "exposure_table.html"))
  
  # save the table as png
    # doing it this way becuase i couldnt get the dpi high enough with gtsave
  webshot2::webshot(
    url = paste0(out_dir, "exposure_table.html"),
    file = paste0(out_dir, "exposure_table.png"),
    zoom = 7,         # apparently this is approx 300 DPI
    selector = "table"  # only capture the table
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
  # Set column labels
  cols_label(
    Respiratory = "Respiratory",
    COPD = "COPD",
    Cardiovascular = "Cardiovascular", 
    Psychiatric = "Psychiatric"
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
    cat(file = paste0(out_dir, "ha_ed_table.html"))
  
  # save the table as png
    # doing it this way becuase i couldnt get the dpi high enough with gtsave
  webshot2::webshot(
    url = paste0(out_dir, "ha_ed_table.html"),
    file = paste0(out_dir, "ha_ed_table.png"),
    zoom = 7,         # apparently this is approx 300 DPI
    selector = "table"  # only capture the table
  )

# make table of mean wf for case days by outcome -------------------------------------------
pretty_wf_by_outcome <- data.frame(
  row_name = "Mean (SD) WFS",
  Respiratory = paste0(round(wf_among_casedays$mean_lag0_lag5_mean[1], 2), " (", round(wf_among_casedays$mean_lag0_lag5_SD[1], 2), ")"),
  COPD = paste0(round(wf_among_casedays$mean_lag0_lag5_mean[2], 2), " (", round(wf_among_casedays$mean_lag0_lag5_SD[2], 2), ")"),
  Cardiovascular = paste0(round(wf_among_casedays$mean_lag0_lag5_mean[3], 2), " (", round(wf_among_casedays$mean_lag0_lag5_SD[3], 2), ")"),
  Psychiatric = paste0(round(wf_among_casedays$mean_lag0_lag5_mean[4], 2), " (", round(wf_among_casedays$mean_lag0_lag5_SD[4], 2), ")")
) %>%
  gt(rowname_col = "row_name") %>%
  # Set column labels
  cols_label(
    Respiratory = "Respiratory",
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

pretty_wf_by_outcome %>% 
  as_raw_html() %>% 
  cat(file = paste0(out_dir, "wf_by_outcome.html"))

# Save as PNG using gtsave instead
pretty_wf_by_outcome %>% 
  gtsave(filename = "wf_by_outcome.png", path = out_dir)

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
      Respiratory = "Respiratory",
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
      Respiratory ~ px(150),
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
    cat(file = paste0(out_dir, "traditional_table1.html"))
  
  # save the table as png
    # doing it this way becuase i couldnt get the dpi high enough with gtsave
  webshot2::webshot(
    url = paste0(out_dir, "traditional_table1.html"),
    file = paste0(out_dir, "traditional_table1.png"),
    zoom = 7,         # apparently this is approx 300 DPI
    selector = "table"  # only capture the table
  )



#-------------------------------------------------
# make table summarizing exposure by cause and exposure -------------------------------------------
# have abs table above! 

# rename columns in both tables to avoid conflicts when joining
abs_table_mod <- abs_table %>%
  rename(
    Abs_Case = `Index days`
  )
hyb_table_mod <- hyb_table %>%
  rename(
    Hyb_Case = `Index days`
  )
# join the tables
supp_combined_table <- abs_table_mod %>%
  full_join(hyb_table_mod, by = c("Cause", "Exposure")) %>%
  mutate(Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric"))) %>%
  mutate(Exposure = factor(Exposure, levels = c("Mild", "Moderate", "Severe", "None"))) %>%
  arrange(Cause, Exposure)

# pretty table 2
supp_pretty_exposure_table <- supp_combined_table %>%
  # Add indentation to the Exposure values
  mutate(Exposure = paste0("\u00A0\u00A0\u00A0\u00A0", Exposure)) %>%  # Add 2 spaces for indentation
  gt() %>%
  fmt_number(
    columns = c(Abs_Case, Hyb_Case),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  # make first col wider
  cols_width(
    Exposure ~ px(200)  # adjust the pixel value as needed
  ) %>%
  # Create the hierarchical header structure - add sub-spanners first, then top-level
  tab_spanner(
    label = "Absolute (primary)",
    columns = c(Abs_Case)
  ) %>%
  tab_spanner(
    label = "Relative (secondary)",
    columns = c(Hyb_Case)
  ) %>%
  tab_spanner(
    label = "Exposure (PSPS)",
    columns = c(Abs_Case, Hyb_Case)
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
    label = "Respiratory",
    rows = Cause == "Respiratory"
  ) %>%
  # hide the original Cause column
  cols_hide(columns = Cause) %>%
  cols_label(
    Exposure = "Disease endpoint",
    Abs_Case = "Index days",
    Hyb_Case = "Index days"
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
  )

  # this is something webshot needs to work...
  options(chromote.headless = "new")

  # save the table as html using cat 
   supp_pretty_exposure_table %>% 
    as_raw_html() %>% 
    cat(file = paste0(out_dir, "supp_exposure_table.html"))
  
  # save the table as png
    # doing it this way becuase i couldnt get the dpi high enough with gtsave
  webshot2::webshot(
    url = paste0(out_dir, "supp_exposure_table.html"),
    file = paste0(out_dir, "supp_exposure_table.png"),
    zoom = 7,         # apparently this is approx 300 DPI
    selector = "table"  # only capture the table
  )

