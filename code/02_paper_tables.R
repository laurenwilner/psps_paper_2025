#-------------------------------------------------
# PSPS: Paper tables 
# March 2025
#-------------------------------------------------
# to do: put n under case days and control days. 
# ask caitlin: the tables are currently uniqu visits not unique ppl, so if someone had multiple visits then they are in there multiple times. lets have it be unique and then also have a row of avg number of visits per person. 
# combine other/unknown
# make table 1 supplement 

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(ggforce, MetBrewer, dplyr, tidyr, knitr, gt, magick, pagedown, readxl, gt)

pal <- met.brewer(name = "Hokusai2", n=2)

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/Results\ -\ June\ 2025/")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/tables_figures/")

exposure_summary_abs_by_ooi <- read.csv(paste0(results_dir, "absexp_summary_byOOI.csv")) %>% 
    mutate(severity_customers = ifelse(severity_customers == "none", "None", severity_customers))
exposure_summary_hybrid_by_ooi <- read.csv(paste0(results_dir, "hybexp_summary_byOOI.csv")) %>% 
    mutate(severity_hybrid = ifelse(severity_hybrid == "none", "None", severity_hybrid))

table1s_df <- read_excel(paste0(results_dir, "PSPSTable1_demo_V3.xlsx"), sheet = "Sheet2")

## NOTE: table 1 is made in the methods in latex. 

# make table 2 -------------------------------------------
# table 2: summary of exposure by OOI, severity_customers, and case_indicator
abs_table <- exposure_summary_abs_by_ooi %>% 
    select(c("OOI", "severity_customers", "case_indicator", "count")) %>%
    group_by(OOI, severity_customers, case_indicator) %>%
    summarize(
        count = sum(count)
    ) %>% 
    # label ooi as "Cause" and severity_customers as "Exposure"
    rename(
        Cause = OOI,
        Exposure = severity_customers
    ) %>%
    # make 2 vars for "Case" and "Control"
    dplyr::mutate(case_indicator = ifelse(case_indicator==1, "Case-days", "Control-days")) %>%
    pivot_wider(
        names_from = case_indicator,
        values_from = count) %>% 
    # rename causes 
    mutate(Cause = ifelse(
        Cause == "cardio", "Cardiovascular",
        ifelse(Cause == "resp", "Respiratory",
        ifelse(Cause == "psych", "Psychiatric",
        "COPD"))), 
        # rename exposure ==0 to unexposed
        Exposure = ifelse(
            Exposure == 0, "Unexposed", Exposure
        )) %>% ungroup() %>% distinct()
hyb_table <- exposure_summary_hybrid_by_ooi %>% 
    select(c("OOI", "severity_hybrid", "case_indicator", "count")) %>%
    group_by(OOI, severity_hybrid, case_indicator) %>%
    summarize(
        count = sum(count)
    ) %>% 
    # label ooi as "Cause" and severity_customers as "Exposure"
    rename(
        Cause = OOI,
        Exposure = severity_hybrid
    ) %>%
    # make 2 vars for "Case" and "Control"
    dplyr::mutate(case_indicator = ifelse(case_indicator==1, "Case-days", "Control-days")) %>%
    pivot_wider(
        names_from = case_indicator,
        values_from = count) %>% 
    # rename causes 
    mutate(Cause = ifelse(
        Cause == "cardio", "Cardiovascular",
        ifelse(Cause == "resp", "Respiratory",
        ifelse(Cause == "psych", "Psychiatric",
        "COPD"))), 
        # rename exposure ==0 to unexposed
        Exposure = ifelse(
            Exposure == 0, "Unexposed", Exposure
        )) %>% ungroup() %>% distinct()

# rename columns in both tables to avoid conflicts when joining
abs_table <- abs_table %>%
  rename(
    Abs_Case = `Case-days`,
    Abs_Control = `Control-days`
  )
hyb_table <- hyb_table %>%
  rename(
    Hyb_Case = `Case-days`,
    Hyb_Control = `Control-days`
  )
# join the tables
combined_table <- abs_table %>%
  full_join(hyb_table, by = c("Cause", "Exposure")) %>%
  mutate(Cause = factor(Cause, levels = c("Cardiovascular", "Psychiatric", "Respiratory", "COPD"))) %>%
  mutate(Exposure = factor(Exposure, levels = c("Mild", "Moderate", "Severe", "None"))) %>%
  arrange(Cause, Exposure)

# pretty table 2
pretty_table2 <- combined_table %>%
  # Add indentation to the Exposure values
  mutate(Exposure = paste0("\u00A0\u00A0\u00A0\u00A0", Exposure)) %>%  # Add 2 spaces for indentation
  gt() %>%
  fmt_number(
    columns = c(Abs_Case, Abs_Control, Hyb_Case, Hyb_Control),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  # make first col wider
  cols_width(
    Exposure ~ px(200)  # Adjust the pixel value as needed
  ) %>%
  # Add spanner headers
  tab_spanner(
    label = "Absolute",
    columns = c(Abs_Case, Abs_Control)
  ) %>%
  tab_spanner(
    label = "Relative",
    columns = c(Hyb_Case, Hyb_Control)
  ) %>%
  # Create row groups in REVERSE order
  tab_row_group(
    label = "COPD",
    rows = Cause == "COPD"
  ) %>%
  tab_row_group(
    label = "Respiratory",
    rows = Cause == "Respiratory"
  ) %>%
  tab_row_group(
    label = "Psychiatric",
    rows = Cause == "Psychiatric"
  ) %>%
  tab_row_group(
    label = "Cardiovascular",
    rows = Cause == "Cardiovascular"
  ) %>%
  # Hide the original Cause column
  cols_hide(columns = Cause) %>%
  # Rename columns - change Exposure to have indentation and add Disease end point
  cols_label(
    Exposure = "Disease end point \n    Exposure",  # This will be the main header
    Abs_Case = "Case-days",
    Abs_Control = "Control-days",
    Hyb_Case = "Case-days",
    Hyb_Control = "Control-days"
  ) %>%
  # Add a stub header for the exposure column
  tab_stubhead(label = html("Disease end point<br><span style='font-weight: normal;'>    Exposure</span>")) %>%
  tab_options(
    row_group.font.weight = "bold",
    row_group.background.color = "#f7f7f7",
    # Table border options
    table.border.top.color = "black",
    table.border.bottom.color = "black", 
    # Header border options
    heading.border.bottom.color = "black",
    # Column labels border options
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    # Body border options
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    # Table body border top and bottom color
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
  # Style options for row groups
  tab_options(
    row_group.font.weight = "bold",
    row_group.background.color = "#f7f7f7"
  ) %>%
  # Style the header (column labels)
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
  # Style the stubhead (Disease end point header)
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
   pretty_table2 %>% 
    as_raw_html() %>% 
    cat(file = paste0(out_dir, "table2.html"))
  
  # save the table as png
    # doing it this way becuase i couldnt get the dpi high enough with gtsave
  webshot2::webshot(
    url = paste0(out_dir, "table2.html"),
    file = paste0(out_dir, "table2.png"),
    zoom = 7,         # apparently this is approx 300 DPI
    selector = "table"  # only capture the table
  )

# make supp table 1 -------------------------------------------
create_table1s <- function(data) {
  categories <- unique(data$category)
  
  # Create an empty data frame with the structure we need
  combined_data <- data.frame(
    Group = character(0),
    Cardiovascular = character(0),
    Psychiatric = character(0),
    Respiratory = character(0),
    COPD = character(0),
    stringsAsFactors = FALSE,
    row_type = character(0)  # Add a row type identifier column
  )
  
  # Process each category
  for (cat in categories) {
    # Skip processing if this is the Total category - we'll handle it separately
    if (cat == "Total") {
      next
    }
    
    cat_data <- data %>% filter(category == cat)
    
    # Add a header row for the category
    header_row <- data.frame(
      Group = cat,
      Cardiovascular = "",  # Blank instead of NA
      Psychiatric = "",
      Respiratory = "", 
      COPD = "",
      stringsAsFactors = FALSE,
      row_type = "header"  # Mark as header row
    )
    
    # Create rows for the subgroups
    subgroup_rows <- data.frame(
      Group = cat_data$group,  # Group names directly in the Group column
      Cardiovascular = cat_data$Cardiovascular,
      Psychiatric = cat_data$Psychiatric,
      Respiratory = cat_data$Respiratory,
      COPD = cat_data$COPD,
      stringsAsFactors = FALSE,
      row_type = "subgroup"  # Mark as subgroup row
    )
    
    # Combine header row with subgroup rows
    section <- rbind(header_row, subgroup_rows)
    
    # Add this section to our combined data
    combined_data <- rbind(combined_data, section)
  }
  
  # Add the Total row
  total_data <- data %>% filter(category == "Total")
  if (nrow(total_data) > 0) {
    total_row <- data.frame(
      Group = "Total",
      Cardiovascular = total_data$Cardiovascular[1],
      Psychiatric = total_data$Psychiatric[1],
      Respiratory = total_data$Respiratory[1],
      COPD = total_data$COPD[1],
      stringsAsFactors = FALSE,
      row_type = "total"  # Mark as total row
    )
    combined_data <- rbind(combined_data, total_row)
  }
  
  # Create gt table - remove the row_type column before creating the table
  visible_data <- combined_data %>% select(-row_type)
  gt_table <- gt(visible_data)
  
  # Now apply styles based on the row_type in the original data
  header_rows <- which(combined_data$row_type == "header")
  subgroup_rows <- which(combined_data$row_type == "subgroup")
  total_rows <- which(combined_data$row_type == "total")
  
  gt_table <- gt_table %>%
    cols_label(
      Group = "",
      Cardiovascular = "Cardiovascular",
      Psychiatric = "Psychiatric",
      Respiratory = "Respiratory", 
      COPD = "COPD"
    ) %>%
    # Make category headers bold
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = "Group", rows = header_rows)
    ) %>%
    # Make Total row bold
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = "Group", rows = total_rows)
    ) %>%
    # Add indentation to subgroup values
    tab_style(
      style = cell_text(indent = px(15)),
      locations = cells_body(columns = "Group", rows = subgroup_rows)
    ) %>%
    # Style the header row
    tab_style(
      style = cell_fill(color = pal[1]),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = cells_column_labels(columns = everything())
    ) %>% 
    # Add a border under the column headers
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
      column_labels.padding = px(15)  # Increase this value to make the header taller
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = total_rows)  # Remove the columns = "Group" to apply to all columns
    ) %>%
    tab_style(
      style = list(
        cell_text(align = "center")
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
      row_group.border.top.style = "none",
      row_group.border.bottom.style = "none",
      table_body.border.top.style = "none",
      table_body.border.bottom.style = "none",
      table_body.hlines.style = "none",
      table_body.vlines.style = "none"
    ) %>%
    # Add alternating row colors
    opt_row_striping() %>%
    # Right-align the numeric columns
    cols_align(align = "right", columns = c("Cardiovascular", "Psychiatric", "Respiratory", "COPD")) %>%
    # Left-align the category and subgroup columns
    cols_align(align = "left", columns = "Group")
  
  return(gt_table)
}

# Create and display the table
pretty_table1s <- create_table1s(table1s_df)

# save table 
  # this is something webshot needs to work...
  options(chromote.headless = "new")

  # save the table as html using cat 
   pretty_table1s %>% 
    as_raw_html() %>% 
    cat(file = paste0(out_dir, "supp_table1.html"))
  
  # save the table as png
    # doing it this way becuase i couldnt get the dpi high enough with gtsave
  webshot2::webshot(
    url = paste0(out_dir, "supp_table1.html"),
    file = paste0(out_dir, "supp_table1.png"),
    zoom = 7,         # apparently this is approx 300 DPI
    selector = "table"  # only capture the table
  )



