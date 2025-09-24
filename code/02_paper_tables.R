#-------------------------------------------------
# PSPS: Paper tables 
# March 2025
#-------------------------------------------------

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

## NOTE: table of icd codes is made in the appendix in latex. 

####################
### MAIN TABLES ####
####################
# exposure table just for absolute -------------------------------------------
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
    dplyr::mutate(case_indicator = ifelse(case_indicator==1, "Index days", "Referent days")) %>%
    pivot_wider(
        names_from = case_indicator,
        values_from = count) %>% 
    # rename causes 
    mutate(Cause = ifelse(
        Cause == "cardio", "Cardiovascular",
        ifelse(Cause == "resp", "Respiratory",
        ifelse(Cause == "psych", "Psychiatric",
        "COPD"))), 
        Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric")), 
        # rename exposure ==0 to unexposed
        Exposure = ifelse(
            Exposure == 0, "Unexposed", Exposure
        )) %>% ungroup() %>% distinct()

# rename columns in both tables to avoid conflicts when joining
exposure_table <- abs_table %>%
  rename(
    Abs_Case = `Index days`,
    Abs_Control = `Referent days`
  ) %>%
  mutate(Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric"))) %>%
  mutate(Exposure = factor(Exposure, levels = c("Mild", "Moderate", "Severe", "None"))) %>%
  arrange(Cause, Exposure)

# pretty table 2
pretty_exposure_table <- exposure_table %>%
  # Add indentation to the Exposure values
  mutate(Exposure = paste0("\u00A0\u00A0\u00A0\u00A0", Exposure)) %>%  # Add 2 spaces for indentation
  gt() %>%
  fmt_number(
    columns = c(Abs_Case, Abs_Control),
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
    columns = c(Abs_Case, Abs_Control)
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
    Abs_Case = "Index days",
    Abs_Control = "Referent days",
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



###########################
### SUPPLEMENTAL TABLES ###
###########################

#-------------------------------------------------
# make traditional table 1 for supplement -------------------------------------------
create_traditional_table1 <- function(data) {
  categories <- unique(data$category)
  
  # create an empty data frame with the structure we need
  combined_data <- data.frame(
    Group = character(0),
    Respiratory = character(0),
    COPD = character(0),
    Cardiovascular = character(0),
    Psychiatric = character(0),
    stringsAsFactors = FALSE,
    row_type = character(0)  # add a row type identifier column
  )
  
  # process each category
  for (cat in categories) {
    # skip processing if this is the Total category - we'll handle it separately
    if (cat == "Total") {
      next
    }
    
    cat_data <- data %>% filter(category == cat)
    
    # add a header row for the category
    header_row <- data.frame(
      Group = cat,
      Respiratory = "", 
      COPD = "",
      Cardiovascular = "",  # blank instead of NA
      Psychiatric = "",
      stringsAsFactors = FALSE,
      row_type = "header"  # mark as header row
    )
    
    # create rows for the subgroups
    subgroup_rows <- data.frame(
      Group = cat_data$group,  # group names directly in the Group column
      Respiratory = cat_data$Respiratory,
      COPD = cat_data$COPD,
      Cardiovascular = cat_data$Cardiovascular,
      Psychiatric = cat_data$Psychiatric,
      stringsAsFactors = FALSE,
      row_type = "subgroup" 
    )
    
    # combine header row with subgroup rows
    section <- rbind(header_row, subgroup_rows)
    
    # add this section to our combined data
    combined_data <- rbind(combined_data, section)
  }
  
  # add the Total row
  total_data <- data %>% filter(category == "Total")
  if (nrow(total_data) > 0) {
    total_row <- data.frame(
      Group = "Total",
      Respiratory = total_data$Respiratory[1],
      COPD = total_data$COPD[1],
      Cardiovascular = total_data$Cardiovascular[1],
      Psychiatric = total_data$Psychiatric[1],
      stringsAsFactors = FALSE,
      row_type = "total"  # label as total row
    )
    combined_data <- rbind(combined_data, total_row)
  }
  
  # create gt table - remove the row_type column before creating the table
  visible_data <- combined_data %>% select(-row_type)
  gt_table <- gt(visible_data)
  
  # apply styles based on the row_type in the original data
  header_rows <- which(combined_data$row_type == "header")
  subgroup_rows <- which(combined_data$row_type == "subgroup")
  total_rows <- which(combined_data$row_type == "total")
  
  gt_table <- gt_table %>%
    cols_label(
      Group = "",
      Respiratory = "Respiratory", 
      COPD = "COPD",
      Cardiovascular = "Cardiovascular",
      Psychiatric = "Psychiatric"
    ) %>%
    # category headers bold
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = "Group", rows = header_rows)
    ) %>%
    # Total row bold
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = "Group", rows = total_rows)
    ) %>%
    # indentation to subgroup values
    tab_style(
      style = cell_text(indent = px(15)),
      locations = cells_body(columns = "Group", rows = subgroup_rows)
    ) %>%
    # header row style
    tab_style(
      style = cell_fill(color = pal[1]),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = cells_column_labels(columns = everything())
    ) %>% 
    # add a border under the column headers
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
    # add alternating row colors
    opt_row_striping() %>%
    # right-align the numeric columns
    cols_align(align = "right", columns = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric")) %>%
    # left-align the category and subgroup columns
    cols_align(align = "left", columns = "Group")
  
  return(gt_table)
}

# create and display the table
pretty_traditional_table1 <- create_traditional_table1(table1s_df)

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
    dplyr::mutate(case_indicator = ifelse(case_indicator==1, "Index days", "Referent days")) %>%
    pivot_wider(
        names_from = case_indicator,
        values_from = count) %>% 
    # rename causes 
    mutate(Cause = ifelse(
        Cause == "cardio", "Cardiovascular",
        ifelse(Cause == "resp", "Respiratory",
        ifelse(Cause == "psych", "Psychiatric",
        "COPD"))), 
        Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric")), 
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
    dplyr::mutate(case_indicator = ifelse(case_indicator==1, "Index days", "Referent days")) %>%
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
    Abs_Case = `Index days`,
    Abs_Control = `Referent days`
  )
hyb_table <- hyb_table %>%
  rename(
    Hyb_Case = `Index days`,
    Hyb_Control = `Referent days`
  )
# join the tables
supp_combined_table <- abs_table %>%
  full_join(hyb_table, by = c("Cause", "Exposure")) %>%
  mutate(Cause = factor(Cause, levels = c("Respiratory", "COPD", "Cardiovascular", "Psychiatric"))) %>%
  mutate(Exposure = factor(Exposure, levels = c("Mild", "Moderate", "Severe", "None"))) %>%
  arrange(Cause, Exposure)

# pretty table 2
supp_pretty_exposure_table <- supp_combined_table %>%
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
    Exposure ~ px(200)  # adjust the pixel value as needed
  ) %>%
  # add spanner headers
  tab_spanner(
    label = "Absolute (primary)",
    columns = c(Abs_Case, Abs_Control)
  ) %>%
  tab_spanner(
    label = "Relative (secondary)",
    columns = c(Hyb_Case, Hyb_Control)
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
    Abs_Case = "Index days",
    Abs_Control = "Referent days",
    Hyb_Case = "Index days",
    Hyb_Control = "Referent days"
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

