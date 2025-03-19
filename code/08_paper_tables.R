#-------------------------------------------------
# PSPS: Paper tables 
# March 2025
#-------------------------------------------------

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(ggforce, MetBrewer, dplyr, tidyr, knitr, gt, magick, pagedown)

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/results")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/tables_figures/")

exposure_summary_abs_by_ooi <- read.csv(paste0(results_dir, "/Exposure\ summaries/AbsPSPS_wf_expsummary_byOOI.csv"))
exposure_summary_hybrid_by_ooi <- read.csv(paste0(results_dir, "/Exposure\ summaries/HybPSPS_wf_expsummary_byOOI.csv"))
exp_abs_sm <- read.csv(paste0(results_dir, "/Exposure\ summaries/AbsPSPS_wf_expsummary.csv"))

# make table 1 -------------------------------------------
# table 1: summary of exposure by OOI, severity_customers, and case_indicator
table1 <- exposure_summary_abs_by_ooi %>% 
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
    dplyr::mutate(case_indicator = ifelse(case_indicator==1, "Case", "Control")) %>%
    pivot_wider(
        names_from = case_indicator,
        values_from = count) %>% 
    # rename causes 
    mutate(Cause = ifelse(
        Cause == "cardio", "Cardiovascular",
        ifelse(Cause == "resp", "Respiratory",
        ifelse(Cause == "psych", "Psychiatric",
        ifelse(Cause == "copd", "COPD")))), 
        # rename exposure ==0 to unexposed
        Exposure = ifelse(
            Exposure == 0, "Unexposed", Exposure
        )) %>% ungroup() %>% distinct()

# pretty table 1 
# Option 1: Reorder the data frame first
table1 <- table1 %>%
  mutate(Cause = factor(Cause, levels = c("Cardiovascular", "Psychiatric", "Respiratory", "COPD"))) %>%
  arrange(Cause, Exposure)

# Then create the table
# Option 1: Reorder the data frame first
table1 <- table1 %>%
  mutate(Cause = factor(Cause, levels = c("Cardiovascular", "Psychiatric", "Respiratory", "COPD"))) %>%
  arrange(Cause, Exposure)

# Then create the table
pretty_table1 <- table1 %>%
  gt() %>%
  tab_header(
    title = "Case-Control Counts by Cause and Exposure Level"
  ) %>%
  fmt_number(
    columns = c(Control, Case),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  # Create row groups in REVERSE order of how you want them displayed
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
  # Style options
  tab_options(
    row_group.font.weight = "bold",
    row_group.background.color = "#f7f7f7"
  )

  # this is something webshot needs to work...
  options(chromote.headless = "new")

  # save the table as html using cat 
   pretty_table1 %>% 
    as_raw_html() %>% 
    cat(file = paste0(out_dir, "table1.html"))
  
  # save the table as png
    # doing it this way becuase i couldnt get the dpi high enough with gtsave
  webshot2::webshot(
    url = paste0(out_dir, "table1.html"),
    file = paste0(out_dir, "table1.png"),
    zoom = 7,         # apparently this is approx 300 DPI
    selector = "table"  # only capture the table
  )

