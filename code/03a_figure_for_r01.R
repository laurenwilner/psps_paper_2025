#-------------------------------------------------
# PSPS: One-off figure for R01 - simplified results
# Same-day only, all-cause respiratory only, PSPS + WF PM2.5 (no interaction)
# Uses OKeeffe1 palette
#-------------------------------------------------

args0 <- commandArgs(trailingOnly = FALSE)
file0 <- grep("^--file=", args0, value = TRUE)
if (length(file0) > 0) {
  source(file.path(dirname(normalizePath(sub("^--file=", "", file0))), "paths.R"))
} else {
  source(file.path(path.expand("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025"), "code", "paths.R"))
}

pacman::p_load(MetBrewer, dplyr, ggplot2)
source(file.path(code_dir, "00_helper_functions.R"))

load_and_prepare_data <- function(metric_type = "abs", age_group = "age 20 and older", severity_level, cov_matrices) {
  if (metric_type == "abs") {
    all_data <- read.csv(file.path(results_dir, "all_results_abs_jan2026.csv"))
  } else {
    all_data <- read.csv(file.path(results_dir, "all_results_hyb_jan2026.csv"))
  }
  all_data <- all_data %>% filter(age_group == !!age_group)
  same_day_data <- all_data %>% filter(lag_type == "same_day") %>% select(Exposure, OR, CI_Lower, CI_Upper, Cause)
  same_day_processed <- if (nrow(same_day_data) > 0) {
    process_results(severity_level, same_day_data, metric = metric_type, cov_matrices, lag_type = "same_day", age_group = age_group)
  } else NULL
  lag4_processed <- if (nrow(all_data %>% filter(lag_type == "lag4")) > 0) {
    lag4_data <- all_data %>% filter(lag_type == "lag4") %>% select(Exposure, OR, CI_Lower, CI_Upper, Cause)
    process_results(severity_level, lag4_data, metric = metric_type, cov_matrices, lag_type = "lag4", age_group = age_group)
  } else NULL
  return(list(same_day = same_day_processed, lag4 = lag4_processed))
}

# Load data
cov_matrices <- readRDS(file.path(results_dir, "cov_matrices_jan2026.rds"))
data_list <- load_and_prepare_data(
  metric_type = "abs",
  age_group = "age 20 and older",
  severity_level = "Severe",
  cov_matrices = cov_matrices
)

# Same-day only, filter to PSPS, WF PM2.5, and Joint effect (no multiplicative interaction), respiratory only
wf_pm_label <- "WF PM\u2082.\u2085"
plot_data <- data_list$same_day %>%
  filter(
    Cause == "Respiratory",
    !grepl("interaction only", Exposure)
  ) %>%
  mutate(
    Exposure = case_when(
      Exposure == "WF smoke" ~ wf_pm_label,
      grepl("combined", Exposure) ~ "Joint effect",
      TRUE ~ "PSPS"
    ),
    Exposure = factor(Exposure, levels = c("PSPS", wf_pm_label, "Joint effect")),
    Cause = "All-cause respiratory"
  )

# OKeeffe1 palette - PSPS (second lightest red), WF PM (darkest red), Joint effect (second darkest blue)
pal_ok <- MetBrewer::met.brewer("OKeeffe1")
exposure_colors <- setNames(
  c(pal_ok[4], pal_ok[1], pal_ok[10]),
  c("PSPS", wf_pm_label, "Joint effect")
)
wf_expr_label <- expression("WF PM"["2.5"])
exposure_labels <- setNames(
  c("PSPS", wf_expr_label, "Joint effect"),
  c("PSPS", wf_pm_label, "Joint effect")
)

results_fig_r01 <- ggplot(plot_data, aes(x = Exposure, y = odds_ratio, ymin = lower_ci, ymax = upper_ci)) +
  geom_point(aes(color = Exposure), size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = Exposure), width = 0.3, linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = exposure_colors, name = NULL, labels = exposure_labels) +
  scale_x_discrete(labels = exposure_labels) +
  labs(x = "", y = "Odds Ratio") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    legend.position = "none",
    panel.grid.major.y = element_line(linewidth = 0.3, color = "gray85"),
    panel.grid.major.x = element_line(linewidth = 0.3, color = "gray85")
  ) +
  scale_y_log10(
    breaks = c(0.95, 1.0, 1.05, 1.1, 1.15, 1.2, 1.25),
    limits = c(0.95, 1.25)
  )

ggsave(file.path(out_dir, "results_fig_r01.pdf"), results_fig_r01, width = 4.5, height = 6, dpi = 100, device = cairo_pdf)
cat("Saved", file.path(out_dir, "results_fig_r01.pdf"), "\n")
