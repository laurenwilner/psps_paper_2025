#-------------------------------------------------
# PSPS: Paper figures
# March 2025
#-------------------------------------------------

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(ggforce, MetBrewer, dplyr, tidyr, knitr, gt, magick, pagedown)

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/results/Results\ -\ PSPS\ lag\ 0,\ wf\ smoke,\ ns(temp)/")
out_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_paper_2025/tables_figures/")

resp_lag0 <- read.csv(paste0(results_dir, "results_PSPS_wflag05_nstemp_resp.csv"))
cardio_lag0 <- read.csv(paste0(results_dir, "results_PSPS_wflag05_nstemp_cardio.csv"))
psych_lag0 <- read.csv(paste0(results_dir, "results_PSPS_wflag05_nstemp_psych.csv"))
copd_lag0 <- read.csv(paste0(results_dir, "results_PSPS_wflag05_nstemp_copd.csv"))

pal <- met.brewer(name = "Hokusai2", n=2)

# make figure 1 -------------------------------------------
# 4 panel plot with one panel per cause, log transform the y axis
# •	wf alone effect 
# •	effect for each psps severity (add the main effect + interaction)
# •	do that for each disease category
# •	main analysis: absolute
# •	supplement: same plots for hybrid
# •	make this figure similar to that hell-ish forest plot where i had the point ests and CIs listed and then the bars for the pt est and CI 
# •	DO NOT INCLUDE LAGS 1 AND 2. ONLY LAG 0. so each plot is wf main effect, psps main effect, and interaction term effect. then in results section sum interaction and main effect. 

# compile data
resp_lag0 <- resp_lag0 %>% 
  select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
  mutate(Cause = "Respiratory")%>% 
  # subset to first 11 rows bc those are for the absolute metric
  slice(1:11) 
cardio_lag0 <- cardio_lag0 %>%
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
    mutate(Cause = "Cardiovascular") %>% 
    # subset to first 11 rows bc those are for the absolute metric
    slice(1:11) 
psych_lag0 <- psych_lag0 %>%
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
    mutate(Cause = "Psychiatric") %>% 
    # subset to first 11 rows bc those are for the absolute metric
    slice(1:11) 
copd_lag0 <- copd_lag0 %>%
    select(c("Exposure", "OR", "CI_Lower", "CI_Upper")) %>%
    mutate(Cause = "COPD") %>% 
    # subset to first 11 rows bc those are for the absolute metric
    slice(1:11) 

# combine data
all_lag0 <- bind_rows(resp_lag0, cardio_lag0, psych_lag0, copd_lag0)

# subset to terms of interest and rename 
vars <- c("severity_customersSevere:mean_lag05_per10", "severity_customersSevere", "mean_lag05_per10")
plot_df <- all_lag0 %>% 
filter(Exposure %in% vars) %>%
mutate(Exposure = ifelse(
    Exposure == "severity_customersSevere:mean_lag05_per10", "Severe PSPS event * WF smoke", 
    ifelse(Exposure=="severity_customersSevere", "Severe PSPS event", "WF smoke")),
    log_odds = log(OR),
    # calc the se
        # upper bound is approximately: ln(OR) + 1.96×SE
        # lower bound is approximately: ln(OR) - 1.96×SE
    se = (log(CI_Upper) - log(CI_Lower)) / (2*1.96),
    ) %>% 
select(Exposure, log_odds, se, Cause)

# combine interaction term and psps main effect to have a single OR and SE, but keep wf separate
plot_df <- plot_df %>%
  # group by cause to process each condition separately
  group_by(Cause) %>%
  # create a nested dataframe to work within each cause
  summarize(
    data = list(tibble(
      Exposure = c(
        "Severe PSPS event * WF smoke",
        "WF smoke"
      ),
      log_odds = c(
        # sum the log odds of main effect and interaction for each cause
        sum(log_odds[Exposure == "Severe PSPS event" | 
                     Exposure == "Severe PSPS event * WF smoke"]),
        # leave wf alone, that i know is correct
        log_odds[Exposure == "WF smoke"]
      ),
      # calc combined se using variance addition (but do we need a covariance term?)
      se = c(
        sqrt(sum(se[Exposure == "Severe PSPS event"]^2 + 
                 se[Exposure == "Severe PSPS event * WF smoke"]^2)),
        # leave wf alone
        se[Exposure == "WF smoke"]
      )
    ))
  ) %>%
  unnest(data) %>%
  # i guess now we can just exponentiate? why am i here?
  mutate(
    odds_ratio = exp(log_odds),
    lower_ci = exp(log_odds - 1.96 * se),
    upper_ci = exp(log_odds + 1.96 * se)
  ) # i think im missing many elements, like covariance, but whats the pt of adding ors? 
  # to do: is step 3-4 right? can we just report the main effect? can we ditch this proj? 

  # i guess ill just plot for now 
  # need to make a box plot with the point estimate and CI bars
  fig1 <- ggplot(plot_df, aes(x = Exposure, y = odds_ratio, ymin = lower_ci, ymax = upper_ci)) +
    geom_point(aes(color = Exposure), position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = Exposure), width = 0.75, position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 1, linetype = "dashed") + 
    facet_wrap(~Cause, nrow = 1, scales = "free_y") +
    scale_color_manual(values = pal) +
    labs(
      x = "",
      y = "Odds Ratio",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      strip.text = element_text(size = 12),
      axis.text = element_text(size = 12),
    )

ggsave(paste0(out_dir, "fig1.png"), fig1, width = 10, height = 5, dpi = 100)
