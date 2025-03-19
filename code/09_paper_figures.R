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

# prep for figure 1 -------------------------------------------
# 4 panel plot with one panel per cause, log transform the y axis
# •	main effect alone
# •	effect for severe psps (add the main effect + interaction)
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
#  y = beta1*psps + beta2*wf + beta3*psps*wf + spline

# no wf + psps = beta1 
# psps + wf = beta3 + beta2
# step 1: log odds and add them
# step 2: calculate se

# plot beta 1 and then the sum thing

# first lets calcualte log odds, log se, log variance
vars <- c("severity_customersSevere:mean_lag05_per10", "severity_customersSevere", "mean_lag05_per10")
plot_df_temp <- all_lag0 %>% 
    filter(Exposure %in% vars) %>%
    mutate(Exposure = ifelse(
        Exposure == "severity_customersSevere:mean_lag05_per10", "Severe PSPS event * WF smoke", 
        ifelse(Exposure=="severity_customersSevere", "Severe PSPS event", "WF smoke")),
        log_odds = log(OR),
        # calc the variance
            # upper bound is approximately: ln(OR) + 1.96×SE
            # lower bound is approximately: ln(OR) - 1.96×SE
        se = (log(CI_Upper) - log(CI_Lower)) / (2*1.96),
        variance = se^2
        ) %>% 
    select(Exposure, log_odds, variance, Cause) 

# now lets calculate the combined term log odds, log se, log variance
    # step 1: sum the log odds
    # step 2: calculate the combined variance: var(x+y)=var(x) + var(y) + 2cov(x,y)
    # step 3: new se = sqrt(var)
    # step 4: calculate OR and CI (or = sum(log_odds1 + log_odds2), ci = β +/- 1.96*st err)
    
# for now, lets set covariance to .0065 so we can proceed:
cov <- .00000065

# combine interaction term and wf main effect to have a single OR and SE, but keep main effect separate
plot_df <- plot_df_temp %>% 
    mutate(group = ifelse(Exposure == "Severe PSPS event * WF smoke" | Exposure == "WF smoke", "interaction", "main")) %>%
    group_by(Cause, group) %>%
    reframe(
        log_odds = sum(log_odds),
        variance = ifelse(group == "interaction", sum(variance) + 2*cov, sum(variance)),
        se = sqrt(variance)
    ) %>%
    distinct() %>% 
    mutate(
        odds_ratio = exp(log_odds),
        lower_ci = exp(log_odds - 1.96 * se),
        upper_ci = exp(log_odds + 1.96 * se)
    ) %>% 
    mutate(group = ifelse(group == "interaction", "Interaction (Severe PSPS event x wildfire smoke)", "Severe PSPS event"))

# make figure 1 -------------------------------------------
  # box plot 
  fig1 <- ggplot(plot_df, aes(x = group, y = odds_ratio, ymin = lower_ci, ymax = upper_ci)) +
    geom_point(aes(color = group), position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = group), width = 0.75, position = position_dodge(width = 0.5)) +
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


# save figs -------------------------------------------
ggsave(paste0(out_dir, "fig1.png"), fig1, width = 10, height = 5, dpi = 100)
