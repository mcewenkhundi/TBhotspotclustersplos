# Project: TBhotspotclusters
# Purpose: Responding to reviewer comments that dont fit in the old scripts
# Purpose: First round of reviewer response & second round of reviewer response at the bottom
# Authur: McEwen Khundi

library(tidyverse)
library(tidybayes)
library(sf)
load(here::here("data/scalebtcity_census_2015_2019_cluster.rda"))
dat_scale_ln <- readRDS(here::here("data/dat_scale_ln.rds"))
p_prev31_notif25_rintc <- readRDS(here::here("data/p_prev31_notif25_rintc.rds"))

# Responding to reviewer#1 query19
dat_scale_ln_2015 <- dat_scale_ln %>%
  ungroup() %>%
  filter(tb_year == 2015) %>%
  select(cluster_number, clinic_distance) %>%
  mutate(clinic_distance_km = clinic_distance / 1000)

dat_scale_ln_2015 %>%
  ggplot(aes(x = clinic_distance_km)) +
  geom_histogram(binwidth = 0.3) +
  theme_bw()

mean(dat_scale_ln_2015$clinic_distance_km)
median(dat_scale_ln_2015$clinic_distance_km)

# Responding to reviewer#1 query21
scalebtcity_census_2015_2019_cluster %>%
  ungroup() %>%
  filter(year == 2019) %>%
  pivot_wider(id_cols = cluster, names_from = age_sex, values_from = population) %>%
  mutate(perc_adults = (adult_female + adult_male) / (adult_female + adult_male + k5_14 + t0_4) * 100) %>%
  summarise(
    mean_perc_adults = mean(perc_adults),
    min_perc_adults = min(perc_adults),
    max_perc_adults = max(perc_adults),
    sd_perc_adults = sd(perc_adults)
  )

# Review3 query number 4, observed vs. fitted prevalence and notification

dat_cnr_pr_2019 <- select(
  dat_scale_ln, cluster_number, tb_year, total_confirmed,
  n_prev_tbcases, tent_cxr_total, population
) %>%
  filter(tb_year == 2019) %>%
  mutate(
    cnr_2019 = (total_confirmed / population) * 100000,
    pr_2019 = (n_prev_tbcases / tent_cxr_total) * 100000
  ) %>%
  rename(cluster = cluster_number)

# 95% CI
cluster_ests_95_cnr <- p_prev31_notif25_rintc %>%
  group_by(cluster) %>%
  mean_qi(p_notif, .width = 0.95) %>%
  rename_with(.fn = ~paste0("p_notif",.x), .cols = starts_with("."))

cluster_ests_95_cnr <- left_join(dat_cnr_pr_2019, cluster_ests_95_cnr,
                                 by="cluster")

cluster_ests_95_pr <- p_prev31_notif25_rintc %>%
  group_by(cluster) %>%
  mean_qi(p_prev, .width = 0.95) %>%
  rename_with(.fn = ~paste0("p_prev",.x), .cols = starts_with("."))

cluster_ests_95_pr <- left_join(dat_cnr_pr_2019,
                                cluster_ests_95_pr,
                                by="cluster")

#Graphs
cluster_ests_95_cnr %>%
  ggplot() +
  geom_pointrange(aes(x = cluster, y = p_notif, ymin = p_notif.lower, ymax = p_notif.upper)) +
  geom_point(aes(x = cluster,y = cnr_2019),color = "red" ) +
  scale_x_continuous(breaks = c(seq(1,72,3),72L)) +
  theme_bw() +
  labs(x= "Neighbourhood identifier", y = "Case notification rate per 100,000",
       title = "Observed in red versus Fitted mean (95% CrIs)")
ggsave(filename = here::here("figures/S6_Fig.tiff"), width = 10, height = 6, dpi = 310)


cluster_ests_95_pr %>%
  ggplot() +
  geom_pointrange(aes(x = cluster, y = p_prev, ymin = p_prev.lower, ymax = p_prev.upper)) +
  geom_point(aes(x = cluster,y = pr_2019),color = "red" ) +
  scale_x_continuous(breaks = c(seq(1,72,3),72L)) +
  theme_bw() +
  labs(x= "Neighbourhood identifier", y = "Prevalence rate per 100,000",
       title = "Observed in red versus Fitted mean (95% CrIs)")
ggsave(filename = here::here("figures/S7_Fig.tiff"), width = 10, height = 6, dpi = 310)

#Work added down as part of response to second round of reviewer queries
#This forms part of the output of S3 Equation in the suplimentary section.

#chisquare for cnr
cluster_ests_95_cnr <- cluster_ests_95_cnr %>%
  mutate(p_notif_prop_pred = p_notif/100000,
         total_confirmed_pred = population* p_notif_prop_pred) %>%
  relocate(total_confirmed, .after =total_confirmed_pred )

cluster_ests_95_cnr <- cluster_ests_95_cnr %>%
  mutate(chi_cnr = ((total_confirmed - total_confirmed_pred)^2)/total_confirmed_pred)

cluster_ests_95_cnr %>%
  summarise(chi_cnr_sum = sum(chi_cnr)) %>%
  View

#chisquare for pr
cluster_ests_95_pr <- cluster_ests_95_pr %>%
  mutate(p_prev_prop_pred = p_prev/100000,
         n_prev_tbcases_pred = tent_cxr_total* p_prev_prop_pred) %>%
  relocate(n_prev_tbcases, .after =n_prev_tbcases_pred )

cluster_ests_95_pr <- cluster_ests_95_pr %>%
  mutate(chi_pr = ((n_prev_tbcases - n_prev_tbcases_pred)^2)/n_prev_tbcases_pred)

cluster_ests_95_pr %>%
  summarise(chi_pr_sum = sum(chi_pr)) %>%
  View




