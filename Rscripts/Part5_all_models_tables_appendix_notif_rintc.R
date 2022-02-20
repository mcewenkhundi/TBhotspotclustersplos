# Project: TBhotspots
# Task: creating the appendix table for all notif random intercept models
# author: McEwen Khundi


# load libraries
library(parameters)
library(tidyverse)
library(brms)
library(tidybayes)
library(gt)

## Function for tuning models to simple tible
model_parameters_notif_rintc <- function(model_notif_rintc, model_name) {
  # model_notif_rintc <- model_parameters(model_notif_rintc, centrality = "mean", effects = "all", component = "all", ci = 0.95, exponentiate = TRUE)
  model_notif_rintc <- posterior_summary(model_notif_rintc)

  model_notif_rintc.parameters <- row.names(model_notif_rintc)
  model_notif_rintc <- as_tibble(model_notif_rintc)

  model_notif_rintc <- model_notif_rintc %>%
    mutate(Parameter = model_notif_rintc.parameters, .before = 1) %>%
    select(Parameter, Mean = Estimate, CI_low = Q2.5, CI_high = Q97.5)

  model_notif_rintc1 <- model_notif_rintc %>%
    filter(!str_detect(Parameter, "sd_cluster__Intercept|sdcar|zi")) %>%
    filter(!str_detect(Parameter, "rrintc|lp__|r_cluster")) %>%
    mutate(across(c(Mean, CI_low, CI_high), ~ exp(.x)))

  model_notif_rintc2 <- model_notif_rintc %>%
    filter(str_detect(Parameter, "sd_cluster__Intercept|sdcar|zi"))

  model_notif_rintc <- bind_rows(model_notif_rintc1, model_notif_rintc2)


  model_notif_rintc$Mean[1] <- model_notif_rintc$Mean[1] * 100000
  model_notif_rintc$CI_low[1] <- model_notif_rintc$CI_low[1] * 100000
  model_notif_rintc$CI_high[1] <- model_notif_rintc$CI_high[1] * 100000

  # model_name <-  deparse(substitute(model_notif_rintc)) #https://stackoverflow.com/questions/10520772/in-r-how-to-get-an-objects-name-after-it-is-sent-to-a-function
  # model_name <- paste0(model_name, "m")

  model_notif_rintc %>%
    select(Parameter, Mean, CI_low, CI_high) %>%
    mutate(across(where(is.numeric), ~ formatC(round(.x, 2), 2, format = "f"))) %>%
    mutate(Parameter = str_replace(Parameter, "b_", "")) %>%
    mutate({{ model_name }} := paste0(Mean, " (", CI_low, "-", CI_high, ")")) %>%
    # https://stackoverflow.com/questions/26003574/use-dynamic-variable-names-in-dplyr
    select(Parameter, {{ model_name }})
}

notif_model_list_df_t_df <- function(model_list) {
  notif_model_list_df <- reduce(.x = model_list, .f = left_join, by = "Parameter")

  notif_model_list_df_t <- t(notif_model_list_df)

  colnames(notif_model_list_df_t) <- notif_model_list_df_t[1, ]

  Model_names <- row.names(notif_model_list_df_t)

  notif_model_list_df_t_df <- as_tibble(notif_model_list_df_t)

  notif_model_list_df_t_df$model_names <- Model_names

  notif_model_list_df_t_df <- notif_model_list_df_t_df %>%
    select(model_names, everything())

  notif_model_list_df_t_df <- notif_model_list_df_t_df[-1, ]

  notif_model_list_df_t_df
}

# notif_model_rintc_1_df <- model_parameters_notif_rintc(notif_model_rintc_1, "notif_model_rintc_1")
# notif_model_rintc_2_df <- model_parameters_notif_rintc(notif_model_rintc_2, "notif_model_rintc_2")
# notif_model_rintc_3_df <- model_parameters_notif_rintc(notif_model_rintc_3, "notif_model_rintc_3")

##### This is for notif rintc models
files_to_read <- list.files(
  path = here::here("data"),
  pattern = "^notif_model_rintc_[0-9]{1,2}",
  full.names = TRUE
)

model_names <- str_extract(files_to_read, pattern = "notif_model_rintc_[0-9]{1,2}")
model_names

notif_model_list <- map(
  files_to_read,
  function(x) {
    readRDS(x)
  }
)

notif_model_list <- notif_model_list[order(readr::parse_number(model_names))]
model_names <- model_names[order(readr::parse_number(model_names))]


model_names_new <- paste0("notification model ", 1:32)
notif_model_list_df <- map2(.x = notif_model_list, .y = model_names_new, .f = model_parameters_notif_rintc)
rm(notif_model_list)

notif_model_list_df_t_df(notif_model_list_df) %>%
  rename(
    "Model names" = model_names, "Percentage of adults (=15y)" = scale_prop_adults_mean,
    "Percentage of household heads that did not complete primary school" = scale_perc_never_primary_mean,
    "Distance to nearest TB clinic (km)" = scale_clinic_distance_1km,
    "Percentage of HIV prevalence" = scale_perc_hiv_mean,
    "Percentage of male adults" = scale_perc_male_mean,
    "Year: 2015" = tb_year2015,
    "Year: 2016" = tb_year2016,
    "Year: 2017" = tb_year2017,
    "Year: 2018" = tb_year2018,
    "Random effects SD: cluster" = sd_cluster__Intercept
  ) %>%
  gt::gt() %>%
  fmt_missing(
    everything(),
    missing_text = ""
  ) %>%
  gtsave(filename = "S2_Table_notif_models_rintc.rtf", path = here::here("figures"))
