# Project: TBhotspots
# Task: creating the appendix table for all prevalence car models
# author: McEwen Khundi

library(parameters)
library(tidyverse)
library(brms)
library(tidybayes)
library(gt)

## Function for tuning models to simple tible
model_parameters_prev_car <- function(model_prev_car, model_name) {
  # model_prev_car <- model_parameters(model_prev_car, centrality = "mean", effects = "all", component = "all", ci = 0.95, exponentiate = TRUE)
  model_prev_car <- posterior_summary(model_prev_car)

  model_prev_car.parameters <- row.names(model_prev_car)
  model_prev_car <- as_tibble(model_prev_car)

  model_prev_car <- model_prev_car %>%
    mutate(Parameter = model_prev_car.parameters, .before = 1) %>%
    select(Parameter, Mean = Estimate, CI_low = Q2.5, CI_high = Q97.5)

  model_prev_car1 <- model_prev_car %>%
    filter(!str_detect(Parameter, "sd_cluster__Intercept|sdcar|zi")) %>%
    filter(!str_detect(Parameter, "rcar|lp__|r_cluster")) %>%
    mutate(across(c(Mean, CI_low, CI_high), ~ exp(.x)))

  model_prev_car2 <- model_prev_car %>%
    filter(str_detect(Parameter, "sd_cluster__Intercept|sdcar|zi"))

  model_prev_car <- bind_rows(model_prev_car1, model_prev_car2)


  model_prev_car$Mean[1] <- model_prev_car$Mean[1] * 100000
  model_prev_car$CI_low[1] <- model_prev_car$CI_low[1] * 100000
  model_prev_car$CI_high[1] <- model_prev_car$CI_high[1] * 100000

  # model_name <-  deparse(substitute(model_prev_car)) #https://stackoverflow.com/questions/10520772/in-r-how-to-get-an-objects-name-after-it-is-sent-to-a-function
  # model_name <- paste0(model_name, "m")

  model_prev_car %>%
    select(Parameter, Mean, CI_low, CI_high) %>%
    mutate(across(where(is.numeric), ~ formatC(round(.x, 2), 2, format = "f"))) %>%
    mutate(Parameter = str_replace(Parameter, "b_", "")) %>%
    mutate({{ model_name }} := paste0(Mean, " (", CI_low, "-", CI_high, ")")) %>%
    # https://stackoverflow.com/questions/26003574/use-dynamic-variable-names-in-dplyr
    select(Parameter, {{ model_name }})
}

prev_model_list_df_t_df <- function(model_list) {
  prev_model_list_df <- reduce(.x = model_list, .f = left_join, by = "Parameter")

  prev_model_list_df_t <- t(prev_model_list_df)

  colnames(prev_model_list_df_t) <- prev_model_list_df_t[1, ]

  Model_names <- row.names(prev_model_list_df_t)

  prev_model_list_df_t_df <- as_tibble(prev_model_list_df_t)

  prev_model_list_df_t_df$model_names <- Model_names

  prev_model_list_df_t_df <- prev_model_list_df_t_df %>%
    select(model_names, everything())

  prev_model_list_df_t_df <- prev_model_list_df_t_df[-1, ]

  prev_model_list_df_t_df
}

# prev_model_car_1_df <- model_parameters_prev_car(prev_model_car_1, "prev_model_car_1")
# prev_model_car_2_df <- model_parameters_prev_car(prev_model_car_2, "prev_model_car_2")
# prev_model_car_3_df <- model_parameters_prev_car(prev_model_car_3, "prev_model_car_3")

##### This is for prev car models
files_to_read <- list.files(
  path = here::here("data"),
  pattern = "^prev_model_car_[0-9]{1,2}",
  full.names = TRUE
)

model_names <- str_extract(files_to_read, pattern = "prev_model_car_[0-9]{1,2}")
model_names

prev_model_list <- map(
  files_to_read,
  function(x) {
    readRDS(x)
  }
)

prev_model_list <- prev_model_list[order(readr::parse_number(model_names))]
model_names <- model_names[order(readr::parse_number(model_names))]

# saveRDS(object = prev_model_list, file = here::here("data/prev_model_list_car.rds"))
# rm(prev_model_list)
# prev_model_list <- readRDS("~/Projects/TBhotspotclusters/data/prev_model_list_car.rds")

model_names_new <- paste0("prevalence model ", 33:64)
prev_model_list_df <- map2(.x = prev_model_list, .y = model_names_new, .f = model_parameters_prev_car)
rm(prev_model_list)
# prev_model_list <- list(prev_model_car_1_df, prev_model_car_2_df, prev_model_car_3_df)

prev_model_list_df_t_df(prev_model_list_df) %>%
  rename(
    "Model names" = model_names, "Percentage of adults (=15y)" = scale_prop_adults_mean,
    "Percentage of household heads that did not complete primary school" = scale_perc_never_primary_mean,
    "Distance to nearest TB clinic (km)" = scale_clinic_distance_1km,
    "Percentage of HIV prevalence" = scale_perc_hiv_mean,
    "Percentage of male adults" = scale_perc_male_mean,
    "Random effects SD: CAR" = sdcar
  ) %>%
  gt::gt() %>%
  fmt_missing(
    everything(),
    missing_text = ""
  ) %>%
  gtsave(filename = "S3_Table_prev_models_car.rtf", path = here::here("figures"))
