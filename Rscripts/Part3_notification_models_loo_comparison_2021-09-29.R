# Project: TB hotspots
# Task: notification models comparison using loo statistics
# Author: McEwen Khundi

# Import libraries
library(tidyverse) # for data manipulation
library(pmthemes) # for ggplot themes
library(knitr)
library(gt)
library(brms)

# import datasets
# Import data required for the analysis.
# Model naming convention: models with rintc have a random intercept term and models with car have a conditional autoregressive term.

files_to_read <- list.files(
  path = here::here("data"),
  pattern = "^notif_model_(car|rintc)_[0-9]{1,2}",
  full.names = TRUE
)

model_names <- str_extract(files_to_read, pattern = "notif_model_(car|rintc)_[0-9]{1,2}")
model_names

# Get the models into a list
model_list <- map(
  files_to_read,
  function(x) {
    readRDS(x)
  }
)

## Check one model to be sure of what is going on.
model_list[[1]]

## Compare the models using loo_compare.

loo_list <- map(model_list, ~ loo(.x, k_threshold = 0.7))

rm(model_list)
# Naming the list elements
names(loo_list) <- model_names

loo_list_comp <- loo_compare(loo_list)

saveRDS(object = loo_list_comp, file = here::here("data/notif_loo_list_comp.rds"))

#notif_loo_list_comp <- readRDS(here::here("data/notif_loo_list_comp.rds"))

loo_list_comp <- loo_list_comp[1:10, 1:2] # only keep top 10 models


# export the top ten best models to word
loo_list_comp_rownames <- row.names(loo_list_comp)


loo_list_comp_df <- as_tibble(loo_list_comp)

loo_list_comp_df <- loo_list_comp_df %>%
  mutate(Model_name = loo_list_comp_rownames, .before = 1) %>%
  slice(1:10)


loo_list_comp_df %>%
  gt::gt() %>%
  gt::fmt_missing(
    everything(),
    missing_text = ""
  ) %>%
  gt::gtsave(filename = "S6_Table_top10_notif_models.rtf", path = here::here("figures"))
