#Project: TB hotspots
#Task: prevalence models comparison using loo statistics
#Author: McEwen Khundi

#Load all required packages for analysis.

library(tidyverse)    #for data manipulation
library(pmthemes)     #for ggplot themes
library(knitr)        #for tables
library(brms)
library(gt)

# Import datasets
# Import data required for the analysis.
# Model naming convention: models with rintc have a random intercept term and models with car have conditional autoregressive term.


files_to_read <- list.files(
  path = here::here("data"),
  pattern = "^prev_model_(car|rintc)_[0-9]{1,2}",
  full.names = TRUE
)

model_names <- str_extract(files_to_read,pattern = "prev_model_(car|rintc)_[0-9]{1,2}")
model_names

#Get the models into a list
model_list = map(files_to_read,
                 function(x) {readRDS(x)})


#Check one model to be sure of what is going on.
model_list[[1]]

# Compare the models using loo_compare.

loo_list <- map(model_list,~loo(.x,k_threshold=0.7))
rm(model_list)

#Naming the list elements
names(loo_list) <- model_names

loo_list_comp <- loo_compare(loo_list)
saveRDS(object = loo_list_comp, file = here::here("data/prev_loo_list_comp.rds"))

prev_loo_list_comp <- readRDS(here::here("data/prev_loo_list_comp.rds"))

loo_list_comp <- prev_loo_list_comp[1:10, 1:2] #only keep top10 models



## Top ten models exported to word
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
  gt::gtsave(filename = "S5_Table_top10_prev_models.rtf", path = here::here("figures"))





















