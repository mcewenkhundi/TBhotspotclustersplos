# Project: TBhotspots
# Task: run all the model for prevalence with the random intercept term
# Author: McEwen Khundi

# Some steps of trying to solve the crashes that happen in Rstudio after running brms models several times
# https://github.com/rstudio/rstudio/issues/9055
library(tidyverse)
library(brms)

#import data
dat_scale <- readRDS(here::here("data","dat_scale.rds"))

# avoid asigning to the same object after rerunning a model

regressors <- c(
  "scale_prop_adults_mean", "scale_perc_never_primary_mean",
  "scale_clinic_distance_1km", "scale_perc_hiv_mean", "scale_perc_male_mean"
)

regMat <- expand.grid(
  c(TRUE, FALSE), c(TRUE, FALSE),
  c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE)
)
regMat


# regMat <- regMat[-(dim(regMat)[1]),]

# let's name the columns

names(regMat) <- regressors
regMat

allModelsList <- apply(regMat, 1, function(x) {
  paste(c("n_prev_tbcases ~ 1", regressors[x]),
    collapse = " + "
  )
})


paste0(allModelsList[[30]], " + ", "(1|cluster)", " + ", "offset(log(tent_cxr_total))")


allModelsList[[30]]

prior_prev_1 <- c(
  prior(normal(0, 10), class = "b"),
  prior(normal(0, 10), class = "Intercept"),
  prior(cauchy(0, 1), class = "sd"),
  prior(normal(0, 10), class = "zi")
)

prior_prev_2 <- c(
  prior(normal(0, 10), class = "Intercept"),
  prior(cauchy(0, 1), class = "sd"),
  prior(normal(0, 10), class = "zi")
)


model_function <- function(model_var, prior_var) {
  brm(
    formula = as.formula(model_var),
    data = dat_scale,
    family = zero_inflated_poisson(),
    control = list(adapt_delta = 0.99, max_treedepth = 10),
    # autocor=cor_car(w4, ~ 1 | scale_cluster_area, type = "icar"),
    inits = 0,
    prior = prior_var,
    cores = 3,
    iter = 15000, warmup = 1000,
    seed = 1293,
    chains = 3
  )
}

for (i in seq_along(allModelsList[1:32])) {
  print(allModelsList[1:32][[i]])

  if (allModelsList[1:32][[i]] == "n_prev_tbcases ~ 1") {
    model <- model_function(
      model = paste0(allModelsList[1:32][[i]], " + ", "(1|cluster)", " + ", "offset(log(tent_cxr_total))"),
      prior_var = prior_prev_2
    )
  } else {
    model <- model_function(
      model = paste0(allModelsList[1:32][[i]], " + ", "(1|cluster)", " + ", "offset(log(tent_cxr_total))"),
      prior_var = prior_prev_1
    )
  }

  print(paste0("prev_model_rintc_", i))
  saveRDS(object = model, file = here::here("data", paste0("prev_model_rintc_", i, ".rds")))
  rm(model)
  gc()
}
