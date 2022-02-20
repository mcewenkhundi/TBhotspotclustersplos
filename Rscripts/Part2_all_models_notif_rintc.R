# Project: TBhotspots
# Task: run all the model for notification with the random intercept term
# Author: McEwen Khundi


# Some steps of trying to solve the crashes that happen in Rstudio after running brms models several times
library(tidyverse)
library(brms)

#impot data
dat_scale_ln <- readRDS(here::here("data","dat_scale_ln.rds"))

# https://github.com/rstudio/rstudio/issues/9055

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
  paste(c("total_confirmed ~ 1", regressors[x]),
    collapse = " + "
  )
})


paste0(allModelsList[[10]], " + ", "tb_year", " + ", "(1|cluster)", " + ", "offset(log(population))")


allModelsList[[10]]

prior_notif_1 <- c(
  prior(normal(0, 10), class = "b"),
  prior(normal(0, 10), class = "Intercept"),
  prior(cauchy(0, 1), class = "sd")
)

prior_notif_2 <- c(
  prior(normal(0, 10), class = "Intercept"),
  prior(cauchy(0, 1), class = "sd")
)


model_function <- function(model_var, prior_var) {
  brm(
    formula = as.formula(model_var),
    data = dat_scale_ln,
    family = poisson,
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

  if (allModelsList[1:32][[i]] == "total_confirmed ~ 1") {
    model <- model_function(
      model = paste0(allModelsList[1:32][[i]], " + ", "tb_year", " + ", "(1|cluster)", " + ", "offset(log(population))"),
      prior_var = prior_notif_2
    )
  } else {
    model <- model_function(
      model = paste0(allModelsList[1:32][[i]], " + ", "tb_year", " + ", "(1|cluster)", " + ", "offset(log(population))"),
      prior_var = prior_notif_1
    )
  }

  print(paste0("notif_model_rintc_", i))
  saveRDS(object = model, file = here::here("data", paste0("notif_model_rintc_", i, ".rds")))
  rm(model)
  gc()
}
