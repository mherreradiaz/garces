library(tidyverse)
library(tidymodels)
library(stacks)

data <- read_rds('data/processed/modelo_potencial.rds') |> 
  select(-(temporada:codigo))

set.seed(123)
splits      <- initial_split(data, strata = sitio)

pot_train <- training(splits)
pot_test  <- testing(splits)

set.seed(321)
folds <- rsample::vfold_cv(pot_train, v = 5)

pot_rec <- 
  recipe(potencial_bar ~ ., data = pot_train)

metric <- metric_set(rmse)

ctrl_grid <- control_stack_grid()
ctrl_res <- control_stack_resamples()

# Random Forest
# 
rf_mod <- rand_forest(
  mode = "regression",
  mtry = tune(),
  trees = 1000,
  min_n = tune()) |> 
  set_engine('ranger') 

rf_spec <-
  pot_rec |> 
  step_impute_mean(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors())

#workflow
#
rf_wflow <- 
  workflow() |> 
  add_model(rf_mod) %>%
  add_recipe(pot_rec)

doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  rf_wflow,
  resamples = folds,
  grid = 20
)

tune_res |> 
  collect_metrics() |> 
  filter(.metric == "rmse") |> 
  select(mean, min_n, mtry) |> 
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) |> 
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

rf_grid <- grid_regular(
  mtry(range = c(15, 20)),
  min_n(range = c(1, 10)),
  levels = 5
)

rf_grid

set.seed(456)
regular_res <- tune_grid(
  rf_wflow,
  resamples = folds,
  grid = rf_grid
)

regular_res

regular_res |> 
  collect_metrics()
