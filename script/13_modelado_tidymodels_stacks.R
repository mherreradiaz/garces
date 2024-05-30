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

# Random Forest con tunning
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
  control = ctrl_res
)

# Revisar como afectan los parametros a la metrica RMSE
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
  labs(x = NULL, y = "RMSE")

# se ajusta el tunning de acuerod al análisis del gráfico anterior
# 
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

#SVM con tunning
# create a model definition
svm_spec <- 
  svm_rbf(
    cost = tune("cost"), 
    rbf_sigma = tune("sigma")
  ) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

svm_wflow <- 
  workflow() %>% 
  add_model(svm_spec) %>%
  add_recipe(pot_rec)

set.seed(2020)
svm_res <- 
  tune_grid(
    svm_wflow, 
    resamples = folds, 
    grid = 6,
    metrics = metric,
    control = ctrl_grid
  )

svm_res |> collect_metrics()

# Ensmablando modelos con stacks
# 
stacks()

pot_data_st <- 
  stacks() |> 
  add_candidates(tune_res) |> 
  add_candidates(svm_res)

as_tibble(pot_data_st)

pot_model_st <-
  pot_data_st |> 
  blend_predictions()

autoplot(pot_model_st)
autoplot(pot_model_st,type = 'members')
autoplot(pot_model_st,type = 'weights')

pot_model_st <-
  pot_model_st %>%
  fit_members()

pot_test <- 
  pot_test %>%
  bind_cols(predict(pot_model_st, .))

ggplot(pot_test) +
  aes(x = .pred, 
      y = potencial_bar) +
  geom_abline(col='red') +
  geom_point(alpha = .4) + 
  coord_obs_pred() +
  theme_bw()

member_preds <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(predict(pot_model_st, pot_test, members = TRUE))

map(member_preds, rsq_vec, truth = member_preds$potencial_bar)  |> 
  as_tibble()