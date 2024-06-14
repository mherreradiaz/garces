library(xgboost)
library(tidymodels)
library(tidyverse)

## Cargar los datos

data <- read_rds('data/processed/modelo_potencial.rds') |>
  select(-(temporada:codigo))

data <- data |>
  select(-vv,-vh)

data <- data |> 
  select(-(B01:B8A))

data <- data |> 
  na.omit()

set.seed(123)
splits <- initial_split(data)

pot_train <- training(splits)
pot_test  <- testing(splits)

## XGBoost

xgb_spec <-boost_tree(
  trees = 500,
  tree_depth = tune(), 
  min_n = tune(),
  loss_reduction = tune(),                    ## first three: model complexity
  sample_size = tune(), mtry = tune(),        ## randomness
  learn_rate = tune()                         ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

#tunning

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), pot_train),
  learn_rate(),
  size = 20
)

xgb_wf <- workflow() |> 
  add_formula(potencial_bar~.) |> 
  add_model(xgb_spec)

xgb_wf

set.seed(123)
vb_folds <- vfold_cv(pot_train, strata = sitio)

vb_folds

library(finetune)
doParallel::registerDoParallel()

set.seed(234)
xgb_res <-tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred  = TRUE)
)
xgb_res

## Explorar resultados

collect_metrics(xgb_res)

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               names_to = "parameter",
               values_to = "value") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE)+
  facet_wrap(~parameter, scales = "free_x")
