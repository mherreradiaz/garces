library(tidyverse)
library(tidymodels)

data <- read_rds('data/processed/modelo_potencial.rds') |> 
  select(-(temporada:codigo))

set.seed(123)
splits      <- initial_split(data, strata = sitio)

pot_train <- training(splits)
pot_test  <- testing(splits)

# Random Forest
# 
rf_mod <- rand_forest(mode = "regression",trees = 1000) |> 
  set_engine('ranger') |> 
  fit(potencial_bar~.,data = pot_train)
  
test_results <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(rf_mod, new_data = pot_test)
  )

test_results  |>  
  metrics(truth = potencial_bar, estimate = .pred) 

test_results |> 
  ggplot(aes(x = .pred, y = potencial_bar)) + 
  geom_abline(col = "green", lty = 2) + 
  geom_point(alpha = .4) + 
  #facet_wrap(~model) + 
  coord_fixed()

# Resampling

set.seed(345)
folds <- vfold_cv(pot_train, v = 10)

rf_wf <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_formula(potencial_bar ~ .)

set.seed(456)
rf_fit_rs <- 
  rf_wf |> 
  fit_resamples(folds)

collect_metrics(rf_fit_rs)
  
## LightGBM
library(bonsai)
lgbm_mod <- boost_tree(
  mode = "regression",
  trees = 1000) |> 
  set_engine("lightgbm") %>%
  set_mode("regression") %>%
  translate() |> 
  fit(potencial_bar~.,data = pot_train)


test_results <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(lgbm_mod, new_data = pot_test)
  )

test_results  |>  
  metrics(truth = potencial_bar, estimate = .pred) 

test_results |> 
  ggplot(aes(x = .pred, y = potencial_bar)) + 
  geom_abline(col = "green", lty = 2) + 
  geom_point(alpha = .4) + 
  #facet_wrap(~model) + 
  coord_fixed()
