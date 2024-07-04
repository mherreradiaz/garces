library(xgboost)
library(tidymodels)
library(tidyverse)

## Cargar los datos

<<<<<<< HEAD
data <- read_rds('data/processed/modelo_potencial_smoothbiopar.rds') |>
=======
data <- read_rds('data/processed/modelo_potencial.rds') |>
>>>>>>> 9e60338 (actualizar 12_4)
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
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),                    ## first three: model complexity
  sample_size = tune(), mtry = tune(),        ## randomness
  learn_rate = tune()                         ## step size
) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

#tunning

xgb_ctrl_grid <- control_stack_grid()
xgb_ctrl_res <- control_stack_resamples()

xgb_wf <- workflow() |> 
  add_model(xgb_spec) |> 
  add_formula(potencial_bar~.) 

xgb_wf

set.seed(123)
vb_folds <- vfold_cv(pot_train |> select(-sitio),v=5)

vb_folds

# library(finetune)
# doParallel::registerDoParallel()

set.seed(234)
xgb_res <-tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = 10,
  control = xgb_ctrl_grid)
xgb_res

## Explorar resultados

collect_metrics(xgb_res)

xgb_res |> 
  collect_metrics() |> 
  filter(.metric == "rsq") |> 
  select(mean, mtry:sample_size) |> 
  pivot_longer(mtry:sample_size,
               names_to = "parameter",
               values_to = "value") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE)+
  facet_wrap(~parameter, scales = "free_x")

show_best(xgb_res, metric =  "rsq")

best_rsq <- select_best(xgb_res, metric = "rsq")

final_xgb <- finalize_workflow(
  xgb_wf,
  best_rsq
)

final_xgb

#train

xgb_fit <- final_xgb |> fit(data = pot_train |> select(-sitio))

write_rds(xgb_fit,'data/processed/modelos/xgboost.rds')

test_results <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(xgb_fit, new_data = pot_test)
  )

met <- test_results  |>  
  metrics(truth = potencial_bar, estimate = .pred) |> 
  mutate(.estimate = round(.estimate,2))

test_results |> 
  ggplot(aes(x = .pred, y = potencial_bar)) + 
  geom_abline(col = "green", lty = 2) + 
  geom_point(alpha = .4) + 
  labs(x = 'SWP estimated (kPA)', y= 'SWP observed (kPa)') +
  annotate("text",label = paste0('RMSE = ',met[1,3],'\n MAE = ',met[3,3]),
           x=20,y = 4,size=5) +
  annotate("text",label = paste("R^{2}==",met[2,3]),
           x=21,y=8,size=5,parse = TRUE) +
  #annotate("text")
  theme_bw()
ggsave('output/figs/pred_vs_obser_xgboost.png',scale=1.2)

## Tunning Random Forest
  
rf_spec <-rand_forest(
  trees = 1000,
  mtry = tune(),
  min_n = tune()) |> 
  set_engine("ranger",importance = 'impurity') |> 
  set_mode("regression")

rf_ctrl_grid <- control_stack_grid()
rf_ctrl_res <- control_stack_resamples()

rf_wf <- workflow() |> 
  add_model(rf_spec) |> 
  add_formula(potencial_bar~.) 

rf_wf

set.seed(123)
vb_folds <- vfold_cv(pot_train |> select(-sitio),v=5)

vb_folds
  
set.seed(234)
rf_res <-tune_grid(
  rf_wf,
  resamples = vb_folds,
  grid = 10,
  control = rf_ctrl_grid
)
rf_res

## Explorar resultados

collect_metrics(rf_res)

rf_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  select(mean, mtry:min_n) %>%
  pivot_longer(mtry:min_n,
               names_to = "parameter",
               values_to = "value") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE)+
  facet_wrap(~parameter, scales = "free_x")

show_best(rf_res, metric = "rsq")

best_rsq <- select_best(rf_res, metric = "rsq")

final_rf <- finalize_workflow(
  rf_wf,
  best_rsq
)

final_rf

#train

rf_fit <- final_rf |> fit(data = pot_train |> select(-sitio))

write_rds(rf_fit,'data/processed/modelos/random_forest.rds')

test_results <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(rf_fit, new_data = pot_test)
  )

met <- test_results  |>  
  metrics(truth = potencial_bar, estimate = .pred) |> 
  mutate(.estimate = round(.estimate,2))

test_results |> 
  ggplot(aes(x = .pred, y = potencial_bar)) + 
  geom_abline(col = "green", lty = 2) + 
  geom_point(alpha = .4) + 
  labs(x = 'SWP estimated (kPA)', y= 'SWP observed (kPa)') +
  annotate("text",label = paste0('RMSE = ',met[1,3],'\n MAE = ',met[3,3]),
           x=20,y = 4,size=5) +
  annotate("text",label = paste("R^{2}==",met[2,3]),
           x=21,y=8,size=5,parse = TRUE) +
  #annotate("text")
  theme_bw()
ggsave('output/figs/pred_vs_obser_random_forest.png',scale=1.2)

# SVM

svm_spec <-
  svm_rbf(cost = tune(), 
          rbf_sigma = tune())  |> 
  set_mode("regression")  |> 
  set_engine("kernlab")

svm_ctrl_grid <- control_stack_grid()
svm_ctrl_res <- control_stack_resamples()

svm_wf <- workflow() |> 
  add_model(svm_spec) |> 
  add_formula(potencial_bar~.) 

svm_wf

set.seed(123)
vb_folds <- vfold_cv(pot_train |> select(-sitio),v=5)

vb_folds

# library(finetune)
# doParallel::registerDoParallel()

set.seed(234)
svm_res <-tune_grid(
  svm_wf,
  resamples = vb_folds,
  grid = 10,
  control = svm_ctrl_grid)
svm_res

## Explorar resultados

collect_metrics(svm_res)

svm_res |> 
  collect_metrics() |> 
  filter(.metric == "rsq") |> 
  select(mean, cost:rbf_sigma) |> 
  pivot_longer(cost:rbf_sigma,
               names_to = "parameter",
               values_to = "value") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE)+
  facet_wrap(~parameter, scales = "free_x")

show_best(xgb_res, "rsq")

best_rsq <- select_best(svm_res, "rsq")

final_svm <- finalize_workflow(
  svm_wf,
  best_rsq
)

final_svm

#train


svm_fit <- final_svm |> fit(data = pot_train |> select(-sitio))

write_rds(svm_fit,'data/processed/modelos/support_vector_machine.rds')

test_results <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(svm_fit, new_data = pot_test)
  )

met <- test_results  |>  
  metrics(truth = potencial_bar, estimate = .pred) |> 
  mutate(.estimate = round(.estimate,2))

test_results |> 
  ggplot(aes(x = .pred, y = potencial_bar)) + 
  geom_abline(col = "green", lty = 2) + 
  geom_point(alpha = .4) + 
  labs(x = 'SWP estimated (kPA)', y= 'SWP observed (kPa)') +
  annotate("text",label = paste0('RMSE = ',met[1,3],'\n MAE = ',met[3,3]),
           x=20,y = 4,size=5) +
  annotate("text",label = paste("R^{2}==",met[2,3]),
           x=21,y=8,size=5,parse = TRUE) +
  #annotate("text")
  theme_bw()
ggsave('output/figs/pred_vs_obser_support_vector_machine.png',scale=1.2)


# ensemble models

library(stacks)
data_potencial_st <- stacks() |> 
  add_candidates(xgb_res) |> 
  add_candidates(rf_res)

potencial_model_st <-
  data_potencial_st %>%
  blend_predictions()

potencial_model_st <-
  potencial_model_st %>%
  fit_members()

potencial_test <- 
  pot_test %>% 
  bind_cols(predict(potencial_model_st, .))

rsq(potencial_test,'potencial_bar','.pred')
rmse(potencial_test,'potencial_bar','.pred')
