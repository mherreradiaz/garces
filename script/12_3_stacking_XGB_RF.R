library(xgboost)
library(tidymodels)
library(tidyverse)

## Cargar los datos

# split por temporada
data <- read_rds('data/processed/modelo_potencial.rds') |> 
  select(-(tratamiento:codigo)) |> 
  mutate(potencial_bar = -potencial_bar,
         fecha = ymd(fecha)) |> 
  drop_na() 

# set.seed(12) #buenos resultado
# set.seed(876) #mal
# set.seed(123) #regular
# set.seed(321) #regular
# set.seed(987)

splits <- group_initial_split(data,fecha)

pot_train <- training(splits) |> select(-(sitio:fecha))
pot_test  <- testing(splits) |> select(-(sitio:fecha))

cbind(pot_train$potencial_bar,pot_test$potencial_bar) |> boxplot()


## LightGBM

lgbm_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),                    ## first three: model complexity
  sample_size = tune(), mtry = tune(),        ## randomness
  learn_rate = tune()                         ## step size
) |> 
  set_engine("lightgbm") |> 
  set_mode("regression")

#tunning

lgbm_ctrl_grid <- control_stack_grid()
lgbm_ctrl_res <- control_stack_resamples()

lgbm_wf <- workflow() |> 
  add_model(lgbm_spec) |> 
  add_formula(potencial_bar~.) 

lgbm_wf

set.seed(123)
vb_folds <- vfold_cv(pot_train,v=5)

vb_folds
 
set.seed(234)
lgbm_res <-tune_grid(
  lgbm_wf,
  resamples = vb_folds,
  grid = 10,
  control = lgbm_ctrl_grid)
lgbm_res

## Explorar resultados

collect_metrics(lgbm_res)

lgbm_res |> 
  collect_metrics() |> 
  filter(.metric == "rsq") |> 
  select(mean, mtry:sample_size) |> 
  pivot_longer(mtry:sample_size,
               names_to = "parameter",
               values_to = "value") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE)+
  facet_wrap(~parameter, scales = "free_x")

show_best(lgbm_res, metric =  "rsq")

best_rsq <- select_best(lgbm_res, metric = "rsq")

final_lgbm <- finalize_workflow(
  lgbm_wf,
  best_rsq
)

final_lgbm

#train

lgbm_fit <- final_lgbm |> fit(data = pot_train)

write_rds(lgbm_fit,'data/processed/modelos/lightgbm.rds')

test_results_lgbm <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(lgbm_fit, new_data = pot_test)
  )

met_lgbm <- test_results_lgbm  |>  
  metrics(truth = potencial_bar, estimate = .pred) |> 
  mutate(.estimate = round(.estimate,2))

# test_results |> 
#   ggplot(aes(x = .pred, y = potencial_bar)) + 
#   geom_abline(col = "green", lty = 2) + 
#   geom_point(alpha = .4) + 
#   labs(x = 'SWP estimated (kPA)', y = 'SWP observed (kPa)') +
#   annotate("text",label = paste0('RMSE = ',met[1,3],'\n MAE = ',met[3,3]),
#            x=0,y = 4,size=5) +
#   annotate("text",label = paste("R^{2}==",met[2,3]),
#            x=1,y=8,size=5,parse = TRUE) +
#   #annotate("text")
#   theme_bw()
# ggsave('output/figs/pred_vs_obser_lightgbm.png',scale=1.2)


## XGBoost

xgb_spec <- boost_tree(
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
vb_folds <- vfold_cv(pot_train,v=5)

vb_folds

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

xgb_fit <- final_xgb |> fit(data = pot_train)

write_rds(xgb_fit,'data/processed/modelos/xgboost.rds')

test_results_xgb <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(xgb_fit, new_data = pot_test)
  )

met_xgb <- test_results_xgb  |>  
  metrics(truth = potencial_bar, estimate = .pred) |> 
  mutate(.estimate = round(.estimate,2))

# test_results |> 
#   ggplot(aes(x = .pred, y = potencial_bar)) + 
#   geom_abline(col = "green", lty = 2) + 
#   geom_point(alpha = .4) + 
#   labs(x = 'SWP estimated (kPA)', y= 'SWP observed (kPa)') +
#   annotate("text",label = paste0('RMSE = ',met[1,3],'\n MAE = ',met[3,3]),
#            x=0,y = 4,size=5) +
#   annotate("text",label = paste("R^{2}==",met[2,3]),
#            x=1,y=8,size=5,parse = TRUE) +
#   #annotate("text")
#   theme_bw()
# ggsave('output/figs/pred_vs_obser_xgboost.png',scale=1.2)


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
vb_folds <- vfold_cv(pot_train,v=5)

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

rf_fit <- final_rf |> fit(data = pot_train)

write_rds(rf_fit,'data/processed/modelos/random_forest.rds')

test_results_rf <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(rf_fit, new_data = pot_test)
  )

met_rf <- test_results_rf  |>  
  metrics(truth = potencial_bar, estimate = .pred) |> 
  mutate(.estimate = round(.estimate,2))

# test_results |> 
#   ggplot(aes(x = .pred, y = potencial_bar)) + 
#   geom_abline(col = "green", lty = 2) + 
#   geom_point(alpha = .4) + 
#   labs(x = 'SWP estimated (kPA)', y= 'SWP observed (kPa)') +
#   annotate("text",label = paste0('RMSE = ',met[1,3],'\n MAE = ',met[3,3]),
#            x=20,y = 4,size=5) +
#   annotate("text",label = paste("R^{2}==",met[2,3]),
#            x=21,y=8,size=5,parse = TRUE) +
#   #annotate("text")
#   theme_bw()
# ggsave('output/figs/pred_vs_obser_random_forest.png',scale=1.2)

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
vb_folds <- vfold_cv(pot_train,v=5)

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

show_best(xgb_res, metric = "rsq")

best_rsq <- select_best(svm_res, metric = "rsq")

final_svm <- finalize_workflow(
  svm_wf,
  best_rsq
)

final_svm

#train

svm_fit <- final_svm |> fit(data = pot_train)

write_rds(svm_fit,'data/processed/modelos/support_vector_machine.rds')

test_results_svm <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(svm_fit, new_data = pot_test)
  )

met_svm <- test_results_svm  |>  
  metrics(truth = potencial_bar, estimate = .pred) |> 
  mutate(.estimate = round(.estimate,2))


test_results_lgbm <- test_results_lgbm |> mutate(model = 'lightGBM')
test_results_rf <- test_results_rf |> mutate(model = 'RF')
test_results_svm <- test_results_svm |> mutate(model = 'SVM')
test_results_xgb <- test_results_xgb |> mutate(model = 'xgBoost')

test_results <- bind_rows(test_results_lgbm,
                          test_results_rf,test_results_svm,test_results_xgb)

met_lgbm <- met_lgbm |> mutate(model = 'lightGBM')
met_rf <- met_rf |> mutate(model = 'RF')
met_svm <- met_svm |> mutate(model = 'SVM')
met_xgb <- met_xgb |> mutate(model = 'xgBoost')

met <- bind_rows(met_lgbm,met_rf,met_svm,met_xgb)
met <- met |> mutate(x=-10,y = 0)

met <- met |> mutate(y = case_when(
  .metric == 'rsq' ~ -1,
  .metric == 'mae' ~ -2,
  .default = 0))

test_results |> 
  ggplot(aes(x = .pred, y = potencial_bar)) + 
  geom_abline(col = "green", lty = 2) + 
  geom_point(alpha = .4) + 
  labs(x = 'SWP estimated (kPA)', y= 'SWP observed (kPa)') +
  geom_text(data = met,aes(x,y,label = paste0(.metric,'=',.estimate))) +
  #annotate("text",aes(label = paste0(.metric,'=',.estimate))) +
  # RMSE = ',met[1,3],'\n MAE = ',met[3,3]),
  #          x=-10,y = 0,size=5) +
  # annotate("text",label = paste("R^{2}==",met[2,3]),
  #          x=-11,y=0,size=5,parse = TRUE) +
  # #annotate("text")
  facet_grid(.~model) +
  theme_bw()
ggsave('output/figs/pred_vs_obser_models.png',scale=1.2)


# ensemble models

library(stacks)
data_potencial_st <- stacks() |> 
  add_candidates(xgb_res) |> 
  add_candidates(rf_res) |> 
  add_candidates(svm_res)

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
