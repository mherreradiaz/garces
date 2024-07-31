library(xgboost)
library(tidymodels)
library(tidyverse)

## Cargar los datos

# split 
data <- read_rds('data/processed/modelo_potencial_smooth.rds') |> 
  select(-(tratamiento:codigo),-(sitio:temporada)) |> 
  mutate(potencial_bar = -potencial_bar,
         fecha = ymd(fecha)) |> 
  drop_na() 

set.seed(12) #$rsq=.446
# set.seed(567) #$rsq=
# set.seed(1987) #$rsq=.566
# set.seed(875432) #rsq=.554
# set.seed(9475) #$rsq=.445
# set.seed(4567) #rsq 0.528
#

splits <- group_initial_split(data,fecha)
#splits <- initial_split(data)

pot_train <- training(splits) 
pot_test  <- testing(splits) 

#pot_val <- validation_set(splits) |> select(-(sitio:fecha))

## Modeling
### Model specification

### XGBoost
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

### LightGBM

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

### Random Forest

rf_spec <-rand_forest(
  trees = 1000,
  mtry = tune(),
  min_n = tune()) |> 
  set_engine("ranger",importance = 'impurity') |> 
  set_mode("regression")

### Support Vector Machine

svm_spec <-
  svm_rbf(cost = tune(), 
          rbf_sigma = tune())  |> 
  set_mode("regression")  |> 
  set_engine("kernlab")

## Recipe

pot_rec <- recipe(potencial_bar ~ . ,data = pot_train) |> 
  update_role(fecha, new_role = 'dont_use') |> 
  step_zv(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors())

ctrl <- control_grid(parallel_over = "everything")

vb_folds <- vfold_cv(pot_train,v=5)

pot_res <- 
  workflow_set(
    preproc = list(basic = potencial_bar ~., rec = pot_rec), 
    models = list(rf = rf_spec, 
                  svm = svm_spec,
                  xgb = xgb_spec, 
                  lgbm = lgbm_spec)
  ) |>  
  workflow_map(
    verbose = TRUE,
    seed = 1603,
    resamples = vb_folds,
    grid = 10,
    metrics = metric_set(rsq,rmse, mae),
    control = ctrl
  )

# rankear los modelos

rankings <- 
  rank_results(pot_res, select_best = TRUE) |> 
  mutate(method = map_chr(wflow_id, ~ str_split(.x, "_", simplify = TRUE)[1])) 

tidymodels_prefer()
filter(rankings, rank <= 10) |>  
  dplyr::select(rank, mean, model, method, .metric) |> 
  filter(.metric == 'rsq') |> 
  ggplot(aes(rank,mean,color = model,shape = method)) + 
  geom_point() + 
  theme_bw()

# fit and test the best model y extraer los mejores modelos

xgb_res <- 
  pot_res |>  
  extract_workflow("basic_xgb") %>% 
  finalize_workflow(
    pot_res |>  
      extract_workflow_set_result("basic_xgb") %>% 
      select_best(metric = "rsq")
  ) |>  
  last_fit(split = splits, metrics = metric_set(rsq,rmse,mae))

rf_res <- 
  pot_res |>  
  extract_workflow("basic_rf") %>% 
  finalize_workflow(
    pot_res |>  
      extract_workflow_set_result("basic_rf") %>% 
      select_best(metric = "rsq")
  ) |>  
  last_fit(split = splits, metrics = metric_set(rsq,rmse,mae))

svm_res <- 
  pot_res |>  
  extract_workflow("basic_svm") %>% 
  finalize_workflow(
    pot_res |>  
      extract_workflow_set_result("basic_svm") %>% 
      select_best(metric = "rsq")
  ) |>  
  last_fit(split = splits, metrics = metric_set(rsq,rmse,mae))


xgb_wflow_fit <- extract_workflow(xgb_res)
rf_wflow_fit <- extract_workflow(rf_res)
svm_wflow_fit <- extract_workflow(svm_res)

write_rds(xgb_wflow_fit,'data/processed/modelos/xgboost.rds')
write_rds(rf_wflow_fit,'data/processed/modelos/random_forest.rds')
write_rds(svm_wflow_fit,'data/processed/modelos/support_vector_machine.rds')

df_metrics <- bind_rows(
  tibble(collect_metrics(xgb_res),model = 'XGBoost'),
  tibble(collect_metrics(rf_res),model = 'RF'),
  tibble(collect_metrics(svm_res),model = 'SVM')
)

df_metrics <- df_metrics |> 
  mutate(
    .metric = toupper(.metric),
    x = -1.2,
    y = case_when(
      .metric == 'RSQ' ~ -3,
      .metric == 'MAE' ~ -3.1,
      .default = -3.2),
    unit = case_when(
      .metric == 'RSQ' ~ '',
      .default = '~~MPa'),
    
    .metric = case_when(.metric == 'RSQ' ~ 'R^2',
                        .default = .metric)
  )

###

test_results_xgb <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(xgb_wflow_fit, new_data = pot_test)
  ) |> 
  bind_cols(model = 'XGBoost')

test_results_rf <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(rf_wflow_fit, new_data = pot_test)
  ) |> 
  bind_cols(model = 'RF')

test_results_svm <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(xgb_wflow_fit, new_data = pot_test)
  ) |> 
  bind_cols(model = 'SVM')


tst_res <- bind_rows(test_results_rf,
          test_results_svm,
          test_results_xgb)

## Visualización de los resultados

tst_res |> 
  ggplot(aes(x = .pred*0.1, y = potencial_bar*0.1)) + 
  geom_abline(col = "darkgreen", lty = 2,lwd=1) + 
  geom_point(alpha = .4) + 
  labs(x = expression(paste(Psi[s],' estimated (MPa)')), 
       y = expression(paste(Psi[s],' observed (MPa)'))) +
  geom_text(data = df_metrics,
            aes(x,y,label = paste(.metric,'==',round(.estimate,2),unit)),
            parse = TRUE) +
  #geom_smooth(method = 'lm') +
  #annotate("text",aes(label = paste0(.metric,'=',.estimate))) +
  # RMSE = ',met[1,3],'\n MAE = ',met[3,3]),
  #          x=-10,y = 0,size=5) +
  # annotate("text",label = paste("R^{2}==",met[2,3]),
  #          x=-11,y=0,size=5,parse = TRUE) +
  # #annotate("text")
  facet_grid(.~model) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))
# ggsave('output/figs/pred_vs_obser_models_random_split.png',
#        scale=1.5,width = 10,height = 6,
#        device = cairo_pdf)
ggsave('output/figs/pred_vs_obser_models_group_split.png',
       scale=1.2,width = 8,height = 6, device = png, type = "cairo",
       dpi = 300)

