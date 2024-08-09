library(xgboost)
library(tidymodels)
library(tidyverse)
library(glue)

## Cargar los datos
# rnd_split or tme_split
split <- 'tme_split'

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

if(split == 'rnd_split'){
  splits <- initial_split(data) #aleatorio, sobreajusta
  } else {
    splits <- group_initial_split(data,fecha) #independiente de la fecha
  }

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

pot_rec_pca <- pot_rec |> 
  step_pls(all_numeric_predictors(),outcome = 'potencial_bar')

ctrl <- control_grid(parallel_over = "everything")

set.seed(432)

if (split =='rnd_split'){
  vb_folds <- vfold_cv(pot_train,v=5)
} else vb_folds <- group_vfold_cv(pot_train,fecha,v=5)


pot_res <- 
  workflow_set(
    preproc = list(rec = pot_rec, rec_pca = pot_rec_pca), 
    models = list(RF = rf_spec, 
                  SVM = svm_spec,
                  XGBoost = xgb_spec) 
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
rankings |>  
  dplyr::select(rank, mean, model, wflow_id, .metric,std_err) |> 
  filter(.metric == 'rsq') |> 
  rename(Model = wflow_id) |> 
  mutate(Model = str_remove(Model,'rec_'),
         split = split) |> 
  write_rds(glue('data/processed/eval_modelos/{split}_rankings.rds'))

xgb_res <- 
  pot_res |>  
  extract_workflow("rec_XGBoost") %>% 
  finalize_workflow(
    pot_res |>  
      extract_workflow_set_result("rec_XGBoost") %>% 
      select_best(metric = "rsq")
  ) |>  
  last_fit(split = splits, metrics = metric_set(rsq,rmse,mae))

rf_res <- 
  pot_res |>  
  extract_workflow("rec_RF") %>% 
  finalize_workflow(
    pot_res |>  
      extract_workflow_set_result("rec_RF") %>% 
      select_best(metric = "rsq")
  ) |>  
  last_fit(split = splits, metrics = metric_set(rsq,rmse,mae))

svm_res <- 
  pot_res |>  
  extract_workflow("rec_SVM") %>% 
  finalize_workflow(
    pot_res |>  
      extract_workflow_set_result("rec_SVM") %>% 
      select_best(metric = "rsq")
  ) |>  
  last_fit(split = splits, metrics = metric_set(rsq,rmse,mae))


xgb_wflow_fit <- extract_workflow(xgb_res)
rf_wflow_fit <- extract_workflow(rf_res)
svm_wflow_fit <- extract_workflow(svm_res)

write_rds(xgb_wflow_fit,glue('data/processed/modelos/xgboost_{split}.rds'))
write_rds(rf_wflow_fit,glue('data/processed/modelos/random_forest_{split}.rds'))
write_rds(svm_wflow_fit,glue('data/processed/modelos/support_vector_machine_{split}.rds'))

df_metrics <- bind_rows(
  tibble(collect_metrics(xgb_res),model = 'XGBoost'),
  tibble(collect_metrics(rf_res),model = 'RF'),
  tibble(collect_metrics(svm_res),model = 'SVM')
)

df_metrics <- df_metrics |> 
  mutate(
    split = split,
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

write_rds(df_metrics,glue('data/processed/eval_modelos/metrics_{split}.rds'))

### Predecir en el set de testeo

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
          test_results_xgb) |> 
  mutate(split = split)

write_rds(tst_res,glue('data/processed/eval_modelos/pred_vs_obs_{split}.rds'))

## Visualización de los resultados

# Rankings models

data_rank <- bind_rows(
  read_rds('data/processed/eval_modelos/rnd_split_rankings.rds'),
  read_rds('data/processed/eval_modelos/tme_split_rankings.rds')
)

data_rank |> 
  ggplot(aes(rank,mean,color = Model)) +
  geom_errorbar(aes(ymin = mean - std_err,ymax = mean + std_err)) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(breaks = seq(0.25,0.85,0.05)) +
  scale_color_viridis_d() +
  labs(y = expression(R^2)) +
  geom_point() +
  facet_grid(.~split) +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.background = element_rect(fill = 'white'))
ggsave('output/figs/fig_rankings_models.png',scale = 1,width=8,height=5)


##
data_test <- bind_rows(
  read_rds('data/processed/eval_modelos/pred_vs_obs_rnd_split.rds'),
  read_rds('data/processed/eval_modelos/pred_vs_obs_tme_split.rds'))

df_metrics <- bind_rows(
  read_rds('data/processed/eval_modelos/metrics_rnd_split.rds'),
  read_rds('data/processed/eval_modelos/metrics_tme_split.rds')
  )

data_test |> 
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
  facet_grid(split~model) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))
ggsave('output/figs/pred_vs_obser_models.png',
       scale=1.5,width = 10,height = 8, device = png, type = "cairo",
       dpi = 300)

