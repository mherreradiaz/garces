source('script/funciones/paquetes.R')

library(tidyverse)
library(tidymodels)

get_rf_imp <- function(x) {
  x %>% 
    extract_fit_parsnip() %>% 
    vip::vi()
}

data <- read_rds('data/processed/modelo_potencial_old.rds') |>
  select(-(sitio:codigo))

data <- data |>
  select(-vv,-vh)

data <- data |> 
  select(-(B01:B8A))

# data <- data |> 
#   select(-pp,-eto)

data <- data |> 
  na.omit()

set.seed(123)
splits <- initial_split(data)

pot_train <- training(splits)
pot_test  <- testing(splits)

# Random Forest
library(vip)

rf_spec <- rand_forest(mode = "regression",trees = 1000) |> 
  set_engine('ranger',importance = "impurity")
  
data_folds <- vfold_cv(pot_train, v = 5)

rf_wf <- 
  workflow() |> 
  add_model(rf_spec) |> 
  add_formula(potencial_bar~.) 

ctrl_imp <- control_grid(extract = get_rf_imp)

rf_fit_resample <-
  rf_wf |> 
  fit_resamples(data_folds,control = ctrl_imp)

rf_fit <- rf_wf |> fit(data = pot_train)

df_met <- collect_metrics(rf_fit_resample) |> 
  select(-.estimator,-.config)

df_var_imp <- rf_fit_resample |> 
  select(id, .extracts) %>%
  unnest(.extracts) %>%
  unnest(.extracts) %>%
  group_by(Variable) %>%
  summarise(Mean = mean(Importance),
            Variance = sd(Importance))
df_var_imp |>   
  slice_max(Mean, n = 15) %>%
  ggplot(aes(Mean, reorder(Variable, Mean))) +
  geom_errorbar(aes(xmin = Mean - Variance, xmax = Mean + Variance)) +
  geom_point(aes(Mean)) +
  labs(x = "Variable importance", y = NULL) +
  annotate("text",x=max(df_var_imp$Mean),y=1,label =paste0('r2=',round(df_met$mean[2],2))) +
  theme_bw()
ggsave(paste0('output/figs/fig_errorbar_resample_random_forest_trends.png'),scale =1.5)

# df_var_imp <- df_var_imp |> 
#   slice_max(Mean, n = 5) |> 
#   mutate(type = vars[i],macrozona = {{macro}})
# 
# list(df_met,df_var_imp)

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
  labs(x = 'Potencial estimado (kPA)', y= 'Potencial observado (kPa)') +
  annotate("text",label = paste0('rmse = ',met[1,3],'\n r2 = ',met[2,3],'\n mae = ',met[3,3]),
           x=20,y = 2,size=2) +
  #facet_wrap(~model) + 
  coord_fixed()
ggsave('output/figs/scatterplots_estimado_observado_modelo_potencial.png',scale=2)

# Resampling

set.seed(345)
folds <- vfold_cv(pot_train, v = 10)

rf_wf <- 
  workflow() %>%
  add_model(rf_spec) %>%
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

## XGBoost

xgbst_mod <- boost_tree(
  mode = "regression",
  trees = 1000,
  tree_depth = tune(), 
  min_n = tune(),
  loss_reduction = tune(),                    ## first three: model complexity
  sample_size = tune(), 
  mtry = tune(),        ## randomness
  learn_rate = tune()  ) |> 
  set_engine("xgboost") %>%
  set_mode("regression") %>%
  translate() |> 
  fit(potencial_bar~.,data = pot_train)

#tunning


test_results <- 
  pot_test |> 
  select(potencial_bar) %>%
  bind_cols(
    predict(xgbst_mod, new_data = pot_test)
  )

test_results  |>  
  metrics(truth = potencial_bar, estimate = .pred) 

test_results |> 
  ggplot(aes(x = .pred, y = potencial_bar)) + 
  geom_abline(col = "green", lty = 2) + 
  geom_point(alpha = .4) + 
  #facet_wrap(~model) + 
  coord_fixed()
