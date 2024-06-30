library(tidyverse)
library(tidymodels)

get_rf_imp <- function(x) {
  x |> 
    extract_fit_parsnip() |> 
    vip::vi()
}

data <- read_rds('data/processed/modelo_potencial.rds') |>
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

# XGBoost
library(vip)

data_folds <- vfold_cv(pot_train, v = 5)

xgb_wf <- read_rds('data/processed/modelos/xgboost.rds')

ctrl_imp <- control_grid(extract = get_rf_imp)

xgb_fit_resample <-
  xgb_wf |> 
  fit_resamples(data_folds,control = ctrl_imp)

df_met <- collect_metrics(xgb_fit_resample) |> 
  select(-.estimator,-.config)

df_var_imp <- xgb_fit_resample |> 
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
ggsave(paste0('output/figs/fig_errorbar_resample_xgboost.png'),scale =1.5)

# Random Forest
data_folds <- vfold_cv(pot_train, v = 5)

rf_wf <- read_rds('data/processed/modelos/random_forest.rds')

ctrl_imp <- control_grid(extract = get_rf_imp)

rf_fit_resample <-
  rf_wf |> 
  fit_resamples(data_folds,control = ctrl_imp)

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
ggsave(paste0('output/figs/fig_errorbar_resample_random_forest.png'),scale =1.5)

# Support Vector Machine
data_folds <- vfold_cv(pot_train, v = 5)

svm_wf <- read_rds('data/processed/modelos/support_vector_machine.rds')

ctrl_imp <- control_grid(extract = get_rf_imp)

svm_fit_resample <-
  svm_wf |> 
  fit_resamples(data_folds,control = ctrl_imp)

df_met <- collect_metrics(rf_fit_resample) |> 
  select(-.estimator,-.config)

df_var_imp <- svm_fit_resample |> 
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
ggsave(paste0('output/figs/fig_errorbar_resample_random_forest.png'),scale =1.5)
