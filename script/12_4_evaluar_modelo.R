library(tidyverse)
library(tidymodels)

get_rf_imp <- function(x) {
  x |> 
    extract_fit_parsnip() |> 
    vip::vi(scale = TRUE)
}

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

# XGBoost
library(vip)

set.seed(432)
vb_folds <- group_vfold_cv(pot_train,fecha,v=5)

xgb_wf <- read_rds('data/processed/modelos/xgboost.rds')

ctrl_imp <- control_grid(extract = get_rf_imp)

xgb_fit_resample <-
  xgb_wf |> 
  fit_resamples(vb_folds,control = ctrl_imp)

df_met <- collect_metrics(xgb_fit_resample) |> 
  select(-.estimator,-.config)

df_var_imp_xgb <- xgb_fit_resample |> 
  select(id, .extracts) %>%
  unnest(.extracts) %>%
  unnest(.extracts) %>%
  group_by(Variable) %>%
  summarise(Mean = mean(Importance),
            Variance = sd(Importance)) |> 
  mutate(model = 'XGBoost')

df_var_imp_xgb |>   
  slice_max(Mean, n = 15) %>%
  ggplot(aes(Mean, reorder(Variable, Mean))) +
  geom_errorbar(aes(xmin = Mean - Variance, xmax = Mean + Variance)) +
  geom_point(aes(Mean)) +
  labs(x = "Variable importance", y = NULL) +
  annotate("text",x=max(df_var_imp_xgb$Mean),y=1,label =paste0('r2=',round(df_met$mean[2],2))) +
  theme_bw()
ggsave(paste0('output/figs/fig_errorbar_resample_xgboost.png'),scale =1.5)

# Random Forest

rf_wf <- read_rds('data/processed/modelos/random_forest.rds')

ctrl_imp <- control_grid(extract = get_rf_imp)

rf_fit_resample <-
  rf_wf |> 
  fit_resamples(vb_folds,control = ctrl_imp)

df_met <- collect_metrics(rf_fit_resample) |> 
  select(-.estimator,-.config)

df_var_imp_rf <- rf_fit_resample |> 
  select(id, .extracts) %>%
  unnest(.extracts) %>%
  unnest(.extracts) %>%
  group_by(Variable) %>%
  summarise(Mean = mean(Importance),
            Variance = sd(Importance)) |> 
  mutate(model = 'RF')

df_var_imp_rf |>   
  slice_max(Mean, n = 15) %>%
  ggplot(aes(Mean, reorder(Variable, Mean))) +
  geom_errorbar(aes(xmin = Mean - Variance, xmax = Mean + Variance)) +
  geom_point(aes(Mean)) +
  labs(x = "Variable importance", y = NULL) +
  annotate("text",x=max(df_var_imp_rf$Mean),y=1,label =paste0('r2=',round(df_met$mean[2],2))) +
  theme_bw()
ggsave(paste0('output/figs/fig_errorbar_resample_random_forest.png'),scale =1.5)

# Support Vector Machine

svm_wf <- read_rds('data/processed/modelos/support_vector_machine.rds')

potencial_rec <- recipe(potencial_bar~.,data = pot_train)

potencial_prep <- prep(potencial_rec)

juice(potencial_prep)

pfun <- function(object, newdata) predict(object, new_data = newdata) |> pull(.pred)

svm_wf |> 
  extract_fit_parsnip() |> 
  vip::vip(method = 'permute',nsim = 10,
           target = 'potencial_bar', metric = 'rsq',
           all_permutations = TRUE,
           geom = 'point',
           pred_wrapper = pfun, 
           train = pot_train |> select(-fecha)
  )
           

df_var_imp_svm <- 
       svm_wf |> 
  extract_fit_parsnip() |> 
  vip::vi(method = 'permute',nsim = 10,
          target = 'potencial_bar', metric = 'rsq',
          scale = TRUE,
          pred_wrapper = pfun, train =pot_train |> select(-fecha) ) |> 
  mutate(model = 'SVM',
         StDev = sqrt(StDev)) |> 
  rename(Mean = Importance,
         Variance = StDev)
  

## Visualizar resultados variable importance para los tres modelos
## 
df_vimp <- bind_rows(df_var_imp_xgb,df_var_imp_rf,df_var_imp_svm)

df_vimp |> 
  group_by(model) |> 
  slice_max(Mean, n = 10) |> 
  arrange(desc(Mean)) |> 
  mutate(Mean = scales::rescale(Mean) |> as.numeric(),
         Variable = toupper(Variable),
         Variable = case_when(
           Variable == 'RH_MEDIA' ~ 'RH',
           Variable == 'VPD_MEDIO' ~ 'VPD',
           Variable == 'T_MEDIA' ~ 'Temp',
           Variable == 'ETO' ~ 'ET0',
           Variable == 'CAB' ~ 'LAI_cab',
           .default = Variable),
         ) |> 
  filter(Mean > 0.05) |> 
  ggplot(aes(reorder(Variable,-Mean),Mean,fill=model)) +
  geom_col(position = position_dodge()) +
  scale_fill_viridis_d('Model') +
  scale_y_continuous(expand = c(0,0.0),limits = c(0,1.05)) +
  labs(y = 'Importance') +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank())
ggsave('output/figs/fig_variable_importance_by_model.png',width=9,height =5,scale = 1)
  
  
### borrar de aquí abajo ?

ctrl_imp <- control_grid(extract = get_rf_imp)

svm_fit_resample <-
  svm_wf |> 
  fit_resamples(data_folds,control = ctrl_imp)

df_met <- collect_metrics(svm_fit_resample) |> 
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
# ggsave(paste0('output/figs/fig_errorbar_resample_random_forest.png'),scale =1.5)
# 