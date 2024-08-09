library(tidyverse)
library(tidymodels)
library(glue)

get_rf_imp <- function(x) {
  x |> 
    extract_fit_parsnip() |> 
    vip::vi(scale = TRUE)
}

## Cargar los datos
# rnd_split or tme_split

splts <- c('rnd_split','tme_split')
models <- c('random_forest','xgboost')

# split 
data <- read_rds('data/processed/modelo_potencial_smooth.rds') |> 
  select(-(tratamiento:codigo),-(sitio:temporada)) |> 
  mutate(potencial_bar = -potencial_bar,
         fecha = ymd(fecha)) |> 
  drop_na() 

d <- map_df(splts,\(split){
  map_df(models,\(model){
    
  #split <- 'tme_split'
  
  if(split == 'rnd_split'){
    splits <- initial_split(data) #aleatorio, sobreajusta
  } else {
    splits <- group_initial_split(data,fecha) #independiente de la fecha
  }
  
  pot_train <- training(splits) 
  pot_test  <- testing(splits) 
  
  # XGBoost
  library(vip)
  
  set.seed(432)
  
  if (split =='rnd_split'){
    vb_folds <- vfold_cv(pot_train,v=5)
  } else vb_folds <- group_vfold_cv(pot_train,fecha,v=5)
  
  
  model_wf <- read_rds(glue('data/processed/modelos/{model}_{split}.rds'))
  
  ctrl_imp <- control_grid(extract = get_rf_imp)
  
  model_fit_resample <-
    model_wf |> 
    fit_resamples(vb_folds,control = ctrl_imp)
  
  # df_met <- collect_metrics(xgb_fit_resample) |> 
  #   select(-.estimator,-.config)
  mod_name <- ifelse(model == 'random_forest','RF','XGBoost')

  df_var_imp <- model_fit_resample |> 
    select(id, .extracts) %>%
    unnest(.extracts) %>%
    unnest(.extracts) %>%
    group_by(Variable) %>%
    summarise(Mean = mean(Importance),
              Variance = sd(Importance)) |> 
    mutate(model = mod_name,
           split = split)
  })
})
  

  
# Support Vector Machine
d2 <- map_df(splts,\(split){
  
  set.seed(12)
  
  if(split == 'rnd_split'){
    splits <- initial_split(data) #aleatorio, sobreajusta
  } else {
    splits <- group_initial_split(data,fecha) #independiente de la fecha
  }
  
  pot_train <- training(splits) 
  pot_test  <- testing(splits) 
  
  svm_wf <- read_rds(glue('data/processed/modelos/support_vector_machine_{split}.rds'))

  pfun <- function(object, newdata) predict(object, new_data = newdata) |> pull(1)
  
  df_var_imp_svm <- 
         svm_wf |> 
    extract_fit_parsnip() |> 
    vip::vi(method = 'permute',nsim = 10,
            target = 'potencial_bar', 
            metric = ifelse(split == 'rnd_split', 'rsq_trad', 'rsq'),
            scale = TRUE,
            pred_wrapper = pfun, train =pot_test) |> 
    mutate(model = 'SVM',
           split = split,
           StDev = sqrt(StDev)) |> 
    rename(Mean = Importance,
           Variance = StDev)
  
  df_var_imp_svm
})

## Visualizar resultados variable importance para los tres modelos
## 
df_vimp <- bind_rows(d,d2)

df_vimp |> 
  group_by(model,split) |> 
  slice_max(Mean, n = 10) |> 
  arrange(desc(Mean)) |> 
  mutate(Mean = scales::rescale(Mean) |> as.numeric(),
         Variable = toupper(Variable),
         Variable = case_when(
           Variable == 'RH_MEDIA' ~ 'RH',
           Variable == 'VPD_MEDIO' ~ 'VPD',
           Variable == 'T_MEDIA' ~ 'Temp',
           Variable == 'ETO' ~ 'ET0',
           Variable == 'CAB' ~ 'Cab',
           .default = Variable),
         ) |> 
  filter(Mean > 0.05) |> 
  ggplot(aes(reorder(Variable,Mean),Mean,fill=model)) +
  geom_col(position = position_dodge()) +
  scale_fill_viridis_d('Model',option = 'E',alpha=.7) +
  scale_y_continuous(expand = c(0,0.0),limits = c(0,1.05)) +
  labs(y = 'Importance') +
  coord_flip() +
  facet_grid(.~split) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = 'white'))
ggsave('output/figs/fig_variable_importance_by_model.png',width=10,height =5,scale = 1)
  
  
