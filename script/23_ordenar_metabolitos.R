source('script/00_setup.R')

int <- function(...) {
  Reduce(intersect, list(...))
}

rc_2022 <- read_xlsx('data/raw/metaboloma/metabolitos.xlsx',sheet = 3)
le_2022 <- read_xlsx('data/raw/metaboloma/metabolitos.xlsx',sheet = 2)
meta_2023 <- read_xlsx('data/raw/metaboloma/metabolitos.xlsx',sheet = 1)

var <- Reduce(intersect,list(names(rc_2022),names(le_2022),names(meta_2023)))

metabolitos <- meta_2023 |> select(all_of(var)) |> 
  bind_rows(rc_2022 |> select(all_of(var))) |> 
  bind_rows(le_2022 |> select(all_of(var))) |> 
  arrange(sitio,temporada,tratamiento,unidad) |> 
  mutate(unidad = as.factor(unidad))

produccion <- read_rds('data/processed/cosecha/produccion.rds') |> 
  select(sitio:codigo,rendimiento)

data <- produccion |> 
  left_join(metabolitos) |> 
  select(sitio:codigo,rendimiento,everything(),-fecha)

#

library(randomForest)
library(rsample)
library(caret)
sitio_reclass <- function(x) {
  factor(case_when(x == 'la_esperanza' ~ 'La Esperanza',
                   x == 'rio_claro' ~ 'Rio Claro'),
         levels = c('Rio Claro','La Esperanza'))
}

set.seed(123)

train_data <- data |> 
  group_by(sitio, temporada) |> 
  slice_sample(prop = 0.75) |>
  ungroup()

test_data <- data |> 
  anti_join(train_data) |> 
  select(-(sitio:codigo))

train_data <- train_data |> 
select(-(sitio:codigo))

modelo_rf <- randomForest(rendimiento ~ ., data = train_data, 
                          importance = TRUE,
                          ntree = x)   
# print(modelo_rf)
importancia <- importance(modelo_rf)
varImpPlot(modelo_rf)

df <- as.data.frame(importancia)
df$variable <- rownames(df)
df <- df %>% select(variable, everything())
tibble_df <- as_tibble(df)

tibble_df |> 
  arrange(`%IncMSE`) |> 
  ggplot(aes(`%IncMSE`,factor(variable,levels=variable))) +
  geom_point(shape = 21,size = 3,stroke=.7) +
  theme_bw() +
  labs(y = 'metabolitos') +
  theme(panel.grid.major.y = element_line(linetype = "dashed"),
        panel.grid.minor.y = element_blank())

ggsave(paste0('output/reunion/importancia_metabolitos.png'), width = 6, height = 6)

predicciones <- predict(modelo_rf, newdata = test_data)
postResample(pred = predicciones, obs = test_data$rendimiento)

tibble(obs = test_data$rendimiento,
       pred = predicciones) |> 
  ggplot(aes(obs,pred)) +
  geom_point()

rc_2023 <- metabolitos |> 
  filter(sitio == 'rio_claro',
         temporada == '2023-2024')

rendimiento <- data |> 
  select(sitio:rendimiento) |> 
  mutate(rendimiento_predict = predict(modelo_rf,newdata = data)) |> 
  bind_rows(tibble(sitio = 'rio_claro',
                   temporada = '2023-2024',
                   tratamiento = rep(paste0('T',0:4),each=3),
                   unidad = as.factor(rep(1:3,5)),
                   rendimiento_predict = predict(modelo_rf,newdata=rc_2023))) |> 
  select(-codigo) |> 
  group_by(sitio,temporada,tratamiento) |> 
  reframe(observado = mean(rendimiento,na.rm=T),
          predicho = mean(rendimiento_predict,na.rm=T)) |> 
  pivot_longer(cols=c('observado','predicho'),
               names_to = 'Rendimiento',
               values_to = 'valor') |> 
  mutate(sitio = sitio_reclass(sitio),
         valor = valor/1000)

rendimiento |> 
  arrange(desc(valor)) |> 
  ggplot(aes(tratamiento,valor, fill = Rendimiento)) +
  geom_col(position = 'dodge') +
  facet_grid(sitio ~ temporada) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white')) +
  labs(x = 'Tratamiento',
       y = 'Rendimiento predicho (ton/ha)')

ggsave(paste0('output/reunion/rendimiento_metabolitos.png'), width = 10, height = 6)
  
9000kg 32ha regina
16000kg 15ha lappins

6000kg 32ha regina
11000kg 15ha lappins
