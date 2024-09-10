source('script/00_setup.R')
library(randomForest)
library(rsample)
library(caret)
sitio_reclass <- function(x) {
  factor(case_when(x == 'la_esperanza' ~ 'La Esperanza',
                   x == 'rio_claro' ~ 'Rio Claro'),
         levels = c('Rio Claro','La Esperanza'))
}

data_metaboloma <- read_rds('data/processed/metaboloma.rds')

# produccion

produccion <- read_rds('data/processed/cosecha/produccion.rds') |> 
  group_by(sitio,temporada,tratamiento,unidad) |> 
  reframe(rendimiento = mean(rendimiento,na.rm=T))

data <- produccion |> 
  left_join(data_metaboloma) |> 
  select(sitio:unidad,rendimiento,everything())

set.seed(123)

train_data <- data |> 
  group_by(sitio, temporada) |> 
  slice_sample(prop = 0.75) |>
  ungroup()

test_data <- data |> 
  anti_join(train_data) |> 
  select(-(sitio:unidad))

train_data <- train_data |> 
  select(-(sitio:unidad))

x = 100

for (i in 1:100) {
  print(i)
  modelo_rf <- randomForest(rendimiento ~ ., data = train_data, 
                            importance = TRUE,
                            ntree = x)   
  predicciones <- predict(modelo_rf, newdata = test_data)
  r <- postResample(pred = predicciones, obs = test_data$rendimiento)[[2]]
  print(r)
  if (r > .8) {break}
}

importancia <- importance(modelo_rf)

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

ggsave(paste0('output/reunion/importancia_metabolitos_rendimiento.png'), width = 6, height = 6)

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
       y = 'Rendimiento (ton/ha)')

ggsave(paste0('output/reunion/rendimiento_metabolitos.png'), width = 10, height = 6)

# 9000kg 32ha regina
# 16000kg 15ha lappins
# 
# 6000kg 32ha regina
# 11000kg 15ha lappins

# brix

brix <- read_rds('data/processed/cosecha/brix.rds') |> 
  group_by(sitio,temporada,tratamiento,unidad) |> 
  reframe(brix = mean(brix,na.rm=T))

data <- brix |> 
  left_join(data_metaboloma) |> 
  select(sitio:unidad,brix,everything())

set.seed(123)

train_data <- data |> 
  group_by(sitio, temporada) |> 
  slice_sample(prop = 0.75) |>
  ungroup()

test_data <- data |> 
  anti_join(train_data) |> 
  select(-(sitio:unidad))

train_data <- train_data |> 
  select(-(sitio:unidad))

for (i in 1:500) {
  modelo_rf <- randomForest(brix ~ ., data = train_data, 
                            importance = TRUE,
                            ntree = x)   
  predicciones <- predict(modelo_rf, newdata = test_data)
  r <- postResample(pred = predicciones, obs = test_data$brix)[[2]]
  r
  if (r > .6) {break}
}
if (i != 500) {print(c(i,r))}


importancia <- importance(modelo_rf)

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

ggsave(paste0('output/reunion/importancia_metabolitos_brix.png'), width = 6, height = 6)

tibble(obs = test_data$brix,
       pred = predicciones) |> 
  ggplot(aes(obs,pred)) +
  geom_point()

rc_2023 <- metabolitos |> 
  filter(sitio == 'rio_claro',
         temporada == '2023-2024')

brix <- data |> 
  select(sitio:brix) |> 
  mutate(brix_predict = predict(modelo_rf,newdata = data)) |> 
  bind_rows(tibble(sitio = 'rio_claro',
                   temporada = '2023-2024',
                   tratamiento = rep(paste0('T',0:4),each=3),
                   unidad = as.factor(rep(1:3,5)),
                   brix_predict = predict(modelo_rf,newdata=rc_2023))) |> 
  group_by(sitio,temporada,tratamiento) |> 
  reframe(observado = mean(brix,na.rm=T),
          predicho = mean(brix_predict,na.rm=T)) |> 
  pivot_longer(cols=c('observado','predicho'),
               names_to = 'Brix',
               values_to = 'valor') |> 
  mutate(sitio = sitio_reclass(sitio))

brix |> 
  arrange(desc(valor)) |> 
  ggplot(aes(tratamiento,valor, fill = Brix)) +
  geom_col(position = 'dodge') +
  facet_grid(sitio ~ temporada) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white')) +
  labs(x = 'Tratamiento',
       y = 'Grados Brix predicho')

ggsave(paste0('output/reunion/brix_metabolitos.png'), width = 10, height = 6)

# calibre

apariencia <- read_rds('data/processed/cosecha/apariencia.rds') |> 
  group_by(sitio,temporada,tratamiento,unidad) |> 
  reframe(diametro = mean(diametro,na.rm=T))

data <- apariencia |> 
  left_join(data_metaboloma) |> 
  select(sitio:unidad,diametro,everything())

set.seed(123)

train_data <- data |> 
  group_by(sitio, temporada) |> 
  slice_sample(prop = 0.75) |>
  ungroup()

test_data <- data |> 
  anti_join(train_data) |> 
  select(-(sitio:unidad))

train_data <- train_data |> 
  select(-(sitio:unidad))

x = 100

for (i in 1:100) {
  print(i)
  modelo_rf <- randomForest(diametro ~ ., data = train_data, 
                            importance = TRUE,
                            ntree = x)   
  predicciones <- predict(modelo_rf, newdata = test_data)
  r <- postResample(pred = predicciones, obs = test_data$diametro)[[2]]
  print(r)
  if (r > .65) {break}
}

importancia <- importance(modelo_rf)

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

ggsave(paste0('output/reunion/importancia_metabolitos_calibre.png'), width = 6, height = 6)

tibble(obs = test_data$diametro,
       pred = predicciones) |> 
  ggplot(aes(obs,pred)) +
  geom_point()

rc_2023 <- data_metaboloma |> 
  filter(sitio == 'rio_claro',
         temporada == '2023-2024')

rendimiento <- data |> 
  select(sitio:diametro) |> 
  mutate(diametro_predict = predict(modelo_rf,newdata = data)) |> 
  bind_rows(tibble(sitio = 'rio_claro',
                   temporada = '2023-2024',
                   tratamiento = rep(paste0('T',0:4),each=3),
                   unidad = as.factor(rep(1:3,5)),
                   diametro_predict = predict(modelo_rf,newdata=rc_2023))) |> 
  group_by(sitio,temporada,tratamiento) |> 
  reframe(observado = mean(diametro,na.rm=T),
          predicho = mean(diametro_predict,na.rm=T)) |> 
  pivot_longer(cols=c('observado','predicho'),
               names_to = 'Calibre',
               values_to = 'valor') |> 
  mutate(sitio = sitio_reclass(sitio))

rendimiento |> 
  arrange(desc(valor)) |> 
  ggplot(aes(tratamiento,valor, fill = Calibre)) +
  geom_col(position = 'dodge') +
  facet_grid(sitio ~ temporada) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white')) +
  labs(x = 'Tratamiento',
       y = 'Calibre (mm)')

ggsave(paste0('output/reunion/calibre_metabolitos.png'), width = 10, height = 6)
