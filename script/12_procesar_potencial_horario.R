library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(tibble)

data_potencial <- list.files('data/data_raw/potencial/ciclo_diario/', full.names = T) |> 
  lapply(FUN =read_delim) |> 
  bind_rows() |>
  mutate(unidad = factor(unidad,levels = 1:3),
         fecha = as.Date(fecha))
   
write_rds(data_potencial, 'data/data_processed/potencial_horario.rds')

## Analisis

data_potencial <- read_rds('data/data_processed/potencial_horario.rds')
data_turgor <- read_rds('data/data_processed/zim_turgor_prepro.rds')

fecha_sitio <- data_potencial |> 
  distinct(sitio,fecha) |>
  mutate(grupo = paste(sitio,fecha)) |> 
  pull(grupo)

data_turgor <- data_turgor |> 
  filter(paste(sitio,fecha) %in% fecha_sitio,
         between(hora,7,21)) |>
  select(-turgor,-cluster)

data_turgor_2022 <- data_turgor |> 
  filter(temporada == '2022-2023',
         hora > 7) |> 
  mutate(hora = hora-1)

data_turgor_2022 <- data_turgor |> 
  filter(temporada == '2022-2023',
         hora < 21) |> 
  rbind(data_turgor_2022) |>
  group_by(sitio,temporada,fecha,hora,tratamiento,unidad,codigo,sensor,zim) |> 
  summarise(turgor_sc = mean(turgor_sc,na.rm=T)) |>
  mutate(hora = hora+.5)
  
data_cor <- data_turgor |> 
  filter(temporada == '2023-2024',
         between(hora,7,20)) |> 
  rbind(data_turgor_2022) |> 
  arrange(sitio,temporada,fecha,tratamiento,unidad,zim) |> 
  mutate(fecha = as.Date(fecha)) |> 
  left_join(data_potencial, by = c('sitio','temporada','fecha','hora','tratamiento','unidad','codigo'))


  