library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(tibble)

data_potencial <- list.files('data/data_raw/potencial/ciclo_diario/', full.names = T) |> 
  lapply(FUN =read_delim) |> 
  bind_rows() |>
  mutate(unidad = factor(unidad,levels = 1:3),
         fecha = as.character(fecha),
         hora = substr(as.character(hora),1,5))
   
write_rds(data_potencial, 'data/data_processed/potencial_horario.rds')

## Analisis

data_potencial <- read_rds('data/data_processed/potencial_horario.rds')

data_turgor <- read_rds('data/data_processed/zim_turgor_preprocesado.rds') |> 
  group_by(sitio,temporada,fecha,hora,tratamiento,unidad,codigo) |> 
  summarise(turgor_sc = mean(turgor_sc,na.rm=T))

fecha_sitio <- data_potencial |> 
  distinct(sitio,fecha) |>
  mutate(grupo = paste(sitio,fecha)) |> 
  pull(grupo)

data_turgor <- data_turgor |> 
  filter(paste(sitio,fecha) %in% fecha_sitio,
         between(hora,7,21))

data_turgor_2022 <- data_turgor |> 
  filter(temporada == '2022-2023',
         hora > 7) |> 
  mutate(hora = hora-1)

data_turgor_2022 <- data_turgor |> 
  filter(temporada == '2022-2023',
         hora < 21) |> 
  rbind(data_turgor_2022) |>
  group_by(sitio,temporada,fecha,hora,tratamiento,unidad) |> 
  summarise(turgor_sc = mean(turgor_sc,na.rm=T)) |>
  ungroup() |> 
  mutate(hora = hora+.5)
  
data_cor <- data_turgor |> 
  filter(temporada == '2023-2024',
         between(hora,7,20)) |> 
  rbind(data_turgor_2022) |> 
  arrange(sitio,temporada,fecha,tratamiento,unidad) |> 
  mutate(fecha = as.Date(fecha)) |> 
  left_join(data_potencial, by = c('sitio','temporada','fecha','hora','tratamiento','unidad')) |> 
  group_by(sitio,temporada,fecha,tratamiento,unidad) |> 
  mutate(bar_sc = scale(bar),
         fecha_hora = as.POSIXct(paste(fecha,sprintf("%02d:%02d", trunc(hora),
                                                     (hora - trunc(hora)) * 60)), 
                                 format = "%Y-%m-%d %H:%M")) |> 
  ungroup() |> 
  pivot_longer(cols = c('turgor_sc','bar_sc'), names_to = 'tipo', values_to = 'potencial')

data_cor |> 
  filter(temporada == '2022-2023') |> 
  ggplot(aes(fecha_hora, potencial,color = tipo)) +
  geom_point() +
  geom_line() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free')

data_cor |> 
  filter(temporada == '2023-2024') |> 
  ggplot(aes(fecha_hora, potencial,color = tipo)) +
  geom_point() +
  geom_line() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free')

## Prueba

data_turgor <- read_rds('data/data_processed/zim_turgor.rds') |> 
  group_by(sitio,temporada,fecha,hora,tratamiento,unidad,codigo) |> 
  summarise(turgor = mean(turgor,na.rm=T),
            turgor_sc = mean(turgor_sc,na.rm=T))

data_cor |> 
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |> 
  filter(sitio == 'la_esperanza',
         temporada == '2022-2023',
         tratamiento == 'T4',
         unidad == 1) |> 
  ggplot(aes(fecha_hora,turgor_sc, color = zim)) +
  geom_point() +
  geom_line() +
  geom_line(aes(fecha_hora,t_sc)) +
  scale_x_datetime(limits = c(as.POSIXct('2023-01-10'),as.POSIXct('2023-01-15')))


data_cor |> 
  filter(sitio == 'la_esperanza',
         temporada == '2022-2023',
         fecha == '2023-01-13',
         sensor == 8785) |> 
  ggplot(aes(t_sc,turgor_sc)) +
  geom_point()


