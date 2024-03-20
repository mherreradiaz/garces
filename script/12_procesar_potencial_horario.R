library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(tibble)

data_potencial <- read_xlsx('data/data_raw/potencial/ciclo_diario/potencial_horario.xlsx')

data_potencial <- data_potencial |>
  mutate(unidad = factor(unidad,levels = 1:3),
         across(bar_8:bar_20,as.numeric),
         fecha = as.Date(fecha)) |>
  pivot_longer(cols = 7:19, names_to = 'hora', values_to = 'bar') |>
  mutate(hora = as.numeric(substr(hora,5,length(hora)))) |>
  select(temporada,sitio,fecha,hora,everything())
   
write_rds(data_potencial, 'data/data_processed/potencial_horario.rds')
