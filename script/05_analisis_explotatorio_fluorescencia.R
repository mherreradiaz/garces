
library(tidyverse)
library(highcharter)
library(gt)
library(readr)

data_fluo <- read_rds('data/data_processed/rds/data_fluorescencia.rds')
codigos_fluo <- read.csv('data/metadata/analisis_fluor.csv', sep = ';')

data_info <- codigos_fluo |>
  select(codigo,arbol)

data_fluo |> 
  mutate(arbol = as.factor(arbol)) |> 
  separate(codigo,2,into =c('tratamiento','codigo')) |>
  ggplot(aes(unidad,value)) +
  geom_boxplot() +
  facet_grid(sitio~tratamiento) +
  geom_text(data = data_info,aes(unidad,75,label = codigo),size=2) +
  theme_light()
