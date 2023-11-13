
library(tidyverse)
library(highcharter)
library(gt)
library(readr)

data_sm <- read_rds('data/data_processed/rds/data_zim_sm.rds')
data_tur <- read_rds('data/data_processed/rds/data_zim_turgor.rds')

data_info <- data_sm |> 
  select(sitio,sensor,tratamiento,codigo,unidad) |>
  distinct(sitio,sensor,tratamiento,codigo,unidad)

data_sm |> 
  mutate(unidad = as.factor(unidad)) |> 
  ggplot(aes(unidad,value)) +
  geom_boxplot() +
  facet_grid(sitio~tratamiento) +
  geom_text(data = data_info,aes(unidad,75,label = codigo),size=2) +
  theme_light()
