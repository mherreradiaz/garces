
library(ggplot2)
library(tidyverse)
library(highcharter)
library(gt)
library(readr)
library(anomalize)

data_turgor <- 
  read_rds('C:/HEMERA/ID2023/id23_10297/data/data_processed/zim_turgor.rds') |>
  filter(temporada == '2023-2024')

data_turgor |>
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M")) |>
  arrange(sensor,fecha_hora) |>
  drop_na() |> 
  group_by(sitio,codigo,zim) |> 
  ggplot(aes(fecha_hora,turgor,color=zim)) +
  geom_point(size=.05) +
  labs(y = 'Presión de parche') +
  facet_grid(tratamiento+sitio~unidad) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2)))

datos_limpio <- data_turgor %>%
  group_by(sensor) |>
  group_by(sitio, codigo, zim) %>%
  mutate(turgor_norm = scale(turgor)) %>%
  time_decompose(turgor_norm, method = "stl") %>%
  anomalize(remainder = "iqr", alpha = 0.05) %>%
  filter(anomaly == 0) %>%
  ungroup() %>%
  select(-c(trend, season, remainder, is_anomaly))
