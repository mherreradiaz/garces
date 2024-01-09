
library(ggplot2)
library(tidyverse)
library(highcharter)
library(gt)
library(readr)
library(anomalize)
library(tibbletime)

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

data_limpia <- data_turgor |>
  filter(!is.na(turgor),
         sensor == 8569) |>
  mutate(turgor_norm = scale(turgor),
         fecha_hora = as.POSIXct(paste(fecha, hora), format="%Y-%m-%d %H", tz="UTC")) |>
  arrange(fecha_hora) |>
  as_tbl_time(index = fecha_hora) |>
  select(fecha_hora, turgor_norm) |>
  time_decompose(turgor_norm, method = "twitter")
  mutate(iqr = IQR(remainder),
         lower_bound = quantile(remainder, 0.25) - umbral_iqr * iqr,
         upper_bound = quantile(remainder, 0.75) + umbral_iqr * iqr,
         anomaly = ifelse(remainder < lower_bound | remainder > upper_bound, 1, 0)) |>
  filter(anomaly == 0) |>
  ungroup() |>
  select(-c(trend, season, remainder, iqr, lower_bound, upper_bound, anomaly))

data_turgor |> 
  group_by(sensor) |>
  drop_na() |> 
  mutate(turgor_sc = scale(turgor)) |> 
  group_by(sitio,tratamiento,unidad,hora) |> 
  summarize(turgor_hora = mean(turgor_sc,na.rm = TRUE)) |> 
  ggplot(aes(hora,turgor_hora,color=unidad)) +
  geom_point(size=.05) +
  geom_line() +
  labs(y = 'Presión de parche estandarizada') +
  facet_grid(tratamiento+unidad~sitio,scales = 'free_y') +
  theme_bw()
