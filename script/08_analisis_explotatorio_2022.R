
library(tidyverse)
library(highcharter)
library(gt)
library(readr)

data_fluo <- read_rds('data/data_processed_old/rds/data_fluorescencia.rds') |>
  mutate(codigo = case_when(grepl("^T0", codigo) & sitio == "la_esperanza" ~ gsub("T0", "T0H", codigo),
      TRUE ~ codigo))
data_potencial <- read_csv('data/data_processed_old/csv/data_potencial.csv')
data_cepto <- read.csv('data/data_processed_old/csv/data_ceptometro.csv', sep = ';') |>
  mutate(tratamiento = treatment,
         sitio = field,
         fecha = as.Date(data,"%d %m %Y"))
data_sm <- read_rds('data/data_processed_old/rds/data_zim_sm.rds')
data_turgor <- read_rds('data/data_processed_old/rds/data_zim_turgor.rds')

codigos <- read_csv2('data/data_processed_old/csv/codigos.csv')

data_info <- codigos |>
  select(codigo,unidad)

# FLUORESCENCIA ####

(data_fluo |> # BOXPLOT 
   left_join(data_info, by = 'codigo') |>
   mutate(unidad = as.factor(unidad)) |> 
   separate(codigo,2,into =c('tratamiento','codigo')) |>
   ggplot(aes(unidad,Fv/Fm)) +
   geom_boxplot() +
   facet_grid(sitio~tratamiento) +
   geom_text(aes(unidad,1,label = codigo),size=2) +
   theme_light()) %>%
  ggsave('reporte/png_2022/fluorescencia_boxplot.png', ., width = 7, height = 5, units = "in")

(data_fluo |> # SERIE DE TIEMPO
    left_join(data_info, by = 'codigo') |>
    mutate(unidad = as.factor(unidad)) |> 
    separate(codigo,2,into =c('tratamiento','codigo')) |>
    ggplot(aes(fecha,Fv/Fm,color=unidad)) +
    geom_point(size=1) +
    geom_line(linewidth = .5) +
    facet_grid(tratamiento~sitio) +
    theme_bw())  %>%
  ggsave('reporte/png_2022/fluorescencia_serie.png', ., width = 9, height = 5, units = "in")

# POTENCIAL ####

(data_potencial |> # BOXPLOT 
   mutate(unidad = as.factor(unidad)) |> 
   mutate(MPa = -potencial_bar/10) |>
   separate(codigo,2,into =c('tratamiento','codigo')) |>
   ggplot(aes(unidad,MPa)) +
   geom_boxplot() +
   facet_grid(sitio~tratamiento) +
   geom_text(aes(unidad,-.4,label = codigo),size=2) +
   theme_light())  %>%
  ggsave('reporte/png_2022/potencial_boxplot.png', ., width = 7, height = 5, units = "in")

(data_potencial |> # SERIE DE TIEMPO
    mutate(unidad = as.factor(unidad)) |> 
    mutate(MPa = -potencial_bar/10) |>
    separate(codigo,2,into =c('tratamiento','codigo')) |>
    ggplot(aes(fecha,MPa,color=unidad)) +
    geom_point(size=1) +
    geom_line(linewidth = .5) +
    facet_grid(tratamiento~sitio) +
    theme_bw()) %>%
  ggsave('reporte/png_2022/potencial_serie.png', ., width = 9, height = 5, units = "in")

# PAR ####

(data_cepto |> # BOXPLOT
   mutate(tratamiento = as.factor(tratamiento)) |>
   ggplot(aes(tratamiento,above_par)) +
   geom_boxplot() +
   facet_grid(~sitio) +
   theme_light()) %>%
  ggsave('reporte/png_2022/par_boxplot.png', ., width = 7, height = 5, units = "in")

(data_cepto |> # SERIE DE TIEMPO
    mutate(tratamiento = as.factor(tratamiento)) |>
    ggplot(aes(fecha,above_par,color=tratamiento)) +
    geom_point(size=1) +
    geom_line(linewidth = .5) +
    facet_grid(~sitio) +
    theme_bw()) %>%
  ggsave('reporte/png_2022/par_serie.png', ., width = 9, height = 5, units = "in")

# ZIM HUMEDAD DEL SUELO ####

(data_sm |> # BOXPLOT
   mutate(unidad = factor(unidad, levels = 1:3)) |> 
   ggplot(aes(unidad,value)) +
   geom_boxplot() +
   labs(y = 'VWC (%)') +
   facet_grid(sitio~tratamiento) +
   geom_text(aes(unidad,100,label = codigo),size=2) +
   theme_light()) %>%
  ggsave('reporte/png_2022/zim_sm_boxplot.png', ., width = 7, height = 5, units = "in")

(data_sm |> # SERIE DE TIEMPO
    mutate(unidad = factor(unidad, levels = 1:3)) |>
    ggplot(aes(hora,value,color=unidad)) +
    geom_point(size = .2) +
    labs(y = 'VWC (%)') +
    facet_grid(tratamiento~sitio) +
    theme_bw() +
    guides(color = guide_legend(override.aes = list(size = 2)))) %>%
  ggsave('reporte/png_2022/zim_sm_serie.png', ., width = 9, height = 5, units = "in")

# ZIM PARCHE DE TURGOR ####

(data_turgor |> # BOXPLOT
   mutate(unidad = factor(unidad, levels = 1:3),
          codigo_arbol = substr(codigo,1,str_length(codigo)-2)) |>
   ggplot(aes(unidad,value)) +
   geom_boxplot() +
   labs(y = 'Presión de parche') +
   facet_grid(sitio~tratamiento) +
   geom_text(aes(unidad,350,label = codigo_arbol),size=2) +
   theme_light()) %>%
  ggsave('reporte/png_2022/zim_turgor_boxplot.png', ., width = 7, height = 5, units = "in")

(data_turgor |> # SERIE DE TIEMPO ESTANDARIZADA
    drop_na() |> 
    separate(codigo,into = c('codigo','zim'),sep="Z") |> 
    mutate(unidad = factor(unidad, levels = 1:3)) |> 
    group_by(sitio,codigo,zim) |> 
    mutate(value_sc = scale(value)) |> 
    ggplot(aes(hora,value_sc,color=zim)) +
    geom_point(size=.05) +
    labs(y = 'Presión de parche') +
    facet_grid(tratamiento+sitio~unidad) +
    theme_bw() +
    guides(color = guide_legend(override.aes = list(size = 2)))) %>%
  ggsave('reporte/png_2022/zim_turgor_serie_std.png', ., width = 9, height = 5, units = "in")

(data_turgor |> # SERIE DE TIEMPO
    drop_na() |> 
    separate(codigo,into = c('codigo','zim'),sep="Z") |> 
    mutate(unidad = factor(unidad, levels = 1:3)) |> 
    group_by(sitio,codigo,zim) |> 
    ggplot(aes(hora,value,color=zim)) +
    geom_point(size=.05) +
    labs(y = 'Presión de parche') +
    facet_grid(tratamiento+sitio~unidad) +
    theme_bw() +
    guides(color = guide_legend(override.aes = list(size = 2)))) %>%
  ggsave('reporte/png_2022/zim_turgor_serie.png', ., width = 9, height = 5, units = "in")

(data_turgor |> 
    drop_na() |> 
    mutate(unidad = as.factor(unidad)) |> 
    group_by(sitio,codigo) |> 
    mutate(value_sc = scale(value)) |> 
    drop_na() |> 
    arrange(sitio,tratamiento,unidad) |> 
    mutate(unidad = as.factor(unidad),
           hora_dia = as.numeric(format(hora,"%H"))) |> 
    group_by(sitio,tratamiento,unidad,hora_dia) |> 
    summarize(value_hora = mean(value_sc,na.rm = TRUE)) |> 
    ggplot(aes(hora_dia,value_hora,color=unidad)) +
    geom_point(size=.05) +
    geom_line() +
    #geom_text(data = data_info,aes(unidad,75,label = codigo),size=2) +
    facet_grid(tratamiento+unidad~sitio,scales = 'free_y') +
    theme_bw()) %>%
  ggsave('reporte/png_2022/zim_turgor_ciclo.png', ., width = 9, height = 5, units = "in")
