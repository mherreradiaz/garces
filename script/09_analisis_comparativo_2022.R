
library(tidyverse)
library(highcharter)
library(gt)
library(readr)
library(dunn.test)
library(lsmeans)
library(rcompanion)

cld <- function(data, sitios, variable, zim = F) {
  cld_c <- c()
  for (i in 1:length(sitios)) { 
    data_sitio <- data %>%
      filter(sitio == sitios[i])
    dunn_result <- dunn.test(data_sitio[[variable]], data_sitio$tratamiento, method = "bonferroni")
    cld_c <- append(cld_c, cldList(comparison = dunn_result$comparisons, p.value = dunn_result$P.adjusted, threshold = .05/2)$Letter)
  } 
  
  if (zim == F) {
    return(data.frame(sitio = rep(sitios, each = 5), 
                      tratamiento = rep(paste0('T', 0:4), 2), 
                      cld = cld_c))
  } else {
    return(data.frame(sitio = rep(sitios, each = 4), 
                      tratamiento = rep(paste0('T', 1:4), 2), 
                      cld = cld_c))
  }
}

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

sitio_name <- c('la_esperanza', 'rio_claro')

# FLUORESCENCIA ####

data_cld <- data_fluo |> 
  separate(codigo,2,into =c('tratamiento','codigo')) |> 
  cld(sitio_name,'Fv/Fm')

(data_fluo |>
    separate(codigo,2,into =c('tratamiento','codigo')) |> 
    ggplot(aes(tratamiento,Fv/Fm)) +
    geom_boxplot() +
    facet_grid(~sitio) +
    geom_text(data = data_cld, aes(tratamiento,1,label = cld),size=3) +
    theme_light()) %>%
  ggsave('reporte/png_2022/fluorescencia_dunn.png', ., width = 7, height = 5, units = "in")

# PAR ####

data_cld <- data_cepto |> 
  cld(sitio_name,'above_par')

(data_cepto |>
    ggplot(aes(tratamiento,above_par)) +
    geom_boxplot() +
    facet_grid(~sitio) +
    geom_text(data = data_cld, aes(tratamiento,2300,label = cld),size=3) +
    theme_light()) %>%
  ggsave('reporte/png_2022/par_dunn.png', ., width = 7, height = 5, units = "in")

# POTENCIAL ####

data_cld <- data_potencial |> 
  separate(codigo,2,into =c('tratamiento','codigo')) |>
  mutate(MPa = -potencial_bar/10) |>
  cld(sitio_name,'MPa')

(data_potencial |> 
    separate(codigo,2,into =c('tratamiento','codigo')) |>
    mutate(MPa = -potencial_bar/10) |>
    ggplot(aes(tratamiento,MPa)) +
    geom_boxplot() +
    facet_grid(~sitio) +
    geom_text(data = data_cld, aes(tratamiento,0,label = cld),size=3) +
    theme_light()) %>%
  ggsave('reporte/png_2022/potencial_dunn.png', ., width = 7, height = 5, units = "in")

# HUMEDAD DEL SUELO ####

data_cld <- data_sm |>
  cld(sitio_name,'value', zim = T)

(data_sm |>
    ggplot(aes(tratamiento,value)) +
    geom_boxplot() +
    labs(y = 'VWC (%)') +
    facet_grid(~sitio) +
    geom_text(data = data_cld, aes(tratamiento,80,label = cld),size=3) +
    theme_light()) %>%
  ggsave('reporte/png_2022/zim_sm_dunn.png', ., width = 7, height = 5, units = "in")

# PRESIÓN DE PARCHE ####

data_cld <- data_turgor |>
  cld(sitio_name,'value', zim = T)

(data_turgor |>
    ggplot(aes(tratamiento,value)) +
    labs(y = 'Presión de parche') +
    geom_boxplot() +
    facet_grid(~sitio) +
    geom_text(data = data_cld, aes(tratamiento,350,label = cld),size=3) +
    theme_light()) %>%
  ggsave('reporte/png_2022/zim_turgor_dunn.png', ., width = 7, height = 5, units = "in")

data_cld <- data_turgor |> # ESTANDARIZADO
  mutate(value_sc = scale(value)) |> 
  cld(sitio_name,'value_sc', zim = T)

(data_turgor |>
    mutate(value_sc = scale(value)) |> 
    ggplot(aes(tratamiento,value_sc)) +
    labs(y = 'Presión de parche') +
    geom_boxplot() +
    facet_grid(~sitio) +
    geom_text(data = data_cld, aes(tratamiento,7,label = cld),size=3) +
    theme_light()) %>%
  ggsave('reporte/png_2022/zim_turgor_dunn_std.png', ., width = 7, height = 5, units = "in")

