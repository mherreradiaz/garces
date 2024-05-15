library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
library(dbscan)
library(purrr)
library(grid)
library(cowplot)
library(ggpubr)
library(scales)
library(highcharter)
library(gt)
library(dunn.test)
library(lsmeans)
library(rcompanion)
library(readxl)
library(agvAPI)
library(Metrics)
library(rstac)
library(gdalcubes)
library(sf)
library(earthdatalogin)
library(glue)
library(terra)
options(timeout = max(3600, getOption("timeout")))
names <- c(la_esperanza = 'La Esperanza',rio_claro = 'Rio Claro',
           T1 = 'T1',T2 = 'T2',T3 = 'T3',T4 = 'T4',T0 = 'T0',
           '1' = '1','2' = '2','3' = '3', 
           '2022-2023' = ' 2022-2023', '2023-2024' = '2023-2024')
cld <- function(variable, grupo) {
  
  dunn_result <- dunn.test(variable, grupo, method = "bonferroni")
  
  if (length(unique(grupo)) > 2) {
  cld_c <- cldList(comparison = dunn_result$comparisons, 
                   p.value = dunn_result$P.adjusted, 
                   threshold = .05/2) |>
    rename(grupo = Group,
           cld = Letter) |>
    select(-MonoLetter) |>
    mutate(grupo = ifelse(grupo == 'T','T0',grupo))
  
   } else {
    
    dunn_result <- dunn.test(variable, grupo, method = "bonferroni")
    
    if (dunn_result$P.adjusted <= 0.05/2) {cld_l = c('a','b')} else {cld_l = c('a','a')}
    
    cld_c <- tibble(grupo = unique(grupo), cld = cld_l)
    
   }
  
  return(cld_c)
}
si <- function(condition, true_value, false_value) {
  if (condition) {
    return(true_value)
  } else {
    return(false_value)
  }
}
fecha_f <- function(datetime) {
  return(format(datetime,'%Y-%m-%d'))
}
hora_f <- function(datetime) {
  return(format(datetime,'%H:%M'))
}
fecha_hora_f <- function(fecha,hora) {
  return(as.POSIXct(paste(fecha,hora),format='%Y-%m-%d %H:%M'))
}
dia <- function(año, mes) {
  
  dia <- as.Date(paste(año, mes, "01", sep = "-"))
  
  primer_dia <- dia - days(1)
  ultimo_dia <- dia + months(1)
  
  return(c(primer_dia, ultimo_dia))
}
modal <- function(x) {
  # Calcular la tabla de frecuencias
  tabla_frecuencia <- table(x)
  
  # Encontrar el valor con la frecuencia máxima (la moda)
  moda <- as.numeric(names(tabla_frecuencia)[which.max(tabla_frecuencia)])
  
  return(moda)
}
clima <- function(id_estacion, var, periodo) {
  
  for (x in 1:length(var)) {
    var_x <- getDataAGV_clima(station_id =id_estacion, var = var[x],
                              time_span = periodo) |> 
      mutate(datetime = as_datetime(datetime,tz = 'America/Santiago'))
    if (x == 1) {var_df <- var_x}
    else {var_df <- var_df |> 
      left_join(var_x, by = 'datetime')}
  }
  
  var_df <- var_df |> 
    group_by(datetime = floor_date(datetime,'30 min')) |> 
    summarise(t_media = mean(`avg (°C)`,na.rm=T),
              t_max = max(`max (°C)`,na.rm=T),
              t_min = min(`min (°C)`,na.rm=T),
              vpd_medio = mean(`avg (mbar)`,na.rm=T),
              vpd_min = min(`min (mbar)`,na.rm=T),
              eto = sum(`ETo[mm] (mm)`,na.rm=T),
              pp = sum(`sum (mm)`,na.rm=T),
              rh_media = mean(`avg (%)`,na.rm=T),
              rh_max = max(`max (%)`,na.rmn=T),
              rh_min = min(`min (%)`,na.rm=T))
  
}
coeficientes <- function(x1, x2, y, minRow = 3) {

  df <- data.frame(x1 = x1, x2 = x2, y = y) |> 
    na.omit()

  if (nrow(df) < minRow) {
    return(list(m1 = NA, m2 = NA, int = NA, r = NA))
  }

  modelo <- lm(y ~ x1 + x2, data = df)

  coeficientes <- coef(modelo)
  pendientes <- as.numeric(coeficientes[2:3])
  intercepto <- as.numeric(coeficientes[1])
  coef_cor <- as.numeric(summary(modelo)$r.squared^.5)

  return(list(m1 = pendientes[1], m2 = pendientes[2], int = intercepto, r = coef_cor))
}
pca <- function(x1,x2,x3) {
  df <- data.frame(x1,x2,x3)
  pca_result <- predict(prcomp(df,scale. = T))
  return(data.frame(pca_result))
}