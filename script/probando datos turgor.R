
library(ggplot2)
library(tidyverse)
library(highcharter)
library(gt)
library(readr)
library(anomalize)
library(tibbletime)
library(forecast)
library(prophet)
library(scales)
library(ggpubr)
library(moments)
library(e1071)
es_norm <- function(vector, nivel_confianza = 0.05) {
  prueba_shapiro <- shapiro.test(vector)
  return(prueba_shapiro$p.value > nivel_confianza)
}
prophet_predict <- function(ds,y) {
  df <- tibble(ds,y)
  y_suavizada <- prophet(df) |>
    predict(df = df) |>
    mutate(ds = force_tz(ds, tzone = "UTC")) |>
    pull(yhat)
  return(y_suavizada)
}

data_turgor <- read_rds('C:/HEMERA/ID2023/id23_10297/data/data_processed/zim_turgor.rds') |>
  filter(temporada == '2023-2024')

sensores_arbol <- data_turgor |>
  select(sitio,tratamiento,unidad,codigo,zim,sensor) |>
  distinct() |>
  arrange(sitio,tratamiento,unidad,zim)

sensores_filtro <- read.xlsx('data/metadata/turgor_filter.xlsx', sheetIndex = 1) |>
  mutate(from = as.character(from),
         to = as.character(to),
         from = ifelse(is.na(from), '2023-10-09',from),
         to = ifelse(is.na(to), substr(now(),1,10),to))

s_f <- unname(unlist(c(sensores_filtro[1,1:3],sensores_filtro[2,1:3])))

# LIMPIEZA Y PROMEDIO SIMPLE ####

data <- data_turgor |>
  filter(sensor %in% s_f[c(1,4)]) |>
  drop_na() |>
  group_by(sensor) |>
  mutate(fecha = as.POSIXct(paste(fecha, hora), format="%Y-%m-%d %H", tz="UTC"),
         y = scale(turgor))

ggplot(data, aes(x = fecha, y = y, color = zim)) +
  geom_line() +
  labs(title = "Curvas de turgor normalizado",
       x = "Fecha",
       y = "Turgor normalizado") +
  scale_color_manual(values = c('blue','red')) +
  theme_minimal()

filtered_data <- data_turgor |>
  filter(sensor %in% s_f[c(1,4)]) |>
  filter(!(sensor == s_f[1] & !between(fecha, s_f[2], s_f[3])),
         !(sensor == s_f[4] & !between(fecha, s_f[5], s_f[6]))) |>
  drop_na() |>
  group_by(sensor) |>
  mutate(fecha = as.POSIXct(paste(fecha, hora), format="%Y-%m-%d %H", tz="UTC"),
         y = scale(turgor))

ggplot(filtered_data, aes(x = fecha, y = y, color = zim)) +
  geom_line() +
  labs(title = "Curvas de turgor normalizado",
       x = "Fecha",
       y = "Turgor normalizado") +
  scale_color_manual(values = c('blue','red')) +
  theme_minimal()

{
  promedio_y <- filtered_data |>
    group_by(fecha) |>
    summarise(y_promedio = mean(y))
  prom_data <- filtered_data |>
    left_join(promedio_y, by = "fecha") |>
    ungroup()
  
  r2 <- prom_data |>
    group_by(fecha) |>
    filter(n() > 1) |>
    group_by(zim) |>
    summarize(r2 = cor(y, y_promedio)^2) |> 
    pull(r2)
  }

ggplot(prom_data, aes(x = fecha, y = y, color = zim)) +
  geom_line(linetype = "dashed") +
  geom_line(aes(x= fecha, y=y_promedio), color = 'black') +
  labs(title = "Curvas de turgor normalizado",
       x = "Fecha",
       y = "Turgor normalizado") +
  scale_color_manual(values = c('blue','red')) +
  theme_minimal() +
  geom_text(aes(label = paste("R² z1=", round(r2[1], 2))),
            x = Inf, y = -Inf, hjust = 1, vjust = -2, size = 4, color = 'black') +
  geom_text(aes(label = paste("R² z2=", round(r2[2], 2))),
            x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 4, color = 'black')

# DIFERENCIAS ####

{
  data <- data_turgor |>
    filter(sensor %in% s_f[c(1,4)]) |>
    drop_na() |>
    group_by(sensor) |>
    mutate(fecha = as.POSIXct(paste(fecha, hora), format="%Y-%m-%d %H", tz="UTC"),
           y = scale(turgor))
  
  data_z1 <- data |>
    filter(sensor == s_f[1])
  data_z2 <- data |>
    filter(sensor == s_f[4])
  split_data <- data_z1 |>
    left_join(data_z2, by = 'fecha') |>
    rename(y_z1 = y.x,
           y_z2 = y.y) |>
    select(fecha,y_z1,y_z2) |>
    mutate(dif_y = abs(y_z1-y_z2),
           y_mean = (y_z1+y_z2)/2)
  }

ggplot(split_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1')) +
  geom_line(aes(y = y_z2, color = 'Zim 2')) +
  geom_line(aes(y = dif_y, color = "Diferencia"), linetype = "dashed") +
  labs(title = "Curvas de turgor normalizado y Diferencias",
       x = "Fecha",
       y = "Turgor normalizado y Diferencia") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Diferencia" = 'black'))

{
  q3 <- quantile(split_data$dif_y, 0.75)
  filtered_data <- split_data |>
    mutate(IQR = ifelse(dif_y > q3,1,0),
           y_z1_fil = ifelse(IQR == 1,ifelse(abs(y_z1)<abs(y_z2),y_z1,NA),y_z1),
           y_z2_fil = ifelse(IQR == 1,ifelse(abs(y_z2)<abs(y_z1),y_z2,NA),y_z2))
  lim_z1 <- filtered_data |>
    summarize(low_lim = boxplot.stats(y_z1_fil)$stats[1],
              up_lim = boxplot.stats(y_z1_fil)$stats[5])
  lim_z2 <- filtered_data |>
    summarize(low_lim = boxplot.stats(y_z2_fil)$stats[1],
              up_lim = boxplot.stats(y_z2_fil)$stats[5])
  filtered2_data <- filtered_data |>
    mutate(y_z1_fil2 = ifelse(y_z1_fil < lim_z1$low_lim,NA,ifelse(y_z1_fil > lim_z1$up_lim,NA,y_z1_fil)),
           y_z2_fil2 = ifelse(y_z2_fil < lim_z2$low_lim,NA,ifelse(y_z2_fil > lim_z2$up_lim,NA,y_z2_fil)),
           y_arbol = coalesce((y_z1_fil2 + y_z2_fil2) / 2,y_z1_fil2, y_z2_fil2))
  }

ggplot(filtered2_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1'), linetype = "dashed") +
  geom_line(aes(y = y_z2, color = 'Zim 2'), linetype = "dashed") +
  geom_line(aes(y = y_z1_fil, color = 'Zim 1')) +
  geom_line(aes(y = y_z2_fil, color = 'Zim 2')) +
  #geom_line(aes(y = dif_y, color = "Diferencia")) +
  labs(title = "Curvas de turgor normalizado (Filtradas)",
       x = "Fecha",
       y = "Turgor normalizado") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Diferencia" = "black"))

ggplot(filtered2_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1'), linetype = "dashed") +
  geom_line(aes(y = y_z2, color = 'Zim 2'), linetype = "dashed") +
  geom_line(aes(y = y_z1_fil2, color = 'Zim 1')) +
  geom_line(aes(y = y_z2_fil2, color = 'Zim 2')) +
  #geom_line(aes(y = dif_y, color = "Diferencia")) +
  labs(title = "Curvas de turgor normalizado y Diferencias (Filtradas 2)",
       x = "Fecha",
       y = "Turgor normalizado") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Diferencia" = "black"))

ggplot(filtered2_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1'), linetype = "dashed") +
  geom_line(aes(y = y_z2, color = 'Zim 2'), linetype = "dashed") +
  geom_line(aes(y = y_arbol, color = "Procesado")) +
  labs(title = "Curvas de turgor normalizado y procesado",
       x = "Fecha",
       y = "Turgor normalizado") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Procesado" = 'black'))

# SEGMENTO ####

{
  data <- data_turgor |>
    filter(sensor %in% s_f[c(1,4)]) |>
    drop_na() |>
    group_by(sensor) |>
    mutate(fecha = as.POSIXct(paste(fecha, hora), format="%Y-%m-%d %H", tz="UTC"),
           y = scale(turgor))
  
  data_z1 <- data |>
    filter(sensor == s_f[1])
  data_z2 <- data |>
    filter(sensor == s_f[4])
  split_data <- data_z1 |>
    left_join(data_z2, by = 'fecha') |>
    rename(y_z1 = y.x,
           y_z2 = y.y) |>
    select(fecha,y_z1,y_z2) |>
    mutate(dif_y = abs(y_z1-y_z2),
           y_mean = (y_z1+y_z2)/2)
}

ggplot(split_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1')) +
  geom_line(aes(y = y_z2, color = 'Zim 2')) +
  geom_line(aes(y = dif_y, color = "Diferencia"), linetype = "dashed") +
  labs(title = "Curvas de turgor normalizado y Diferencias",
       x = "Fecha",
       y = "Turgor normalizado y Diferencia") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Diferencia" = 'black'))

{
  q3 <- quantile(split_data$dif_y, 0.75)
  filtered_seg_data <- split_data |>
    mutate(IQR = ifelse(dif_y > q3,1,0),
           segmento = cumsum(c(0, diff(IQR) != 0))) |>
    group_by(fecha, segmento) |>
    mutate(segmento = ifelse(all(IQR == 0), NA, segmento)) |>
    ungroup()
  
  filtered_seg_table <- filtered_seg_data |>
    group_by(segmento) |>
    summarise(seg_z1 = abs(mean(y_z1)),
              seg_z2 = abs(mean(y_z2))) |>
    filter(!is.na(segmento))
  
  filtered_seg_data <- filtered_seg_data |>
    left_join(filtered_seg_table,by = "segmento") |>
    mutate(y_z1_fil = ifelse(!is.na(segmento),ifelse(seg_z1<seg_z2,y_z1,NA),y_z1),
           y_z2_fil = ifelse(!is.na(segmento),ifelse(seg_z2<seg_z1,y_z2,NA),y_z2))
  
  lim_z1 <- filtered_seg_data |>
    summarize(low_lim = boxplot.stats(y_z1_fil)$stats[1],
              up_lim = boxplot.stats(y_z1_fil)$stats[5])
  lim_z2 <- filtered_seg_data |>
    summarize(low_lim = boxplot.stats(y_z2_fil)$stats[1],
              up_lim = boxplot.stats(y_z2_fil)$stats[5])
  filtered2_seg_data <- filtered_seg_data |>
    mutate(y_z1_fil2 = ifelse(y_z1_fil < lim_z1$low_lim,NA,ifelse(y_z1_fil > lim_z1$up_lim,NA,y_z1_fil)),
           y_z2_fil2 = ifelse(y_z2_fil < lim_z2$low_lim,NA,ifelse(y_z2_fil > lim_z2$up_lim,NA,y_z2_fil)),
           y_arbol = coalesce((y_z1_fil2 + y_z2_fil2) / 2,y_z1_fil2, y_z2_fil2))
  }

ggplot(filtered2_seg_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1'), linetype = "dashed") +
  geom_line(aes(y = y_z2, color = 'Zim 2'), linetype = "dashed") +
  geom_line(aes(y = y_z1_fil, color = 'Zim 1')) +
  geom_line(aes(y = y_z2_fil, color = 'Zim 2')) +
  #geom_line(aes(y = dif_y, color = "Diferencia")) +
  labs(title = "Curvas de turgor normalizado y Diferencias (Filtradas)",
       x = "Fecha",
       y = "Turgor normalizado") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Diferencia" = "black"))

ggplot(filtered2_seg_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1'), linetype = "dashed") +
  geom_line(aes(y = y_z2, color = 'Zim 2'), linetype = "dashed") +
  geom_line(aes(y = y_z1_fil2, color = 'Zim 1')) +
  geom_line(aes(y = y_z2_fil2, color = 'Zim 2')) +
  #geom_line(aes(y = dif_y, color = "Diferencia")) +
  labs(title = "Curvas de turgor normalizado y Diferencias (Filtradas 2)",
       x = "Fecha",
       y = "Turgor normalizado") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Diferencia" = "black"))

ggplot(filtered2_seg_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1'), linetype = "dashed") +
  geom_line(aes(y = y_z2, color = 'Zim 2'), linetype = "dashed") +
  geom_line(aes(y = y_arbol, color = "Procesado")) +
  labs(title = "Curvas de turgor normalizado y procesado",
       x = "Fecha",
       y = "Turgor normalizado") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Procesado" = 'black'))

# AMPLITUD ####

{
  data <- data_turgor |>
    filter(sensor %in% s_f[c(1,4)]) |>
    drop_na() |>
    group_by(sensor) |>
    mutate(fecha = as.POSIXct(paste(fecha, hora), format="%Y-%m-%d %H", tz="UTC"),
           y = scale(turgor))
  
  data_z1 <- data |>
    filter(sensor == s_f[1])
  data_z2 <- data |>
    filter(sensor == s_f[4])
  split_data <- data_z1 |>
    left_join(data_z2, by = 'fecha') |>
    rename(y_z1 = y.x,
           y_z2 = y.y) |>
    select(fecha,y_z1,y_z2) |>
    mutate(dif_y = y_z1-y_z2,
           y_mean = (y_z1+y_z2)/2)
}

ggplot(split_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1')) +
  geom_line(aes(y = y_z2, color = 'Zim 2')) +
  #geom_line(aes(y = dif_y, color = "Diferencia"), linetype = "dashed") +
  labs(title = "Curvas de turgor normalizado",
       x = "Fecha",
       y = "Turgor normalizado") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Diferencia" = 'black'))

{
  amp_table <- split_data |>
    mutate(dia = as.Date(fecha)) |>
    group_by(dia) |>
    summarise(y_z1_amp = max(y_z1)-min(y_z1),
              y_z2_amp = max(y_z2)-min(y_z2))
  amp_prom <- c(mean(amp_table$y_z1_amp),mean(amp_table$y_z2_amp))

  filtered_amp_data <- split_data |>
    select(-dif_y,-y_mean) |>
    mutate(dia = as.Date(fecha)) |>
    left_join(amp_table,by = "dia") |>
    select(-dia) |>
    mutate(norm_z1 = ifelse(es_norm(amp_table$y_z1_amp),1,0),
           norm_z2 = ifelse(es_norm(amp_table$y_z2_amp),1,0),
           y_z1 = ifelse(norm_z1 == 1,y_z1,ifelse(y_z1_amp<amp_prom[1],NA,y_z1)),
           y_z2 = ifelse(norm_z2 == 1,y_z2,ifelse(y_z2_amp<amp_prom[2],NA,y_z2))) |>
    select(-y_z1_amp,-y_z2_amp,-norm_z1,-norm_z2) |>
    mutate(y_z1 = scale(y_z1),
           y_z2 = scale(y_z2)) |>
    select(fecha,y_z1,y_z2) |>
    mutate(dif_y = abs(y_z1-y_z2),
           y_mean = (y_z1+y_z2)/2)
  
  q3 <- quantile(split_data$dif_y, 0.75)
  filtered_seg_data <- filtered_amp_data |>
    mutate(IQR = ifelse(dif_y > q3,1,0),
           segmento = cumsum(c(0, diff(IQR) != 0))) |>
    group_by(fecha, segmento) |>
    mutate(segmento = ifelse(all(IQR == 0), NA, segmento)) |>
    ungroup()
  
  filtered_seg_table <- filtered_seg_data |>
    group_by(segmento) |>
    summarise(seg_z1 = abs(mean(y_z1)),
              seg_z2 = abs(mean(y_z2))) |>
    filter(!is.na(segmento))
  
  filtered_seg_data <- filtered_seg_data |>
    left_join(filtered_seg_table,by = "segmento") |>
    mutate(y_z1_fil = ifelse(!is.na(segmento),ifelse(seg_z1<seg_z2,y_z1,NA),y_z1),
           y_z2_fil = ifelse(!is.na(segmento),ifelse(seg_z2<seg_z1,y_z2,NA),y_z2))
  
  lim_z1 <- filtered_seg_data |>
    summarize(low_lim = boxplot.stats(y_z1_fil)$stats[1],
              up_lim = boxplot.stats(y_z1_fil)$stats[5])
  lim_z2 <- filtered_seg_data |>
    summarize(low_lim = boxplot.stats(y_z2_fil)$stats[1],
              up_lim = boxplot.stats(y_z2_fil)$stats[5])
  filtered2_seg_data <- filtered_seg_data |>
    mutate(y_z1_fil2 = ifelse(y_z1_fil < lim_z1$low_lim,NA,ifelse(y_z1_fil > lim_z1$up_lim,NA,y_z1_fil)),
           y_z2_fil2 = ifelse(y_z2_fil < lim_z2$low_lim,NA,ifelse(y_z2_fil > lim_z2$up_lim,NA,y_z2_fil)),
           y_arbol = coalesce((y_z1_fil2 + y_z2_fil2) / 2,y_z1_fil2, y_z2_fil2))
  }

ggplot(filtered_amp_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1')) +
  geom_line(aes(y = y_z2, color = 'Zim 2')) +
  #geom_line(aes(y = dif_y, color = "Diferencia"), linetype = "dashed") +
  labs(title = "Curvas de turgor normalizado",
       x = "Fecha",
       y = "Turgor normalizado") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Diferencia" = 'black'))

ggplot(filtered2_seg_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1'), linetype = "dashed") +
  geom_line(aes(y = y_z2, color = 'Zim 2'), linetype = "dashed") +
  geom_line(aes(y = y_z1_fil2, color = 'Zim 1')) +
  geom_line(aes(y = y_z2_fil2, color = 'Zim 2')) +
  #geom_line(aes(y = dif_y, color = "Diferencia")) +
  labs(title = "Curvas de turgor normalizado y (Filtradas 2)",
       x = "Fecha",
       y = "Turgor normalizado") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Diferencia" = "black"))

ggplot(filtered2_seg_data, aes(x = fecha)) +
  geom_line(aes(y = y_z1, color = 'Zim 1'), linetype = "dashed") +
  geom_line(aes(y = y_z2, color = 'Zim 2'), linetype = "dashed") +
  geom_line(aes(y = y_arbol, color = "Procesado")) +
  labs(title = "Curvas de turgor normalizado y procesado",
       x = "Fecha",
       y = "Turgor normalizado") +
  theme_minimal() +
  scale_color_manual(values = c('Zim 1' = "blue", 'Zim 2' = "red", "Procesado" = 'black'))


###

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

ggplot(data, aes(x = fecha, y = turgor_sc)) +
  geom_line(color = "black") +
  geom_line(aes(y = y_promedio), linetype = "dashed", color = "red") +
  #geom_line(aes(y = y_modelo_promedio), linetype = "dashed", color = "red") +
  #geom_line(aes(y = y_proc), linetype = "dashed", color = "red") +
  labs(title = "Series Temporales",
       x = "Fecha",
       y = "Valor") +
  scale_x_datetime(breaks = seq(min(data$fecha), max(data$fecha), by = "5 days"), date_labels = "%b %d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(sensor ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme(strip.text.x = element_text(size = 14, face = "bold")) +
  geom_text(data = r2_values, aes(label = paste("R² =", round(r2, 2))),
            x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 4)
