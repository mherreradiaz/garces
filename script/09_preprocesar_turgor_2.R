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
cor_clima <- function(turgor,temperatura,vpd) {
  
  if (nrow(na.omit(data.frame(turgor,temperatura,vpd)))==0) {return(NA)} else {
  
  cor_temperatura <- cor(turgor,temperatura, use = 'complete.obs')^2
  cor_vpd <- cor(turgor,vpd, use = 'complete.obs')^2
  
  cor_index <- (cor_temperatura+cor_vpd)/2
  
  return(cor_index)
  
  }
}
dif_mean <- function(turgor,temperatura,vpd) {
  
  if (nrow(na.omit(data.frame(turgor,temperatura,vpd)))==0) {return(NA)} else {
   
    dif_t <- mean(abs(turgor-temperatura))
    dif_vpd <- mean(abs(turgor-vpd))
    
    dif_index <- (dif_t+dif_vpd)/2
    
    return(dif_index)
     
  }
  
}

# CORRELACIÓN DIARIA

data_turgor <- read_rds('data/data_processed/zim_turgor.rds') |> 
  group_by(sitio,fecha,sensor) |> 
  mutate(turgor_sc = scale(turgor)) |> 
  ungroup()

data_clima <- read_rds('data/data_processed/clima.rds') |>
  select(sitio:hora,t_media,vpd_media) |>
  group_by(sitio,fecha) |>
  mutate(t_sc = scale(t_media),
         vpd_sc = scale(log(vpd_media+1))) |>
  ungroup() |> 
  select(-t_media,-vpd_media)

data_cor <- data_turgor |>
  left_join(data_clima,by=c('sitio','temporada','fecha','hora')) |> 
  group_by(sitio,temporada,fecha,sensor) |> 
  summarise(cor_index = as.numeric(cor_clima(turgor_sc,t_sc,vpd_sc)),
            dif_index = dif_mean(turgor_sc,t_sc,vpd_sc)) |>
  ungroup()
  
# data_cor |> 
#   mutate(fecha = ifelse(temporada == '2023-2024',gsub('2023', '2022', fecha),fecha),
#          fecha = ifelse(temporada == '2023-2024',gsub('2024', '2023', fecha),fecha),
#          fecha = as.Date(fecha)) |> 
#   ggplot(aes(fecha,dif_index)) +
#   geom_point() +
#   facet_grid(temporada~., scales = 'free_x') +
#   scale_x_date(date_breaks = "2 month", date_labels = "%b",
#                    limits = as.Date(c("2022-10-01", "2023-04-3")))

filtro <- data_cor |> 
  filter(cor_index > .5,
         dif_index < 1) |> 
  mutate(filtro = paste(sitio,fecha,sensor)) |> 
  pull(filtro)

data_turgor <- data_turgor |> 
  filter(paste(sitio,fecha,sensor) %in% filtro)

write_rds(data_turgor,'data/data_processed/zim_turgor_preprocesado.rds')

# t <- '2022-2023'
# 
# data_turgor |>
#   filter(temporada == t) |>
#   mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'),
#                                  format = "%Y-%m-%d %H:%M")) |>
#   ggplot(aes(fecha_hora,turgor_sc,color=zim)) +
#   geom_point(size = .1) +
#   scale_x_datetime(date_breaks = "2 month", date_labels = "%b",
#                    limits = as.POSIXct(c("2022-10-01", "2023-04-3"))) +
#   facet_grid(tratamiento+unidad~sitio, labeller = as_labeller(names)) +
#   labs(x = "Meses", y = "Presión de parche (bar)",
#        color = 'Zim') +
#   theme_light() +
#   theme(text = element_text(size = 13),
#         axis.title.y = element_text(margin = margin(r = 17)),
#         axis.title.x = element_text(margin = margin(t = 17))) +
#   guides(colour = guide_legend(override.aes = list(size=4)))

# GRAFICAR

data_turgor_old <- read_rds('data/data_processed/zim_turgor.rds') |> 
  left_join(read_rds('data/data_processed/zim_turgor_preprocesado.rds'),
            by = c('sitio','temporada','fecha','hora','tratamiento','unidad','codigo','zim','sensor','turgor')) |> 
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_turgor <- read_rds('data/data_processed/zim_turgor_preprocesado.rds') |> 
  mutate(fecha_hora = as.POSIXct(paste0(fecha,' ',hora,':00'), format = "%Y-%m-%d %H:%M"))

data_clima <- read_rds('data/data_processed/clima.rds') |>
  select(sitio:hora,t_media,vpd_media) |>
  group_by(sitio,fecha) |>
  mutate(t_sc = scale(t_media),
         vpd_sc = scale(log(vpd_media+1))) |>
  ungroup() |> 
  select(-t_media,-vpd_media)

data_turgor <- data_turgor |> 
  left_join(data_clima, by = c('sitio','temporada','fecha','hora'))

codigos <- data_turgor |>
  select(sitio,temporada,tratamiento,unidad,zim, sensor) |>
  distinct() |>
  arrange(temporada,sitio,tratamiento,unidad,zim) |>
  spread(key = 'zim', value = 'sensor')

for (x in 1:nrow(codigos)) {
  
  sensores <- c(codigos$Z1[x],codigos$Z2[x]) |>
    na.omit() |> as.numeric()
  
  plot_name <- paste(codigos$temporada[x],
                     str_to_title(str_replace_all(codigos$sitio[x], "_", " ")),
                     codigos$tratamiento[x],
                     'Unidad', codigos$unidad[x])
  
  combined_plot_final <- NULL
  
  if (codigos$temporada[x] == '2022-2023') {lim <- c(as.POSIXct('2022-09-30'),as.POSIXct('2023-04-01'))
  } else {lim <- c(as.POSIXct('2023-09-30'), max(as.POSIXct(data_turgor$fecha)))}
  
  for (s in 1:length(sensores)) {
    
    data_sensor <- data_turgor |>
      filter(sensor == sensores[s],
             temporada == codigos$temporada[x])
    
    data_sensor_old <- data_turgor_old |>
      filter(sensor == sensores[s],
             temporada == codigos$temporada[x])
    
    p1 <- data_sensor_old |>
      pivot_longer(cols = c(turgor,turgor_sc),names_to = 'proceso',values_to = 'valor') |>
      ggplot(aes(fecha_hora,valor)) +
      geom_point(size = .5) +
      facet_wrap(~proceso, ncol = 1, scales = "free_y", 
                 labeller = as_labeller(c(turgor = 'Presión de parche (crudo)',
                                          turgor_sc = 'Presión de parche (estand.)'))) +
      scale_x_datetime(date_breaks = "2 month", date_labels = "%b",
                       limits = lim) +
      labs(title = paste('Sensor', sensores[s]),
           y = 'Presión de parche',
           x = 'Meses') +
      theme_light() +
      theme(text = element_text(size = 13),
            plot.title = element_text(hjust = 0.5, margin = margin(b = 15)),
            axis.title.y = element_text(margin = margin(r = 17)),
            axis.title.x = element_text(margin = margin(t = 17)),
            legend.position = 'none') +
      guides(colour = guide_legend(override.aes = list(size=4)))
    
    max <- max(data_sensor$turgor_sc)-.1*(max(data_sensor$turgor_sc)-min(data_sensor$turgor_sc))
    
    cor(data_sensor$turgor_sc,data_sensor$t_sc)^2
    cor(data_sensor$turgor_sc,data_sensor$vpd_sc)^2
    
    p2 <- data_sensor |>
      pivot_longer(cols = c(vpd_sc,t_sc), names_to = 'variable',values_to = 'valor') |>
      ggplot(aes(valor,turgor_sc)) +
      geom_point() +
      geom_smooth(method = 'lm', se = F) +
      stat_cor(aes(label = paste(after_stat(rr.label), ..p.label.., sep = "~`,`~")),
               method = 'pearson', label.y = max, color = 'black',geom = 'label') +
      facet_wrap(~variable, scale = 'fixed', ncol = 2, strip.position = 'top',
                 labeller = as_labeller(c(t_sc = 'T° media (estand.)',
                                          vpd_sc = 'VPD medio (log-estand.)'))) +
      labs(title = 'Correlación con Temperatura y VPD',
           y = 'Presión de parche (estand.)',
           x = NULL) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
    p3 <- data_sensor |>
      group_by(hora) |>
      summarize(turgor_hora = mean(turgor_sc, na.rm = TRUE)) |>
      mutate(color = as.factor(1)) |>
      ggplot(aes(hora, turgor_hora, color = color)) +
      geom_point(size = 1) +
      geom_line(linewidth = .7) +
      labs(title = 'Ciclo horario del día',
           x = 'Hora',
           y = 'Presión de parche (est.)') +
      theme_bw() +
      scale_color_manual(values = "black") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      guides(color = guide_legend(override.aes = list(color = NA)))
    
    p1 <- p1 + theme(plot.margin = margin(10, 10, 10, 10))
    p2 <- p2 + theme(plot.margin = margin(10, 10, 10, 10))
    p3 <- p3 + theme(plot.margin = margin(10, 10, 10, 10))
    
    combined_plot <- plot_grid(p1, plot_grid(p2,p3,ncol = 2, rel_widths = c(2,1)), 
                               ncol = 1, rel_heights = c(1.5, 1))
    
    if (is.null(combined_plot_final)) {
      combined_plot_final <- combined_plot
    } else {
      combined_plot_final <- plot_grid(combined_plot_final, combined_plot, ncol = 2, rel_widths = c(1, 1))
    }
  }
  png(paste0('reporte/plots/01_turgor_preprocesado_2/', gsub('-','_',str_replace_all(plot_name, "\\s", "_")), '.png'),
      width = 14 * 300, height = 7 * 300, units = "px", res = 300)
  print(combined_plot_final)
  dev.off()
  
}

data_sensor |> 
  ggplot(aes(fecha_hora,turgor_sc)) +
  geom_point() +
  geom_line() +
  geom_line(aes(fecha_hora,t_sc), color = 'red3') +
  geom_line(aes(fecha_hora,vpd_sc), color = 'blue1') +
  scale_x_datetime(limits = c(as.POSIXct('2022-11-01'),as.POSIXct('2022-11-05')))

