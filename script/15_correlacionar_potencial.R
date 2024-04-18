source('reporte/book/paquetes.R')
normalizar<- function(vector, min_range = 0, max_range = 1) {
  
  vector_normalizado <- (vector - min(vector,na.rm=T)) / (max(vector,na.rm=T) - min(vector,na.rm=T))
  vector_normalizado <- vector_normalizado * (max_range - min_range) + min_range
  return(vector_normalizado)
}

data_turgor <- read_rds('data/data_processed/zim_turgor_preprocesado.rds') |> 
  select(-turgor,-turgor_sc) |> 
  group_by(sitio,temporada,fecha,hora,tratamiento,unidad,codigo) |> 
  summarise(turgor = mean(turgor_cl,na.rm=T),
            turgor_sc = mean(turgor_sc_cl,na.rm=T)) |> 
  ungroup()

data_clima <- read_rds('data/data_processed/clima_lag.rds')

data <- data_turgor |> 
  left_join(data_clima, by = c('sitio','temporada','fecha','hora'))

data_potencial_dia <- read_rds('data/data_processed/potencial.rds') |> 
  mutate(fecha = as.character(fecha),
         .before = tratamiento) |> 
  rename(potencial = potencial_bar) |> 
  group_by(sitio,temporada) |> 
  mutate(potencial_sc = as.numeric(scale(potencial))) |> 
  ungroup() |> 
  mutate(hora = '13:30',
         .before = tratamiento)

data_potencial_hora <- read_rds('data/data_processed/potencial_horario.rds') |> 
  rename(potencial = bar) |> 
  group_by(sitio,temporada,tratamiento,unidad) |> 
  mutate(potencial_sc = as.numeric(scale(potencial))) |> 
  ungroup()

data_potencial <- bind_rows(data_potencial_hora,data_potencial_dia) |> 
  distinct(sitio,temporada,fecha,hora,tratamiento,unidad,codigo, .keep_all = T)

data_cor <- data |> 
  left_join(data_potencial,by=c('sitio','temporada','fecha','hora','tratamiento','unidad','codigo'))

# data diaria

data_dia <- data_cor |> 
  filter(between(as.POSIXct(hora, format = "%H:%M"),
                 as.POSIXct('13:00', format = "%H:%M"),
                 as.POSIXct('14:00', format = "%H:%M"))) |> 
  group_by(sitio,temporada,fecha,tratamiento,unidad,codigo) |> 
  summarise(turgor = mean(turgor,na.rm=T),
            turgor_sc = mean(turgor_sc,na.rm=T),
            potencial = mean(potencial,na.rm=T),
            potencial_sc = mean(potencial_sc,na.rm=T),
            t = mean(t_media,na.rm=T),
            t_sc = mean(t_sc,na.rm=T),
            t_sc_dia = mean(t_sc_dia,na.rm=T),
            vpd = mean(vpd_media,na.rm=T),
            vpd_sc = mean(vpd_sc,na.rm=T),
            vpd_sc_dia = mean(vpd_sc_dia,na.rm=T)) |> 
  ungroup() |> 
  mutate(fecha_hora = as.POSIXct(paste(fecha,'13:30'),format='%Y-%m-%d %H:%M'),
         .before = tratamiento) |> 
  filter()

# data horaria

data_hora <- data_cor |> 
  filter(paste(sitio,fecha) %in% unique(paste(data_potencial_hora$sitio,data_potencial_hora$fecha)),
         between(as.POSIXct(hora, format = "%H:%M"),
                 as.POSIXct('07:00', format = "%H:%M"),
                 as.POSIXct('22:00', format = "%H:%M"))) |> 
  mutate(fecha_hora = as.POSIXct(paste(fecha,hora),format='%Y-%m-%d %H:%M'),
         .before = tratamiento) |> 
  filter(!(temporada == '2023-2024' & hora == '13:30'))

# Serie turgor dia

data_dia_2022 <- data_dia |> 
  filter(temporada == '2022-2023')

data_dia_2023 <- data_dia |> 
  filter(temporada == '2023-2024')

data_dia_2022 |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_dia_2022,aes(fecha_hora,turgor_sc, color = 'Turgor'), size = .7) +
  geom_line(data = data_dia_2022,aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_dia_2023 |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_dia_2023,aes(fecha_hora,turgor_sc, color = 'Turgor'), size = .7) +
  geom_line(data = data_dia_2023,aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

# Serie diaria tratamiento

data_dia_2022 <- data_dia |> 
  filter(temporada == '2022-2023') |> 
  group_by(sitio,temporada,fecha,fecha_hora,tratamiento) |> 
  summarise(potencial_sc = mean(potencial_sc,na.rm=T),
            turgor_sc = mean(turgor_sc,na.rm=T)) |> 
  ungroup()

data_dia_2023 <- data_dia |> 
  filter(temporada == '2023-2024') |> 
  group_by(sitio,temporada,fecha,fecha_hora,tratamiento) |> 
  summarise(potencial_sc = mean(potencial_sc,na.rm=T),
            turgor_sc = mean(turgor_sc,na.rm=T)) |> 
  ungroup()
  
data_dia_2022 |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_dia_2022,aes(fecha_hora,turgor_sc, color = 'Turgor'), size = .7) +
  geom_line(data = data_dia_2022,aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_dia_2023 |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_dia_2023,aes(fecha_hora,turgor_sc, color = 'Turgor'), size = .7) +
  geom_line(data = data_dia_2023,aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

# Serie horaria

data_hora |> 
  filter(temporada == '2022-2023') |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = 1) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(aes(fecha_hora,turgor_sc, color = 'Turgor'), size = 1) +
  geom_line(aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Hora',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_hora |> 
  filter(temporada == '2023-2024') |> 
  filter(!is.na(potencial_sc)) |> 
  ggplot(aes(fecha_hora,potencial_sc)) +
  geom_point(aes(color = 'Potencial'),size = 1) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(aes(fecha_hora,turgor_sc, color = 'Turgor'), size = 1) +
  geom_line(aes(fecha_hora,turgor_sc, color = 'Turgor')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Hora',
       y = 'Potencial (est)',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

# Scatterplot dia

data_dia |> 
  ggplot(aes(turgor_sc,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor',
       y = 'Potencial') +
  theme_light()  +
  facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names))

data_dia |> 
  ggplot(aes(turgor_sc,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor',
       y = 'Potencial') +
  theme_light()

# Scatterplot hora

data_hora |> 
  ggplot(aes(turgor_sc,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor',
       y = 'Potencial') +
  theme_light()  +
  facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names))

data_hora |> 
  ggplot(aes(turgor_sc,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Turgor',
       y = 'Potencial') +
  theme_light() 

# Modelo en función de datos horarios

ms <- lm(potencial~ turgor_sc, data = data_hora)

data_ms_dia <- data_dia |> 
  mutate(potencial_mod = ms$coefficients[[2]]*turgor_sc+ms$coefficients[[1]]) 

data_ms_hora <- data_hora |> 
  mutate(potencial_mod = ms$coefficients[[2]]*turgor_sc+ms$coefficients[[1]])

# Graficar serie modelada diaria

data_ms_dia_2022 <- data_ms_dia |> 
  filter(temporada == '2022-2023')

data_ms_dia_2023 <- data_ms_dia |> 
  filter(temporada == '2023-2024')

data_ms_dia_2022 |> 
  filter(!is.na(potencial)) |> 
  ggplot(aes(fecha_hora,potencial)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_ms_dia_2022,aes(fecha_hora,potencial_mod, color = 'Potencial modelado'), size = .7) +
  geom_line(data = data_ms_dia_2022,aes(fecha_hora,potencial_mod, color = 'Potencial modelado')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_ms_dia_2023 |> 
  filter(!is.na(potencial)) |> 
  ggplot(aes(fecha_hora,potencial)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_ms_dia_2023,aes(fecha_hora,potencial_mod, color = 'Potencial modelado'), size = .7) +
  geom_line(data = data_ms_dia_2023,aes(fecha_hora,potencial_mod, color = 'Potencial modelado')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_ms_dia |> 
  na.omit() |> 
  filter(temporada == '2022-2023') |> 
  

# Graficar serie modelada horaria

data_ms_hora |>
  mutate(fecha = case_when(temporada == '2023-2024' ~ as.POSIXct(fecha)-years(1))) |> 
  ggplot(aes(hora,potencial)) +
  geom_point(aes(color = as.factor('Potencial')), size = 1.2) +
  geom_point(aes(hora,turgor_sc, color = as.factor('Turgor estandarizado')), size = 1.2) +
  geom_point(aes(hora, potencial_mod, color = as.factor('Potencial modelado')), size = 1.2, alpha = .5) +
  labs(x = 'Hora',
       y = 'Potencial',
       color = '') +
  theme_light() +
  scale_x_discrete(breaks = paste0(sprintf('%02d',seq(0, 24, by = 3)),':00'),
                   labels = seq(0, 24, by = 3)) +
  facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names)) +
  theme(text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 13)),
        axis.title.x = element_text(margin = margin(t = 13)))

# Sctarreplot potencial medido vs modelado


# Modelo múltiple

mm <- lm(potencial~ turgor_sc + t_media + vpd_media, data = data_hora)

data_mm_dia <- data_dia |> 
  mutate(potencial_mod = mm$coefficients[[1]]+turgor_sc*mm$coefficients[[2]]+
                                              t*mm$coefficients[[3]]+
                                              vpd*mm$coefficients[[4]]) 
data_mm_hora <- data_hora |> 
  mutate(potencial_mod = mm$coefficients[[1]]+turgor_sc*mm$coefficients[[2]]+
           t_media*mm$coefficients[[3]]+
           vpd_media*mm$coefficients[[4]])

# Graficar serie modelada diaria

data_mm_dia_2022 <- data_mm_dia |> 
  filter(temporada == '2022-2023')

data_mm_dia_2023 <- data_mm_dia |> 
  filter(temporada == '2023-2024')

data_mm_dia_2022 |> 
  filter(!is.na(potencial)) |> 
  ggplot(aes(fecha_hora,potencial)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_mm_dia_2022,aes(fecha_hora,potencial_mod, color = 'Potencial modelado'), size = .7) +
  geom_line(data = data_mm_dia_2022,aes(fecha_hora,potencial_mod, color = 'Potencial modelado')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

data_mm_dia_2023 |> 
  filter(!is.na(potencial)) |> 
  ggplot(aes(fecha_hora,potencial)) +
  geom_point(aes(color = 'Potencial'),size = .7) +
  geom_line(aes(color = 'Potencial')) +
  geom_point(data = data_mm_dia_2023,aes(fecha_hora,potencial_mod, color = 'Potencial modelado'), size = .7) +
  geom_line(data = data_mm_dia_2023,aes(fecha_hora,potencial_mod, color = 'Potencial modelado')) +
  theme_light() +
  facet_grid(tratamiento+unidad~sitio, scales = 'free', labeller = as_labeller(names)) +
  labs(x = 'Mes',
       y = 'Potencial',
       color = '') +
  theme(text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 17)),
        axis.title.x = element_text(margin = margin(t = 17))) +
  guides(colour = guide_legend(override.aes = list(size=2)))

# Graficar serie modelada horaria

data_mm_hora |>
  mutate(fecha = case_when(temporada == '2023-2024' ~ as.POSIXct(fecha)-years(1))) |> 
  ggplot(aes(hora,potencial)) +
  geom_point(aes(color = as.factor('Potencial')), size = 1.2) +
  geom_point(aes(hora,turgor_sc, color = as.factor('Turgor estandarizado')), size = 1.2) +
  geom_point(aes(hora, potencial_mod, color = as.factor('Potencial modelado')), size = 1.2, alpha = .5) +
  labs(x = 'Hora',
       y = 'Potencial',
       color = '') +
  theme_light() +
  scale_x_discrete(breaks = paste0(sprintf('%02d',seq(0, 24, by = 3)),':00'),
                   labels = seq(0, 24, by = 3)) +
  facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names)) +
  theme(text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 13)),
        axis.title.x = element_text(margin = margin(t = 13)))

# Graficar serie modelada horaria

data_modelo |> 
  mutate(fecha_hora = as.POSIXct(paste(fecha,hora),format='%Y-%m-%d %H:%M'),
         .before=tratamiento) |> 
  left_join(fechas,by=c('sitio','fecha')) |> 
  filter(is.na(periodo)) |> 
  ggplot(aes(fecha_hora,potencial)) +
  geom_point() +
  geom_line() +
  facet_grid(tratamiento+unidad~temporada,scales = 'free_x')

# Scatterplot

data_modelo |> 
  left_join(fechas,by=c('sitio','fecha')) |> 
  filter(is.na(periodo)) |> 
  ggplot(aes(potencial_mod,potencial)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Potencial modelado',
       y = 'Potencial observado') +
  theme_light()
  #facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names))
  
# data_modelo |> 
#   na.omit() |> 
#   select(potencial,potencial_pred) |> 
#   write_csv('C:/Hemera/garces/prueba_2.csv')
    
data_modelo |> 
  ggplot(aes(potencial,potencial_pred)) +
  geom_point() +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Potencial observado',
       y = 'Potencial modelado') +
  theme_light()

data_modelo |> 
  ggplot(aes(potencial,potencial_pred)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "red") +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Potencial observado',
       y = 'Potencial modelado') +
  theme_light() +
  facet_grid(sitio~temporada, scales = 'free', labeller = as_labeller(names))

data_modelo |> 
  ggplot(aes(potencial,potencial_pred)) +
  geom_point() +
  stat_cor(aes(label = paste(after_stat(rr.label), sep = "~`,`~")),
           method = 'pearson', color = 'black',geom = 'label') +
  labs(x = 'Potencial observado',
       y = 'Potencial modelado') +
  theme_light()

