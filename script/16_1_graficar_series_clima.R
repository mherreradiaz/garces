source('script/funciones/paquetes.R')

names <- c(la_esperanza = 'La Esperanza',rio_claro = 'Rio Claro',
           T1 = 'T1',T2 = 'T2',T3 = 'T3',T4 = 'T4',T0 = 'T0',
           '1' = '1','2' = '2','3' = '3', 
           '2022-2023' = '2022-2023', '2023-2024' = '2023-2024')

Sys.setlocale("LC_TIME", "C")

# valores mensuales

data <- read_rds('data/processed/clima_dia.rds') |>
  mutate(fecha_mes = floor_date(as.Date(fecha), unit = "month"),
         mes = month(as.Date(fecha))) |> 
  filter(!mes %in% c(6,7)) |> 
  group_by(sitio,temporada,fecha_mes) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T),
            eto = mean(eto,na.rm=T),
            pp = mean(pp,na.rm=T)) |> 
  ungroup() |> 
  pivot_longer(cols=c('t_media','rh_media','vpd_medio','eto','pp'),
               names_to='variable',
               values_to='valor') |> 
  group_by(sitio,temporada,variable) |> 
  mutate(valor = scale(valor)) |> 
  ungroup()

data |> 
  ggplot(aes(fecha_mes,valor,color=variable)) +
  geom_point() +
  geom_line() +
  facet_grid(sitio~temporada,scales='free', labeller = as_labeller(names))

# valores diarios

data_potencial <- read_rds('data/processed/potencial_xgb_predict.rds') |> 
  select(sitio,temporada,fecha) |>
  distinct() |> 
  mutate(id=1)

data <- read_rds('data/processed/clima_dia.rds') |> 
  pivot_longer(cols=c('t_media','rh_media','vpd_medio','eto','pp'),
               names_to='variable',
               values_to='valor') |>  
  left_join(data_potencial,by=c('sitio','temporada','fecha')) |>
  filter(!is.na(id)) |>
  select(-id)

data_eto <- data |> 
  filter(variable %in% c('pp','eto')) |> 
  rename(acum = variable)

data |> 
  mutate(fecha = as.Date(fecha)) |> 
  filter(!variable %in% c('pp','eto')) |> 
  ggplot(aes(x = fecha, y = valor, color = variable)) +
  geom_point() +
  geom_bar(data = data_eto,
           aes(as.Date(fecha),valor,fill = acum), stat = 'identity') +
  geom_point(alpha = 0.5, size = .5) +  
  geom_smooth(method = "loess", span = .1,se = FALSE) + 
  facet_grid(sitio~temporada, scales = "free_x",labeller=as_labeller(names)) +
  labs(x = "month",
       y = "value",
       color = "variable") +
  scale_x_date(labels = date_format("%b %Y")) +
  theme_light() +
  theme(strip.text = element_text(size = 10))
ggsave(paste0('output/figs/series_clima.png'),scale =3:5)

