source('script/funciones/paquetes.R')

data_sentinel_1 <- read_rds('data/processed/sentinel_1.rds') |> 
  distinct() |> 
  mutate(fecha = as.Date(fecha)) |> 
  group_by(sitio,temporada,tratamiento,unidad,codigo) |> 
  complete(fecha = seq.Date(min(fecha), max(fecha), by = "day")) |>
  mutate(across(vh:vv, ~ zoo::na.approx(., na.rm = FALSE))) |> 
  ungroup() |> 
  mutate(fecha = as.character(fecha)) |> 
  select(sitio,temporada,fecha,everything())

# data_sentinel_2 |> 
#   filter(sitio == 'la_esperanza',
#          temporada == '2022-2023',
#          tratamiento == 'T1',
#          unidad == 1) |> 
#   ggplot(aes(fecha,B01)) +
#   geom_point()

data_sentinel_2 <- read_rds('data/processed/sentinel2_bands.rds') |> 
  distinct() |> 
  mutate(fecha = as.Date(fecha)) |> 
  group_by(sitio,temporada,tratamiento,unidad,codigo) |> 
  complete(fecha = seq.Date(min(fecha), max(fecha), by = "day")) |>
  mutate(across(B01:B8A, ~ zoo::na.approx(., na.rm = FALSE))) |> 
  ungroup() |> 
  mutate(fecha = as.character(fecha)) |> 
  select(sitio,temporada,fecha,everything())

data_sentinel_index <- read_rds('data/processed/sentinel2_index_smooth.rds') |> 
  distinct()

data_sentinel <- data_sentinel_index |> 
  left_join(data_sentinel_2,
            by=c('sitio','temporada','fecha','tratamiento','unidad','codigo')) |> 
  distinct(sitio,fecha,tratamiento,unidad,codigo,.keep_all=T)

data_potencial <- read_rds('data/processed/potencial.rds')
data_clima <- read_rds('data/processed/clima.rds') |> 
  filter(hora %in% c('13:00','14:00')) |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup()
  
data <- data_sentinel |> 
  left_join(data_potencial,by=c('sitio','temporada','fecha','tratamiento','unidad','codigo')) |> 
  left_join(data_clima,by=c('sitio','temporada','fecha')) |> 
  na.omit() |> 
  select(sitio:codigo,potencial_bar,everything()) |> 
  arrange(temporada,fecha,sitio,tratamiento,unidad)

write_rds(data,'data/processed/modelo_potencial.rds')
