source('script/funciones/paquetes.R')

# data_sen1 <- read_rds('data/processed/sentinel_1.rds') |> 
#   distinct(sitio,temporada,fecha,tratamiento,unidad,codigo,.keep_all=T)
# 
# data_sen2_bands <- read_rds('data/processed/sentinel2_bands.rds') |> 
#   distinct() |> 
#   mutate(fecha = as.Date(fecha)) |> 
#   group_by(sitio,temporada,tratamiento,unidad,codigo) |> 
#   complete(fecha = seq.Date(min(fecha), max(fecha), by = "day")) |>
#   mutate(across(B01:B8A, ~ zoo::na.approx(., na.rm = FALSE))) |> 
#   ungroup() |> 
#   mutate(fecha = as.character(fecha)) |> 
#   select(sitio,temporada,fecha,everything())

data_vi <- read_rds('data/processed/sentinel_vi_smooth.rds')
data_biopar <- read_rds('data/processed/sentinel_biopar_smooth.rds')

data_sen2 <- data_vi |> 
  left_join(data_biopar,by=c('sitio','temporada','fecha','tratamiento','unidad','codigo'))

# data_sen2 <- data_sen2_index |> 
#   left_join(data_sen2_bands,
#             by=c('sitio','temporada','fecha','tratamiento','unidad','codigo')) 
#   # left_join(data_sen2_bio_raw,
#   #           by=c('sitio','temporada','fecha','tratamiento','unidad','codigo'))

data_potencial <- read_rds('data/processed/potencial.rds')

data_clima_1 <- read_rds('data/processed/clima_hora.rds') |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(eto = max(eto,na.rm=T),
            pp = sum(pp,na.rm=T)) |> 
  ungroup()

data_clima <- read_rds('data/processed/clima_hora.rds') |> 
  filter(hora %in% c('13:00','14:00')) |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup() |> 
  left_join(data_clima_1,by=c('sitio','temporada','fecha'))
  
data <- data_sen2 |> 
  left_join(data_potencial,by=c('sitio','temporada','fecha','tratamiento','unidad','codigo')) |> 
  left_join(data_clima,by=c('sitio','temporada','fecha')) |> 
  # left_join(data_sen1,by=c('sitio','temporada','fecha','tratamiento','unidad','codigo')) |> 
  select(sitio:codigo,potencial_bar,everything()) |> 
  arrange(temporada,fecha,sitio,tratamiento,unidad)

write_rds(data,'data/processed/modelo_potencial.rds')
