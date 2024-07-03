source('script/funciones/paquetes.R')

# Sentinel 2 bandas

files_le <- sort(list.files('data/raw/sentinel/sentinel_2a_la_esperanza',full.names=T))
files_rc <- sort(list.files('data/raw/sentinel/sentinel_2a_rio_claro',full.names=T))

sitio <- c('la_esperanza','rio_claro')
temporada <- c('2022-2023','2023-2024')

pol_2022 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2022'),
            vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2022'))

pol_2023 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2023'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2023'))


pol <- list(pol_2022,pol_2023)

data_list_le <- lapply(files_le,function(x) {
  r <- rast(x)
  r_fecha <- substr(x,nchar(x)-13,nchar(x)-4)
  
  r <- r[[-c(1,14:18)]]
  
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  
  df <- data.frame(sitio = 'la_esperanza',
                   fecha = r_fecha,
                   temporada = temporada[id_tiempo],
                   unidad = 1:3,
                   codigo = pol[[id_tiempo]][[1]]$codigo,
                   terra::extract(r,pol[[id_tiempo]][[1]], ID =F))
  
  return(df)
})

data_list_rc <- lapply(files_rc,function(x) {
  r <- rast(x)
  r_fecha <- substr(x,nchar(x)-13,nchar(x)-4)
  
  r <- r[[-c(1,14:18)]]
  
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  
  df <- data.frame(sitio = 'rio_claro',
                   fecha = r_fecha,
                   temporada = temporada[id_tiempo],
                   unidad = 1:3,
                   codigo = pol[[id_tiempo]][[2]]$codigo,
                   terra::extract(r,pol[[id_tiempo]][[2]], ID =F))
  
  return(df)
})

data_le <- bind_rows(data_list_le)
data_rc <- bind_rows(data_list_rc)

data <- bind_rows(data_le,data_rc) |> 
  as_tibble() |> 
  na.omit() |>
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
  mutate(unidad = factor(unidad,levels = 1:3)) |> 
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |> 
  arrange(sitio,fecha,tratamiento,unidad)

write_rds(data,'data/processed/sentinel2_bands.rds')

read_rds('data/processed/sentinel2_bands.rds') |> 
  filter(temporada == '2023-2024',
         sitio == 'la_esperanza',
         tratamiento == 'T4',
         unidad == 3) |> 
  ggplot(aes(fecha,B01)) +
  geom_point()

# data |>
#   filter(temporada == '2023-2024',
#          sitio == 'rio_claro') |>
#   mutate(fecha = as.POSIXct(fecha)) |>
#   pivot_longer(cols = c('ndwi','ndmi','msi','gci'), names_to = 'index',values_to = 'value') |>
#   group_by(sitio,temporada,index) |>
#   mutate(value = as.numeric(scale(value))) |>
#   ggplot(aes(fecha,value,group=index, color = index)) +
#   geom_line() +
#   facet_grid(tratamiento~unidad,scales='free')

# Índices

files <- list.files('data/processed/espacial/raster/vi_smooth/',full.names=T)

files_le <- grep('la_esperanza',files,value=T)
files_rc <- grep('rio_claro',files,value=T)

sitio <- c('la_esperanza','rio_claro')
temporada <- c('2022-2023','2023-2024')

pol_2022 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2022'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2022'))

pol_2023 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2023'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2023'))

pol <- list(pol_2022,pol_2023)

data_list_le <- lapply(files_le,function(x) {
  r <- rast(x)
  r_fecha <- gsub('_','-',substr(x,nchar(x)-13,nchar(x)-4))
  
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  
  df <- data.frame(sitio = 'la_esperanza',
                   fecha = r_fecha,
                   temporada = temporada[id_tiempo],
                   unidad = 1:3,
                   codigo = pol[[id_tiempo]][[1]]$codigo,
                   terra::extract(r,pol[[id_tiempo]][[1]], ID =F))
  
  return(df)
})

data_list_rc <- lapply(files_rc,function(x) {
  r <- rast(x)
  r_fecha <- gsub('_','-',substr(x,nchar(x)-13,nchar(x)-4))
  
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  
  df <- data.frame(sitio = 'rio_claro',
                   fecha = r_fecha,
                   temporada = temporada[id_tiempo],
                   unidad = 1:3,
                   codigo = pol[[id_tiempo]][[2]]$codigo,
                   terra::extract(r,pol[[id_tiempo]][[2]], ID =F))
  
  return(df)
})

data_le <- bind_rows(data_list_le)
data_rc <- bind_rows(data_list_rc)

data <- bind_rows(data_le,data_rc) |> 
  as_tibble() |> 
  na.omit() |>
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
  mutate(unidad = factor(unidad,levels = 1:3)) |> 
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |> 
  mutate(fecha = gsub('_','-',fecha)) |> 
  arrange(sitio,fecha,tratamiento,unidad)

write_rds(data,'data/processed/sentinel2_vi_smooth.rds')

# smooth <- read_rds('data/processed/sentinel2_index_smooth.rds') |> 
#   mutate(fecha = as.Date(fecha))
# raw <- read_rds('data/processed/sentinel2_index.rds') |> 
#   mutate(fecha = as.Date(fecha))
# 
# data <- smooth |> 
#   left_join(raw,by=c('sitio','temporada','fecha','tratamiento','unidad','codigo'))
# 
# var <- unique(gsub('\\.x$|\\.y$','',names(data)[7:ncol(data)]))
# 
# data |> 
#   filter(temporada == '2023-2024',
#          unidad == 3) |> 
#   pivot_longer(cols = paste0(var[x],c('.x','.y')),names_to = 'tipo',values_to = 'valor') |> 
#   ggplot(aes(fecha,valor,color=tipo)) +
#   geom_point() +
#   facet_grid(tratamiento~sitio+temporada,scales='free')

# Sentinel 1

files_le <- sort(list.files('data/raw/sentinel/sen_1_la_esperanza',full.names=T))
files_rc <- sort(list.files('data/raw/sentinel/sen_1_rio_claro',full.names=T))

sitio <- c('la_esperanza','rio_claro')
temporada <- c('2022-2023','2023-2024')

pol_2022 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2022'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2022'))

pol_2023 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2023'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2023'))


pol <- list(pol_2022,pol_2023)

data_list_le <- lapply(files_le,function(x) {
  r <- rast(x)
  r_fecha <- substr(x,nchar(x)-13,nchar(x)-4)
  
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  
  df <- data.frame(sitio = 'la_esperanza',
                   fecha = r_fecha,
                   temporada = temporada[id_tiempo],
                   unidad = 1:3,
                   codigo = pol[[id_tiempo]][[1]]$codigo,
                   terra::extract(r,pol[[id_tiempo]][[1]], ID =F))
  
  return(df)
})

data_list_rc <- lapply(files_rc,function(x) {
  r <- rast(x)
  r_fecha <- substr(x,nchar(x)-13,nchar(x)-4)
  
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  
  df <- data.frame(sitio = 'rio_claro',
                   fecha = r_fecha,
                   temporada = temporada[id_tiempo],
                   unidad = 1:3,
                   codigo = pol[[id_tiempo]][[2]]$codigo,
                   terra::extract(r,pol[[id_tiempo]][[2]], ID =F))
  
  return(df)
})

data_le <- bind_rows(data_list_le)
data_rc <- bind_rows(data_list_rc)

data <- bind_rows(data_le,data_rc) |> 
  as_tibble() |> 
  na.omit() |>
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
  mutate(unidad = factor(unidad,levels = 1:3)) |> 
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |> 
  arrange(sitio,fecha,tratamiento,unidad)

write_rds(data,'data/processed/sentinel_1.rds')

# Parametros biofísicos

files <- list.files('data/processed/espacial/raster/biopar_smooth/',full.names=T)

files_le <- grep('la_esperanza',files,value=T)
files_rc <- grep('rio_claro',files,value=T)

sitio <- c('la_esperanza','rio_claro')
temporada <- c('2022-2023','2023-2024')

pol_2022 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2022'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2022'))

pol_2023 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2023'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2023'))

pol <- list(pol_2022,pol_2023)

data_list_le <- lapply(files_le,function(x) {
  r <- rast(x)
  r_fecha <- gsub('_','-',substr(x,nchar(x)-13,nchar(x)-4))
  
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  
  df <- data.frame(sitio = 'la_esperanza',
                   fecha = r_fecha,
                   temporada = temporada[id_tiempo],
                   unidad = 1:3,
                   codigo = pol[[id_tiempo]][[1]]$codigo,
                   terra::extract(r,pol[[id_tiempo]][[1]], ID =F))
  
  return(df)
})

data_list_rc <- lapply(files_rc,function(x) {
  r <- rast(x)
  r_fecha <- gsub('_','-',substr(x,nchar(x)-13,nchar(x)-4))
  
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  
  df <- data.frame(sitio = 'rio_claro',
                   fecha = r_fecha,
                   temporada = temporada[id_tiempo],
                   unidad = 1:3,
                   codigo = pol[[id_tiempo]][[2]]$codigo,
                   terra::extract(r,pol[[id_tiempo]][[2]], ID =F))
  
  return(df)
})

data_le <- bind_rows(data_list_le)
data_rc <- bind_rows(data_list_rc)

data <- bind_rows(data_le,data_rc) |> 
  as_tibble() |> 
  na.omit() |>
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
  mutate(unidad = factor(unidad,levels = 1:3)) |> 
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |> 
  mutate(fecha = gsub('_','-',fecha)) |> 
  arrange(sitio,fecha,tratamiento,unidad)

write_rds(data,'data/processed/sentinel2_biopar_smooth.rds')
