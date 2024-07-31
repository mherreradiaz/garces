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

# Índices smooth

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

data_list_le <- lapply(files_le, function(x) {
  r <- rast(x)
  año <- sub(".*_(\\d{4})\\.tif", "\\1", x)
  id_temporada <- ifelse(año == '2022',1,2)
  vi_name <- tolower(sub(".*SMOOTH_(.*?)_.*\\.tif", "\\1", x))
  
  df <- tibble(terra::extract(r,pol[[id_temporada]][[1]], ID =F)) |> 
    mutate(sitio = 'la_esperanza',
           temporada = temporada[id_temporada],
           unidad = rep(1:3,5),
           codigo = pol[[id_temporada]][[1]]$codigo) |> 
    select(sitio:codigo,everything()) |> 
    pivot_longer(cols = !c(sitio, temporada, unidad, codigo),
                 names_to = "fecha",values_to = 'valor') |>
    mutate(vi = vi_name)
  
  df
})
data_list_rc <- lapply(files_rc, function(x) {
  r <- rast(x)
  año <- sub(".*_(\\d{4})\\.tif", "\\1", x)
  id_temporada <- ifelse(año == '2022',1,2)
  vi_name <- tolower(sub(".*SMOOTH_(.*?)_.*\\.tif", "\\1", x))
  
  df <- tibble(terra::extract(r,pol[[id_temporada]][[2]], ID =F)) |> 
    mutate(sitio = 'rio_claro',
           temporada = temporada[id_temporada],
           unidad = rep(1:3,5),
           codigo = pol[[id_temporada]][[2]]$codigo) |> 
    select(sitio:codigo,everything()) |> 
    pivot_longer(cols = !c(sitio, temporada, unidad, codigo),
                 names_to = "fecha",values_to = 'valor') |>
    mutate(vi = vi_name)
  
  df
})

data_le <-  bind_rows(data_list_le) |> 
  pivot_wider(names_from='vi',values_from='valor')
data_rc <-  bind_rows(data_list_rc) |> 
  pivot_wider(names_from='vi',values_from='valor')

data <- bind_rows(data_le,data_rc) |> 
  as_tibble() |> 
  # na.omit() |>
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
  mutate(unidad = factor(unidad,levels = 1:3)) |> 
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |>
  arrange(sitio,fecha,tratamiento,unidad)

write_rds(data,'data/processed/sentinel_vi_smooth.rds')

# Índices raw

files <- list.files('data/processed/espacial/raster/vi_raw/',full.names=T)

files_le <- grep('la_esperanza',files,value=T)
files_rc <- grep('rio_claro',files,value=T)

sitio <- c('la_esperanza','rio_claro')
temporada <- c('2022-2023','2023-2024')

pol_2022 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2022'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2022'))
pol_2023 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2023'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2023'))
pol <- list(pol_2022,pol_2023)

data_list_le <- lapply(files_le, function(x) {
  r <- rast(x)
  año <- sub(".*_(\\d{4})\\.tif", "\\1", x)
  id_temporada <- ifelse(año == '2022',1,2)
  vi_name <- tolower(sub(".*RAW_(.*?)_.*\\.tif", "\\1", x))
  
  df <- tibble(terra::extract(r,pol[[id_temporada]][[1]], ID =F)) |> 
    mutate(sitio = 'la_esperanza',
           temporada = temporada[id_temporada],
           unidad = rep(1:3,5),
           codigo = pol[[id_temporada]][[1]]$codigo) |> 
    select(sitio:codigo,everything()) |> 
    pivot_longer(cols = !c(sitio, temporada, unidad, codigo),
                 names_to = "fecha",values_to = 'valor') |>
    mutate(vi = vi_name)
  
  df
})
data_list_rc <- lapply(files_rc, function(x) {
  r <- rast(x)
  año <- sub(".*_(\\d{4})\\.tif", "\\1", x)
  id_temporada <- ifelse(año == '2022',1,2)
  vi_name <- tolower(sub(".*RAW_(.*?)_.*\\.tif", "\\1", x))
  
  df <- tibble(terra::extract(r,pol[[id_temporada]][[2]], ID =F)) |> 
    mutate(sitio = 'rio_claro',
           temporada = temporada[id_temporada],
           unidad = rep(1:3,5),
           codigo = pol[[id_temporada]][[2]]$codigo) |> 
    select(sitio:codigo,everything()) |> 
    pivot_longer(cols = !c(sitio, temporada, unidad, codigo),
                 names_to = "fecha",values_to = 'valor') |>
    mutate(vi = vi_name)
  
  df
})

data_le <-  bind_rows(data_list_le) |> 
  pivot_wider(names_from='vi',values_from='valor')
data_rc <-  bind_rows(data_list_rc) |> 
  pivot_wider(names_from='vi',values_from='valor')

data <- bind_rows(data_le,data_rc) |> 
  as_tibble() |> 
  # na.omit() |>
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
  mutate(unidad = factor(unidad,levels = 1:3)) |> 
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |>
  arrange(sitio,fecha,tratamiento,unidad)

write_rds(data,'data/processed/sentinel_vi_raw.rds')

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

# bioparametros smooth

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

data_list_le <- lapply(files_le, function(x) {
  r <- rast(x)
  año <- sub(".*_(\\d{4})\\.tif", "\\1", x)
  id_temporada <- ifelse(año == '2022',1,2)
  biopar_name <- tolower(sub(".*SMOOTH_(.*?)_.*\\.tif", "\\1", x))
  
  df <- tibble(terra::extract(r,pol[[id_temporada]][[1]], ID =F)) |> 
    mutate(sitio = 'la_esperanza',
           temporada = temporada[id_temporada],
           unidad = rep(1:3,5),
           codigo = pol[[id_temporada]][[1]]$codigo) |> 
    select(sitio:codigo,everything()) |> 
    pivot_longer(cols = !c(sitio, temporada, unidad, codigo),
                 names_to = "fecha",values_to = 'valor') |>
    mutate(biopar = biopar_name)
  
  df
})
data_list_rc <- lapply(files_rc, function(x) {
  r <- rast(x)
  año <- sub(".*_(\\d{4})\\.tif", "\\1", x)
  id_temporada <- ifelse(año == '2022',1,2)
  biopar_name <- tolower(sub(".*SMOOTH_(.*?)_.*\\.tif", "\\1", x))
  
  df <- tibble(terra::extract(r,pol[[id_temporada]][[2]], ID =F)) |> 
    mutate(sitio = 'rio_claro',
           temporada = temporada[id_temporada],
           unidad = rep(1:3,5),
           codigo = pol[[id_temporada]][[2]]$codigo) |> 
    select(sitio:codigo,everything()) |> 
    pivot_longer(cols = !c(sitio, temporada, unidad, codigo),
                 names_to = "fecha",values_to = 'valor') |>
    mutate(biopar = biopar_name)
  
  df
})

data_le <-  bind_rows(data_list_le) |> 
  pivot_wider(names_from='biopar',values_from='valor')
data_rc <-  bind_rows(data_list_rc) |> 
  pivot_wider(names_from='biopar',values_from='valor')

data <- bind_rows(data_le,data_rc) |> 
  as_tibble() |> 
  # na.omit() |>
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
  mutate(unidad = factor(unidad,levels = 1:3)) |> 
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |>
  arrange(sitio,fecha,tratamiento,unidad)

write_rds(data,'data/processed/sentinel_biopar_smooth.rds')


# bioparametros raw

files <- list.files('data/processed/espacial/raster/biopar_raw/',full.names=T)

files_le <- grep('la_esperanza',files,value=T)
files_rc <- grep('rio_claro',files,value=T)

sitio <- c('la_esperanza','rio_claro')
temporada <- c('2022-2023','2023-2024')

pol_2022 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2022'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2022'))
pol_2023 <- list(vect('data/processed/espacial/sitios/la_esperanza.gpkg','arboles_mediciones_2023'),
                 vect('data/processed/espacial/sitios/rio_claro.gpkg','arboles_mediciones_2023'))
pol <- list(pol_2022,pol_2023)

data_list_le <- lapply(files_le, function(x) {
  r <- rast(x)
  año <- sub(".*_(\\d{4})\\.tif", "\\1", x)
  id_temporada <- ifelse(año == '2022',1,2)
  biopar_name <- tolower(sub(".*RAW_(.*?)_.*\\.tif", "\\1", x))
  
  df <- tibble(terra::extract(r,pol[[id_temporada]][[1]], ID =F)) |> 
    mutate(sitio = 'la_esperanza',
           temporada = temporada[id_temporada],
           unidad = rep(1:3,5),
           codigo = pol[[id_temporada]][[1]]$codigo) |> 
    select(sitio:codigo,everything()) |> 
    pivot_longer(cols = !c(sitio, temporada, unidad, codigo),
                 names_to = "fecha",values_to = 'valor') |>
    mutate(biopar = biopar_name)
  
  df
})
data_list_rc <- lapply(files_rc, function(x) {
  r <- rast(x)
  año <- sub(".*_(\\d{4})\\.tif", "\\1", x)
  id_temporada <- ifelse(año == '2022',1,2)
  biopar_name <- tolower(sub(".*RAW_(.*?)_.*\\.tif", "\\1", x))
  
  df <- tibble(terra::extract(r,pol[[id_temporada]][[2]], ID =F)) |> 
    mutate(sitio = 'rio_claro',
           temporada = temporada[id_temporada],
           unidad = rep(1:3,5),
           codigo = pol[[id_temporada]][[2]]$codigo) |> 
    select(sitio:codigo,everything()) |> 
    pivot_longer(cols = !c(sitio, temporada, unidad, codigo),
                 names_to = "fecha",values_to = 'valor') |>
    mutate(biopar = biopar_name)
  
  df
})

data_le <-  bind_rows(data_list_le) |> 
  pivot_wider(names_from='biopar',values_from='valor')
data_rc <-  bind_rows(data_list_rc) |> 
  pivot_wider(names_from='biopar',values_from='valor')

data <- bind_rows(data_le,data_rc) |> 
  as_tibble() |> 
  # na.omit() |>
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
  mutate(unidad = factor(unidad,levels = 1:3)) |> 
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |>
  arrange(sitio,fecha,tratamiento,unidad)

write_rds(data,'data/processed/sentinel_biopar_raw.rds')
