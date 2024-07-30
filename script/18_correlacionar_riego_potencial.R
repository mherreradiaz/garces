source('script/00_setup.R')

files <- list.files('data/processed/espacial/raster/potencial_predict/',full.names=T) 
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
  r_fecha <- names(r)
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  names(r) <- 'potencial'
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
  r_fecha <- names(r)
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  names(r) <- 'potencial'
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

potencial <- bind_rows(data_le,data_rc) |> 
  as_tibble() |> 
  na.omit() |>
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
  mutate(unidad = factor(unidad,levels = 1:3)) |> 
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything()) |> 
  mutate(fecha = gsub('_','-',fecha)) |> 
  arrange(sitio,fecha,tratamiento,unidad)

riego <- read_rds('data/processed/riego.rds')

data <- potencial |> 
  mutate(fecha=as.Date(fecha)) |> 
  left_join(riego,by=c('sitio','temporada','fecha','tratamiento')) |> 
  mutate(id = paste(sitio,temporada,tratamiento,codigo))

# hacer un ciclo por cada unique(data$id) en donde primero se borren todas las filas
# con NA al principio (antes del valor de riego), meterle el desfase (quizás mejor antes)
# y hacer el lead de chatgpt






data |> 
  arrange(sitio,temporada,codigo,fecha) |> 
  filter(sitiofecha>='2022-10-11')

data |> na.omit() |> 
  group_by(sitio,temporada) |> 
  summarise(min(fecha))
