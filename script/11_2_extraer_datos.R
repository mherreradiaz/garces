source('script/funciones/paquetes.R')

files <- sort(list.files('data/raw/sentinel',full.names=T))

sitio <- c('la_esperanza','rio_claro')
temporada <- c('2022-2023','2023-2024')

pol_2022 <- list(vect('data/processed/espacial/la_esperanza.gpkg','arboles_mediciones_2022'),
            vect('data/processed/espacial/rio_claro.gpkg','arboles_mediciones_2022'))

names(pol_2022[[2]])[4] <- 'codigo'
pol_2022[[1]]$codigo[10:11] <- c('T3H98A8','T3H98A9')

pol_2023 <- list(vect('data/processed/espacial/la_esperanza.gpkg','arboles_mediciones_2023'),
                 vect('data/processed/espacial/rio_claro.gpkg','arboles_mediciones_2023'))

pol_2023[[1]]$codigo[10] <- 'T3H98A8'

pol <- list(pol_2022,pol_2023)

data_list <- lapply(files,function(x) {
  r <- rast(x)
  r_fecha <- as.Date(substr(x,nchar(x)-11,nchar(x)-4),format = '%Y%m%d')
  
  ndwi <- (r[['B03']]-r[['B08']])/(r[['B03']]+r[['B08']]) # Normalized Difference Water Index
  ndmi <- (r[['B08']]-r[['B11']])/(r[['B08']]+r[['B11']]) # Normalized Difference Moisture Index
  msi <- r[['B11']]/r[['B08']] # Moisture Stress Index
  gci <- (r[['B09']]/r[['B03']])-1 # Green Coverage Index
  
  r_index <- c(ndwi,ndmi,msi,gci)
  names(r_index) <- c('ndwi','ndmi','msi','gci')
  
  r <- r[[-c(1,14:18)]]
  
  id_sitio <- ifelse(length(grep('la_esperanza', x))==1,1,2)
  id_tiempo <- ifelse(r_fecha < '2023-06-01',1,2)
  
  df <- data.frame(sitio = sitio[id_sitio],
                   fecha = as.character(r_fecha),
                   temporada = temporada[id_tiempo],
                   unidad = 1:3,
                   codigo = pol[[id_tiempo]][[id_sitio]]$codigo,
                   terra::extract(r,pol[[id_tiempo]][[id_sitio]], ID =F),
                   terra::extract(r_index,pol[[id_tiempo]][[id_sitio]], ID =F))
  
  return(df)
})


data <- bind_rows(data_list) |> 
  as_tibble() |> 
  na.omit() |>
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
  mutate(unidad = factor(unidad,levels = 1:3)) |> 
  select(sitio,temporada,fecha,tratamiento,unidad,codigo,everything())

write_rds(data,'data/processed/sentinel.rds')

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
