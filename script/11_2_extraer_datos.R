source('script/funciones/paquetes.R')

files <- sort(list.files('data/raw/raster',full.names=T))

sitio <- c('la_esperanza','rio_claro')

pol <- list(vect('data/processed/espacial/la_esperanza.gpkg','arboles_mediciones'),
            vect('data/processed/espacial/rio_claro.gpkg','arboles_mediciones'))

data_list <- lapply(files,function(x) {
  r <- rast(x)
  
  ndwi <- (r[['B03']]-r[['B08']])/(r[['B03']]+r[['B08']]) # Normalized Difference Water Index
  ndmi <- (r[['B08']]-r[['B11']])/(r[['B08']]+r[['B11']]) # Normalized Difference Moisture Index
  msi <- r[['B11']]/r[['B08']] # Moisture Stress Index
  gci <- (r[['B09']]/r[['B03']])-1 # Green Coverage Index
  
  r_index <- c(ndwi,ndmi,msi,gci)
  names(r_index) <- c('ndwi','ndmi','msi','gci')
  
  r <- r[[-c(1,14:18)]]
  
  id <- ifelse(length(grep('la_esperanza', x))==1,1,2)
  
  df <- data.frame(sitio = sitio[id],
                   fecha =  as.character(as.Date(gsub("[^0-9]", "", x),
                                                 format = "%Y%m%d")),
                   unidad = 1:3,
                   codigo = pol[[id]]$codigo,
                   terra::extract(r,pol[[id]], ID =F),
                   terra::extract(r_index,pol[[id]], ID =F))
  
  return(df)
})

data <- bind_rows(data_list) |> 
  as_tibble() |> 
  na.omit() |> 
  mutate(temporada = ifelse(fecha<'2023-06-01','2022-2023','2023-2024'),
         unidad = factor(unidad,levels = 1:3)) |> 
  separate(codigo, into = c('tratamiento','codigo'), sep = 2) |> 
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
