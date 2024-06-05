source('script/funciones/paquetes.R')

data_clima <- read_rds('data/processed/clima.rds') |> 
  filter(hora %in% c('13:00','14:00')) |> 
  group_by(sitio,temporada,fecha) |> 
  summarise(t_media = mean(t_media,na.rm=T),
            rh_media = mean(rh_media,na.rm=T),
            vpd_medio = mean(vpd_medio,na.rm=T)) |> 
  ungroup()

names <- data_clima |> 
  mutate(nombre = gsub('-','_',paste('clima',sitio,fecha,sep='_'))) |> 
  pull(nombre)

r <- rast(list.files('data/processed/espacial/raster/index_smooth',full.names=T)[1])[[1]]

for (x in 1:length(names)) {
  
  r_t <- r
  r_vpd <- r
  r_rh <- r
  
  values(r_t) <- data_clima[x,'t_media']
  values(r_vpd) <- data_clima[x,'vpd_medio']
  values(r_rh) <- data_clima[x,'rh_media']
  
  r_x <- c(r_t,r_vpd,r_rh)
  names(r_x) <- c('t_media','vpd_medio','rh_media')
  
  writeRaster(r_x,glue('data/processed/espacial/raster/clima/{names[x]}.tif'))
  
}
