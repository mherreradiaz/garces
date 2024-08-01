library(tidyverse)
library(fs)
library(terra)

files_vi <- dir_ls('data/processed/espacial/raster/vi_smooth')
files_bio <- dir_ls('data/processed/espacial/raster/biopar_smooth')

dates_vi <- str_extract(files_vi,'[0-9]{4}_[0-9]{2}_[0-9]{2}') |> ymd()
dates_bio <- str_extract(files_bio,'[0-9]{4}_[0-9]{2}_[0-9]{2}') |> ymd()

dates_inter <- lubridate::intersect(dates_vi,dates_bio) |> as_date()

files_vi[dates_vi %in% dates_inter] |> length()

files_bio[dates_bio %in% dates_inter] |> length()


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
  left_join(data_clima_1,by=c('sitio','temporada','fecha')) |> 
  mutate(fecha = as.Date(fecha)) |> 
  group_by(sitio,temporada) |> 
  complete(fecha = seq.Date(min(fecha), max(fecha), by = "day")) |>
  mutate(across(t_media:pp, ~ zoo::na.approx(., na.rm = FALSE))) |> 
  ungroup() |> 
  mutate(fecha = as.character(fecha)) |> 
  select(sitio,temporada,fecha,everything())

#utilizamos el modelo sobreajustado para espacializar SWP
modelo <- read_rds('data/processed/modelos/xgboost_over.rds')

# data_clima |> 
#   pivot_longer(cols=c('t_media','vpd_medio','rh_media'),values_to='valor',names_to='var') |> 
#   mutate(fecha=as.Date(fecha)) |> 
#   ggplot(aes(fecha,valor,color=var)) +
#   geom_point() +
#   facet_grid(sitio~temporada,scales='free')

out <- map(files,\(file){
  print(file)
  s2 <- rast(file)
  
  if(!(is.na(values(s2)) |> any())){
    ndwi <- s2['ndwi'] # Normalized Difference Water Index
    ndmi <- s2['ndmi'] # Normalized Difference Moisture Index
    msi <- s2['msi']# Moisture Stress Index
    gci <- s2['gci'] # Green Coverage Index
    ndvi <- s2['ndvi'] #NDVI
    nbr <- s2['nbr']# Normalized Burn Ratio
    b_i <- s2['b_i'] # Índice B11 Y B12
    
    sitio_filt <- str_extract(file,'(?<=smooth_)[a-zA-Z_]+(?=_\\d{4}_\\d{2}_\\d{2})')
    fecha_filt <- gsub('_','-',str_extract(file,'\\d{4}_\\d{2}_\\d{2}'))
    data_clima_x <- data_clima |> 
      filter(sitio == sitio_filt,
             fecha  == fecha_filt)
    
    #crear raster para prediccion
    
    rast_temp <- s2[[1]]
    values(rast_temp) <- data_clima_x$t_media
    rast_rh <- s2[[1]]
    values(rast_rh) <- data_clima_x$rh_media
    rast_vpd <- s2[[1]]
    values(rast_vpd) <- data_clima_x$vpd_medio
    rast_eto <- s2[[1]]
    values(rast_eto) <- data_clima_x$eto
    rast_pp <- s2[[1]]
    values(rast_pp) <- data_clima_x$pp
    
    # "B01"           "B02"           "B03"          
    # [5] "B04"           "B05"           "B06"           "B07"     
    # [9] "B08"           "B09"           "B11"           "B12"     
    # [13] "B8A"           "ndwi"          "ndmi"          "msi"    
    # [17] "gci"           "t_media"       "rh_media"      "vpd_medio"    
    
    predictores <- c(ndwi,ndmi,msi,gci,ndvi,nbr,b_i,
                     rast_temp,rast_rh,rast_vpd,rast_eto,rast_pp)
    names(predictores) <- c('ndwi','ndmi','msi','gci','ndvi','nbr','b_i',
                            't_media','rh_media','vpd_medio','eto','pp')    
    
    #funcion para aplicar el modelo en los rasters 
    fun<-function(...){
      p<-predict(...)
      return(as.matrix(as.numeric(p[, 1, drop=T]))) 
    }
    
    potencial_raster <- 
      terra::predict(predictores,
                     model =modelo,
                     fun=fun)
  } else {
    potencial_raster <- s2[[1]]
    values(potencial_raster) <- NA
  }
  
  names(potencial_raster) <- fecha_filt
  potencial_raster
})

names(out) <- str_extract(names(out),'(?<=smooth_)[^\\.]+(?=\\.tif)')

lapply(names(out), function(name) {
  dir <- paste0('data/processed/espacial/raster/potencial_predict/potencial_xgb_', name, '.tif')
  writeRaster(out[[name]], dir, overwrite=TRUE)
})

out <- subset(out,order(names(out)))
plot(out)

yrmth <- ym(format(ymd(names(out)),'%Y-%m'))

out_mes <- tapp(out,index = yrmth,'mean',na.rm = TRUE)

library(tmap)
library(sf)

st_layers('data/processed/espacial/sitios/la_esperanza.gpkg')
pol <- st_read('data/processed/espacial/sitios/la_esperanza.gpkg', layer = 'borde_cuartel')

namlay <- ymd(str_remove(names(out_mes),'X'))

tmap_mode('plot')

mapa <- tm_shape(out_mes) + 
  tm_raster(style = 'cont',palette = '-RdYlGn',title = 'Potencial (MPa)') +
  tm_shape(pol) +
  tm_borders() +
  tm_facets(nrow = 2,as.layers = TRUE) +
  tm_layout(panel.labels = namlay )

tmap_save(mapa,'output/figs/potencial_raster_estimado_la_esperanza.png')
