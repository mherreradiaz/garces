library(tidyverse)
library(fs)
library(terra)

files <- dir_ls('data/raw/sentinel/rio_claro')

s2 <- rast(files[1])[[-c(1,14,15,16:18)]]
names(s2)

out <- map(files,\(file){
  print(file)
  s2 <- rast(file)[[-c(1,14,15,16:18)]]
  
  if(!(is.na(values(s2)) |> any())){
    ndwi <- app(s2,\(x) (x['B03']-x['B08'])/(x['B03']+x['B08'])) # Normalized Difference Water Index
    ndmi <- app(s2,\(x) (x['B08']-x['B11'])/(x['B08']+x['B11'])) # Normalized Difference Moisture Index
    msi <- app(s2,\(x) x['B11']/x['B08']) # Moisture Stress Index
    gci <- app(s2,\(x) (x['B09']/x['B03'])-1) # Green Coverage Index
    
    fecha_filt <- str_extract(file,'[0-9]{4}-[0-9]{2}-[0-9]{2}')
    data_clima <- read_rds('data/processed/clima.rds') |> 
      group_by(fecha) |> 
      summarize(t_media = mean(t_media,na.rm = TRUE),
                rh_media = mean(rh_media,na.rm = TRUE),
                vpd_medio = mean(vpd_medio,na.rm = TRUE)) |> 
      filter(fecha  == fecha_filt)
    
    #crear raster para prediccion
    
    rast_temp <- s2[[1]]
    values(rast_temp) <- data_clima$t_media
    rast_rh <- s2[[1]]
    values(rast_rh) <- data_clima$rh_media
    rast_vpd <- s2[[1]]
    values(rast_vpd) <- data_clima$vpd_medio
    
    # "B01"           "B02"           "B03"          
    # [5] "B04"           "B05"           "B06"           "B07"     
    # [9] "B08"           "B09"           "B11"           "B12"     
    # [13] "B8A"           "ndwi"          "ndmi"          "msi"    
    # [17] "gci"           "t_media"       "rh_media"      "vpd_medio"    
    
    predictores <- c(s2,ndwi,ndmi,msi,gci,rast_temp,rast_rh,rast_vpd)
    names(predictores) <- c( "B01","B02","B03","B04","B05","B06","B07","B08","B09","B11","B12","B8A","ndwi","ndmi","msi","gci","t_media","rh_media","vpd_medio")    
     
    #funcion para aplicar el modelo en los rasters 
    fun<-function(...){
      p<-predict(...)
      return(as.matrix(as.numeric(p[, 1, drop=T]))) 
    }
    
    potencial_raster <- 
      terra::predict(predictores,
                     model =rf_mod,
                     fun=fun)
  } else {
    potencial_raster <- s2[[1]]
    values(potencial_raster) <- NA
  }
  potencial_raster
})

out <- rast(out)
names(out) <- str_extract(names(out),'[0-9]{4}-[0-9]{2}-[0-9]{2}')

out <- subset(out,order(names(out)))
plot(out)

yrmth <- ym(format(ymd(names(out)),'%Y-%m'))

out_mes <- tapp(out,index = yrmth,'mean',na.rm = TRUE)


library(tmap)
library(sf)

st_layers('data/processed/espacial/rio_claro.gpkg')
pol <- st_read('data/processed/espacial/rio_claro.gpkg', layer = 'borde_cuartel')

namlay <- ymd(str_remove(names(out_mes),'X'))

tmap_mode('plot')

mapa <- tm_shape(out_mes) + 
  tm_raster(style = 'cont',palette = '-RdYlGn',title = 'Potencial (MPa)') +
  tm_shape(pol) +
  tm_borders() +
  tm_facets(nrow = 2,as.layers = TRUE) +
  tm_layout(panel.labels = namlay )

tmap_save(mapa,'output/figs/potencial_raster_estimado_rio_claro.png')
