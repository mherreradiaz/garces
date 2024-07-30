source('script/funciones/paquetes.R')

files_le <- list.files('data/raw/sentinel/sentinel_2a_la_esperanza',full.names=T)
files_rc <- list.files('data/raw/sentinel/sentinel_2a_rio_claro',full.names=T)

fechas_le <- gsub('_','-',substr(files_le,nchar(files_le)-13,nchar(files_le)-4))
fechas_rc <- gsub('_','-',substr(files_rc,nchar(files_rc)-13,nchar(files_rc)-4))

band_name <- names(rast(files_le[1]))[2:12]

r_le_2022 <- rast(files_le[which(fechas_le < '2023-06-01')])
r_rc_2022 <- rast(files_rc[which(fechas_rc < '2023-06-01')])
r_le_2023 <- rast(files_le[which(fechas_le > '2023-06-01')])
r_rc_2023 <- rast(files_rc[which(fechas_rc > '2023-06-01')])

le_2022 <- lapply(band_name, function(x) {
  band <- subset(r_le_2022,which(names(r_le_2022)==x))
  names(band) <- substr(sources(band),nchar(sources(band))-13,
                        nchar(sources(band))-4)
  band[[sort(names(band))]]
})
rc_2022 <- lapply(band_name, function(x) {
  band <- subset(r_rc_2022,which(names(r_rc_2022)==x))
  names(band) <- substr(sources(band),nchar(sources(band))-13,
                        nchar(sources(band))-4)
  band[[sort(names(band))]]
})
le_2023 <- lapply(band_name, function(x) {
  band <- subset(r_le_2023,which(names(r_le_2023)==x))
  names(band) <- substr(sources(band),nchar(sources(band))-13,
                        nchar(sources(band))-4)
  band[[sort(names(band))]]
})
rc_2023 <- lapply(band_name, function(x) {
  band <- subset(r_rc_2023,which(names(r_rc_2023)==x))
  names(band) <- substr(sources(band),nchar(sources(band))-13,
                        nchar(sources(band))-4)
  band[[sort(names(band))]]
})

names(le_2022) <- as.numeric(gsub('B','',band_name))
names(rc_2022) <- as.numeric(gsub('B','',band_name))
names(le_2023) <- as.numeric(gsub('B','',band_name))
names(rc_2023) <- as.numeric(gsub('B','',band_name))
vi <- list(le_2022,rc_2022,
           le_2023,rc_2023)

dir.out <- 'data/processed/espacial/raster/vi_raw/'
sitio_name <- c('la_esperanza_2022','rio_claro_2022',
                'la_esperanza_2023','rio_claro_2023')

for (i in seq_along(sitio_name)) {
  
  b <- vi[[i]]
  
  writeRaster((b$`8`-b$`4`)/(b$`8`+b$`4`),glue('{dir.out}RAW_NDVI_{sitio_name[i]}.tif'))
  writeRaster(2.5*(b$`8`-b$`4`)/(b$`8`+6*b$`4`-7.5*b$`2`+1),glue('{dir.out}RAW_EVI_{sitio_name[i]}.tif'))
  writeRaster(b$`9`/b$`3`-1,glue('{dir.out}RAW_GCI_{sitio_name[i]}.tif'))
  writeRaster((b$`8`-b$`12`)/(b$`8`+b$`12`),glue('{dir.out}RAW_NBR_{sitio_name[i]}.tif'))
  writeRaster((b$`3`-b$`8`)/(b$`3`+b$`8`),glue('{dir.out}RAW_NDWI_{sitio_name[i]}.tif'))
  writeRaster((b$`8`-b$`11`)/(b$`8`+b$`11`),glue('{dir.out}RAW_NDMI_{sitio_name[i]}.tif'))
  writeRaster(b$`11`/b$`8`,glue('{dir.out}RAW_MSI_{sitio_name[i]}.tif'))
  writeRaster((b$`8`-b$`11`+b$`12`)/(b$`8`+b$`11`-b$`12`),glue('{dir.out}RAW_NMDI_{sitio_name[i]}.tif'))
  writeRaster((b$`8`+b$`3`)/(b$`11`+b$`4`),glue('{dir.out}RAW_DWSI_{sitio_name[i]}.tif'))
  writeRaster(b$`7`/b$`5`-1,glue('{dir.out}RAW_CIr_{sitio_name[i]}.tif'))
  writeRaster(b$`7`/b$`3`-1,glue('{dir.out}RAW_CIg_{sitio_name[i]}.tif'))
  writeRaster((b$`6`-b$`5`)/(b$`6`+b$`5`),glue('{dir.out}RAW_NDRE1_{sitio_name[i]}.tif'))
  writeRaster((b$`8`-b$`5`)/(b$`8`+b$`5`),glue('{dir.out}RAW_NDRE2_{sitio_name[i]}.tif'))
  writeRaster((b$`5`-b$`4`)/(b$`5`+b$`4`),glue('{dir.out}RAW_NDCI_{sitio_name[i]}.tif'))
  writeRaster(((b$`6`/b$`5`)-1)/sqrt((b$`6`/b$`5`)+1),glue('{dir.out}RAW_mSR705_{sitio_name[i]}.tif'))
  writeRaster((b$`7`+b$`6`-b$`5`)/(b$`7`+b$`6`+b$`5`),glue('{dir.out}RAW_RESI_{sitio_name[i]}.tif'))
  
}

View(rast('data/processed/espacial/raster/vi_raw/RAW_NDVI_rio_claro_2022.tif'))

rast(glue('{dir.out}SMOOTH_MSI_{sitio_name[i]}.tif'))[2000]
