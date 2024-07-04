library(fs)
library(terra)
library(glue)

dir_scl <- 'data/processed/espacial/raster/scl/'

sitio <- 'rio_claro'
files_scl <- dir_ls(dir_scl,regexp = glue('{sitio}.*tif$'))

vars <- c('lai','lai_cab','lai_cw','fapar','fcover')

lapply(vars,\(var){
  dir_bio <- glue('data/processed/espacial/raster/biophysical/{var}')
  files_bio <- dir_ls(dir_bio,regexp = glue('{sitio}.*tif$'))
  
  dates_scl <- str_extract(files_scl,'[0-9]{8}')
  dates_bio <- str_extract(files_bio,'[0-9]{8}')
  
  dates_scl == dates_bio
  
  #aplicar mascara
  #
  lapply(seq_along(files_bio),\(i){
    bio <- rast(files_bio[i])
    scl <- rast(files_scl[i])
    scl_mask <- scl 
    scl_mask[scl_mask == 3] <- NA
    bio_mask <- mask(bio,scl_mask)
    writeRaster(bio_mask,files_bio[i],overwrite = TRUE)
  })
})