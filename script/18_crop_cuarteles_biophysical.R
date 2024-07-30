library(fs)
library(terra)
library(sf)
library(tidyverse)
library(glue)

dir <- '/mnt/md0/raster_procesada/Sentinel2/Bio'

files <- dir_ls(dir,recurse = TRUE,regexp = '(fcover|lai|fapar).*img$')

var <- 'fcover'
sitio <- 'rio_claro'
tiles <- c('la_esperanza'='HBC','rio_claro'='HCB')
tile <- tiles[sitio]
files_sel <- files[str_detect(files,glue('T19{tile}.*{var}.img'))]

names <- str_extract(files_sel,'T19.*_[0-9]{8}')
pol <- st_read(glue('data/processed/espacial/sitios/{sitio}.gpkg'),'cuartel')

lapply(seq_along(files_sel),\(i){
  im <- rast(files_sel[i])
  pol <- pol |> st_transform(crs(im))
  im <- crop(im,pol)
  writeRaster(im,glue('data/processed/espacial/raster/biophysical/{var}/{names[i]}_{sitio}_{var}.tif'))
})

#cortar SCL
#
dir <- '/mnt/md0/raster_procesada/Sentinel2/GTIFF/SCL'
files <- dir_ls(dir,regexp = 'tif$')
sitio <- 'rio_claro'
tiles <- c('la_esperanza'='HBC','rio_claro'='HCB')
tile <- tiles[sitio]
files_sel <- files[str_detect(files,glue('19{tile}.*tif$'))]

names <- str_extract(files_sel,'[0-9]{8}')

scl <- rast(files_sel)
pol <- st_read(glue('data/processed/espacial/sitios/{sitio}.gpkg'),'cuartel') |> 
  st_transform(crs(scl))
scl_crop <- crop(scl,pol)

seq_along(files_sel) |> 
  lapply(\(i){
    writeRaster(scl_crop[[i]],glue('data/processed/espacial/raster/scl/scl_{names[i]}_{sitio}.tif'),overwrite = TRUE)
  })
