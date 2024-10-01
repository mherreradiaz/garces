source('script/00_setup.R')

files <- grep('NDVI',list.files('data/processed/espacial/raster/vi_smooth',full.names=T),value=T)

kc <- lapply(files,function (x) {
  1.44*rast(x)+.1
})

eto <- read_rds('data/processed/clima_dia.rds') |> 
  select(sitio:fecha,eto)

sitio_grupo <- c(rep('la_esperanza',2),rep('rio_claro',2))
temporada_grupo <- c(rep(c('2022-2023','2023-2024'),2))

for (i in 1:4) {
  
  eto_grupo <- eto |> 
    filter(sitio == sitio_grupo[i],
           temporada == temporada_grupo[i])
  
  r <- kc[[i]]
  fechas_comunes <- intersect(names(r),eto_grupo$fecha)
  
  riego <- r[[which(names(r) %in% fechas_comunes)]]*
    pull(eto_grupo,eto)[which(eto_grupo$fecha %in% fechas_comunes)]
  
  writeRaster(riego,glue('data/processed/espacial/raster/ETc/ETc_{sitio_grupo[[i]]}_{substr(temporada_grupo[[i]],1,4)}.tif'))
  
}

