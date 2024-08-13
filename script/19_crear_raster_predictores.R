source('script/00_setup.R')

files_vi <- list.files('data/processed/espacial/raster/vi_smooth',full.names=T)
files_biopar <- list.files('data/processed/espacial/raster/biopar_smooth',full.names=T)

# resamplear biopar

r <- list(rast(files_vi[1])[[1]],rast(files_vi[3])[[1]])

vi_list <- lapply(files_vi,rast)
biopar_list <- lapply(files_biopar,function(x) {
  sitio <- str_extract(x, "la_esperanza|rio_claro")
  if (sitio == 'la_esperanza') {r_sitio <- r[[1]]} else {r_sitio <- r[[2]]}
  resample(rast(x),r_sitio)
  })

# guardar tif

fechas_le_2022 <- intersect(names(rast(files_vi[1])),names(rast(files_biopar[1])))
fechas_le_2023 <- intersect(names(rast(files_vi[2])),names(rast(files_biopar[2])))
fechas_rc_2022 <- intersect(names(rast(files_vi[3])),names(rast(files_biopar[3])))
fechas_rc_2023 <- intersect(names(rast(files_vi[4])),names(rast(files_biopar[4])))

name_vi <- str_match(files_vi, ".*/SMOOTH_(.*?)\\.tif$")[,2]
name_biopar <- str_match(files_biopar, ".*/SMOOTH_(.*?)\\.tif$")[,2]

la_esperanza_2022 <- lapply(fechas_le_2022,function(x) {
  vi_x <- rast(lapply(vi_list[which(grepl('la_esperanza_2022',name_vi))],
                      function(y) {y[[x]]}))
  biopar_x <- rast(lapply(biopar_list[which(grepl('la_esperanza_2022',name_biopar))],
                      function(y) {y[[x]]}))
  names(vi_x) <- unique(str_split(name_vi, "_", n = 2, simplify = TRUE)[,1])
  names(biopar_x) <- unique(str_split(name_biopar, "_", n = 2, simplify = TRUE)[,1])
  c(vi_x,biopar_x)
})
la_esperanza_2023 <- lapply(fechas_le_2023,function(x) {
  vi_x <- rast(lapply(vi_list[which(grepl('la_esperanza_2023',name_vi))],
                      function(y) {y[[x]]}))
  biopar_x <- rast(lapply(biopar_list[which(grepl('la_esperanza_2023',name_biopar))],
                          function(y) {y[[x]]}))
  names(vi_x) <- unique(str_split(name_vi, "_", n = 2, simplify = TRUE)[,1])
  names(biopar_x) <- unique(str_split(name_biopar, "_", n = 2, simplify = TRUE)[,1])
  c(vi_x,biopar_x)
})
rio_claro_2022 <- lapply(fechas_rc_2022,function(x) {
  vi_x <- rast(lapply(vi_list[which(grepl('rio_claro_2022',name_vi))],
                      function(y) {y[[x]]}))
  biopar_x <- rast(lapply(biopar_list[which(grepl('rio_claro_2022',name_biopar))],
                          function(y) {y[[x]]}))
  names(vi_x) <- unique(str_split(name_vi, "_", n = 2, simplify = TRUE)[,1])
  names(biopar_x) <- unique(str_split(name_biopar, "_", n = 2, simplify = TRUE)[,1])
  c(vi_x,biopar_x)
})
rio_claro_2023 <- lapply(fechas_rc_2023,function(x) {
  vi_x <- rast(lapply(vi_list[which(grepl('rio_claro_2023',name_vi))],
                      function(y) {y[[x]]}))
  biopar_x <- rast(lapply(biopar_list[which(grepl('rio_claro_2023',name_biopar))],
                          function(y) {y[[x]]}))
  names(vi_x) <- unique(str_split(name_vi, "_", n = 2, simplify = TRUE)[,1])
  names(biopar_x) <- unique(str_split(name_biopar, "_", n = 2, simplify = TRUE)[,1])
  c(vi_x,biopar_x)
})

names(la_esperanza_2022) <- fechas_le_2022
names(la_esperanza_2023) <- fechas_le_2023
names(rio_claro_2022) <- fechas_rc_2022
names(rio_claro_2023) <- fechas_rc_2023

dir.out <- 'data/processed/espacial/raster/predictores/'

for (i in seq_along(la_esperanza_2022)) {
  
  writeRaster(la_esperanza_2022[[i]],
              paste0(dir.out,'SMOOTH_la_esperanza_',gsub('-','',fechas_le_2022[i]),'.tif'),
              overwrite=T)
  writeRaster(la_esperanza_2023[[i]],
              paste0(dir.out,'SMOOTH_la_esperanza_',gsub('-','',fechas_le_2023[i]),'.tif'),
              overwrite=T)
  writeRaster(rio_claro_2022[[i]],
              paste0(dir.out,'SMOOTH_rio_claro_',gsub('-','',fechas_rc_2022[i]),'.tif'),
              overwrite=T)
  if (i <= 231) {
    writeRaster(rio_claro_2023[[i]],
                paste0(dir.out,'SMOOTH_rio_claro_',gsub('-','',fechas_rc_2023[i]),'.tif'),
                overwrite=T)
  }
}

lapply(la_esperanza_2022,function(x) {writeRaster(x,glue('{dir.out}SMOOTH_la_esperanza_{names(x)}.tif'),overwrite=T)})
lapply(la_esperanza_2023,function(x) {writeRaster(x,glue('{dir.out}SMOOTH_la_esperanza_{names(x)}.tif'),overwrite=T)})
lapply(rio_claro_2022,function(x) {writeRaster(x,glue('{dir.out}SMOOTH_rio_claro_{names(x)}.tif'),overwrite=T)})
lapply(rio_claro_2023,function(x) {writeRaster(x,glue('{dir.out}SMOOTH_rio_claro_{names(x)}.tif'),overwrite=T)})
