source('script/funciones/paquetes.R')
library(mgcv)
library(parallel)

all_files <- sort(list.files('data/processed/espacial/raster/vi_raw/',full.names=T))
all_fechas <- gsub('_','-',substr(all_files,nchar(all_files)-13,nchar(all_files)-4))

index_name <- c('ndwi','ndmi','msi','gci','ndvi','nbr','nmdi','dwsi','ndvi_705','ndci','b_i')

# La Esperanza 2022-2023

files <- grep('la_esperanza',all_files[all_fechas < '2023-06-01'],value=T)
fechas <- as.Date(gsub('_','-',substr(files,nchar(files)-13,nchar(files)-4)))

r <- rast(files)

index_r <- lapply(index_name, function(x) r[x])

suavizado <- list()
fechas_completas <- seq(min(fechas), max(fechas), by = "day")

for (i in seq_along(index_r)) {
  
  index <- index_r[[i]]
  
  suavizado[[i]] <- app(index,\(y){
    dias <- as.numeric(fechas)
    data <- data.frame(x=dias,y=y)
    model <- loess(y ~ x, data = data,span = 0.5)
    new_data <- data.frame(x=seq(min(dias),max(dias)))
    predict(model,new_data) |> as.numeric()
  })
  
  print(index_name[i])
  
  names(suavizado[[i]]) <- fechas_completas
  
}

names(suavizado) <- index_name

for (x in seq_along(fechas_completas)) {
  index_r <- list()
  for (y in seq_along(suavizado)) {
    index_r[[y]] <- suavizado[[y]][[x]]
    names(index_r[[y]]) <- index_name[y]
  }
  
  fecha_name <- gsub('-','_',fechas_completas[x])
  writeRaster(rast(index_r),paste0('data/processed/espacial/raster/',
                                   'vi_smooth/vi_smooth_la_esperanza_',fecha_name,'.tif'))
}

# La Esperanza 2023-2024

files <- grep('la_esperanza',all_files[all_fechas > '2023-06-01'],value=T)
fechas <- as.Date(gsub('_','-',substr(files,nchar(files)-13,nchar(files)-4)))

r <- rast(files)

index_r <- lapply(index_name, function(x) r[x])

suavizado <- list()
fechas_completas <- seq(min(fechas), max(fechas), by = "day")

for (i in seq_along(index_r)) {
  
  index <- index_r[[i]]
  
  suavizado[[i]] <- app(index,\(y){
    dias <- as.numeric(fechas)
    data <- data.frame(x=dias,y=y)
    model <- gam(y ~ s(x), data = data,method = "REML")
    new_data <- data.frame(x=seq(min(dias),max(dias)))
    predict(model,new_data) |> as.numeric()
  })
  
  names(suavizado[[i]]) <- fechas_completas
  
}

names(suavizado) <- index_name

for (x in seq_along(fechas_completas)) {
  index_r <- list()
  for (y in seq_along(suavizado)) {
    index_r[[y]] <- suavizado[[y]][[x]]
    names(index_r[[y]]) <- index_name[y]
  }
  
  fecha_name <- gsub('-','_',fechas_completas[x])
  writeRaster(rast(index_r),paste0('data/processed/espacial/raster/',
                                   'vi_smooth/vi_smooth_la_esperanza_',fecha_name,'.tif'))
}

# Rio Claro 2022-2023

files <- grep('rio_claro',all_files[all_fechas < '2023-06-01'],value=T)
fechas <- as.Date(gsub('_','-',substr(files,nchar(files)-13,nchar(files)-4)))

r <- rast(files)

index_r <- lapply(index_name, function(x) r[x])

suavizado <- list()
fechas_completas <- seq(min(fechas), max(fechas), by = "day")

for (i in seq_along(index_r)) {
  
  index <- index_r[[i]]
  
  suavizado[[i]] <- app(index,\(y){
    dias <- as.numeric(fechas)
    data <- data.frame(x=dias,y=y)
    model <- gam(y ~ s(x), data = data,method = "REML")
    new_data <- data.frame(x=seq(min(dias),max(dias)))
    predict(model,new_data) |> as.numeric()
  })
  
  names(suavizado[[i]]) <- fechas_completas
  
}

names(suavizado) <- index_name

for (x in seq_along(fechas_completas)) {
  index_r <- list()
  for (y in seq_along(suavizado)) {
    index_r[[y]] <- suavizado[[y]][[x]]
    names(index_r[[y]]) <- index_name[y]
  }
  
  fecha_name <- gsub('-','_',fechas_completas[x])
  writeRaster(rast(index_r),paste0('data/processed/espacial/raster/',
                                   'vi_smooth/vi_smooth_rio_claro_',fecha_name,'.tif'))
}

# Rio Claro 2023-2024

files <- grep('rio_claro',all_files[all_fechas > '2023-06-01'],value=T)
fechas <- as.Date(gsub('_','-',substr(files,nchar(files)-13,nchar(files)-4)))

r <- rast(files)

index_r <- lapply(index_name, function(x) r[x])

suavizado <- list()
fechas_completas <- seq(min(fechas), max(fechas), by = "day")

for (i in seq_along(index_r)) {
  
  index <- index_r[[i]]
  
  suavizado[[i]] <- app(index,\(y){
    dias <- as.numeric(fechas)
    data <- data.frame(x=dias,y=y)
    model <- gam(y ~ s(x), data = data,method = "REML")
    new_data <- data.frame(x=seq(min(dias),max(dias)))
    predict(model,new_data) |> as.numeric()
  })
  
  names(suavizado[[i]]) <- fechas_completas
  
}

names(suavizado) <- index_name

for (x in seq_along(fechas_completas)) {
  index_r <- list()
  for (y in seq_along(suavizado)) {
    index_r[[y]] <- suavizado[[y]][[x]]
    names(index_r[[y]]) <- index_name[y]
  }
  
  fecha_name <- gsub('-','_',fechas_completas[x])
  writeRaster(rast(index_r),paste0('data/processed/espacial/raster/',
                                   'vi_smooth/vi_smooth_rio_claro_',fecha_name,'.tif'))
}

# parametros biofísicos

fapar_files <- list.files('data/processed/espacial/raster/biophysical/fapar/',full.names=T)
fcover_files <- list.files('data/processed/espacial/raster/biophysical/fcover/',full.names=T)
lai_files <- list.files('data/processed/espacial/raster/biophysical/lai/',full.names=T)
cab_files <- list.files('data/processed/espacial/raster/biophysical/lai_cab/',full.names=T)
cw_files <- list.files('data/processed/espacial/raster/biophysical/lai_cw/',full.names=T)

fecha <- str_extract(fapar_files,"\\d{8}")
sitio <- str_extract(fapar_files,"(rio_claro|la_esperanza)")

for (i in seq_along(fapar_files)) {
  
  r <- c(
    rast(fapar_files[i]),
    rast(fcover_files[i]),
    rast(lai_files[i]),
    rast(cab_files[i]),
    rast(cw_files[i])
  )
  
  writeRaster(r,paste0('data/processed/espacial/raster/biopar_raw/bio_',
                       sitio[i],'_',fecha[i],'.tif'))
  
}


#

all_files <- sort(list.files('data/processed/espacial/raster/biopar_raw/',full.names=T))
all_fechas <- substr(all_files,nchar(all_files)-11,nchar(all_files)-4)
all_fechas <- gsub('_','-',paste0(substr(all_fechas, 1, 4), "_", substr(all_fechas, 5, 6), "_", substr(all_fechas, 7, 8)))

index_name <- c('fapar','fcover','lai','lai_cab','lai_cw')

# La Esperanza 2022-2023

files <- grep('la_esperanza',all_files[all_fechas < '2023-06-01'],value=T)
fechas <- substr(files,nchar(files)-11,nchar(files)-4)
fechas <- as.Date(gsub('_','-',paste0(substr(fechas, 1, 4), "_", 
                                      substr(fechas, 5, 6), "_", 
                                      substr(fechas, 7, 8))))

r <- rast(files)

index_r <- lapply(index_name, function(x) subset(r,which(names(r)==x)))

suavizado <- list()
fechas_completas <- seq(min(fechas), max(fechas), by = "day")

for (i in seq_along(index_r)) {
  
  index <- index_r[[i]]
  
  suavizado[[i]] <- app(index,\(y){
    dias <- as.numeric(fechas)
    data <- data.frame(x=dias,y=y)
    model <- gam(y ~ s(x), data = data,method = "REML")
    # model <- loess(y ~ x, data = data, span = 7)
    new_data <- data.frame(x=seq(min(dias),max(dias)))
    predict(model,new_data) |> as.numeric()
  })
  
  names(suavizado[[i]]) <- fechas_completas
  
}

#

index <- index_r[[i]]

# sp = 7/length(fechas)

suavizado <- app(index,\(y){
  
  hampel(y,k=1)
  
  
  
  dias <- as.numeric(fechas)
  data <- data.frame(x=dias,y=y)
  # model <- gam(y ~ s(x), data = data,method = "REML")
  # model <- loess(y ~ x, data = data, span = sp)
  model <- gam(y ~ s(x, k = 10), data = data)
  new_data <- data.frame(x=seq(min(dias),max(dias)))
  predict(model,new_data) |> as.numeric()
})

names(suavizado) <- fechas_completas

# px <- sample(nrow(values(index)),1)
px = 1089
# px = 126

tibble(fecha = as.Date(names(suavizado)), 
       biopar_smooth = as.numeric(suavizado[px])) |> 
  left_join(tibble(fecha = fechas,
                   biopar_raw = as.numeric(index[px])),
            by = 'fecha') |> 
  mutate(temporada = ifelse(fecha < '2023-06-02','2022-2023','2023-2024')) |> 
  pivot_longer(cols=c('biopar_smooth','biopar_raw'),names_to ='origin',values_to='value') |> 
  ggplot(aes(fecha,value,color=origin)) +
  geom_point() +
  labs(y = index_name[i]) +
  facet_grid(~temporada,scales='free_x')



#





names(suavizado) <- index_name

for (x in seq_along(fechas_completas)) {
  index_r <- list()
  for (y in seq_along(suavizado)) {
    index_r[[y]] <- suavizado[[y]][[x]]
    names(index_r[[y]]) <- index_name[y]
  }
  
  fecha_name <- gsub('-','_',fechas_completas[x])
  writeRaster(rast(index_r),paste0('data/processed/espacial/raster/',
                                   'biopar_smooth/biopar_smooth_la_esperanza_',fecha_name,'.tif'))
}

# La Esperanza 2023-2024

files <- grep('la_esperanza',all_files[all_fechas > '2023-06-01'],value=T)
fechas <- substr(files,nchar(files)-11,nchar(files)-4)
fechas <- as.Date(gsub('_','-',paste0(substr(fechas, 1, 4), "_", 
                                      substr(fechas, 5, 6), "_", 
                                      substr(fechas, 7, 8))))

r <- rast(files)

index_r <- lapply(index_name, function(x) subset(r,which(names(r)==x)))

suavizado <- list()
fechas_completas <- seq(min(fechas), max(fechas), by = "day")

for (i in seq_along(index_r)) {
  
  index <- index_r[[i]]
  
  suavizado[[i]] <- app(index,\(y){
    dias <- as.numeric(fechas)
    data <- data.frame(x=dias,y=y)
    model <- gam(y ~ s(x), data = data,method = "REML")
    new_data <- data.frame(x=seq(min(dias),max(dias)))
    predict(model,new_data) |> as.numeric()
  })
  
  names(suavizado[[i]]) <- fechas_completas
  
}

names(suavizado) <- index_name

for (x in seq_along(fechas_completas)) {
  index_r <- list()
  for (y in seq_along(suavizado)) {
    index_r[[y]] <- suavizado[[y]][[x]]
    names(index_r[[y]]) <- index_name[y]
  }
  
  fecha_name <- gsub('-','_',fechas_completas[x])
  writeRaster(rast(index_r),paste0('data/processed/espacial/raster/',
                                   'biopar_smooth/biopar_smooth_la_esperanza_',fecha_name,'.tif'))
}

# Rio Claro 2022-2023

files <- grep('rio_claro',all_files[all_fechas < '2023-06-01'],value=T)
fechas <- substr(files,nchar(files)-11,nchar(files)-4)
fechas <- as.Date(gsub('_','-',paste0(substr(fechas, 1, 4), "_", 
                                      substr(fechas, 5, 6), "_", 
                                      substr(fechas, 7, 8))))

r <- rast(files)

index_r <- lapply(index_name, function(x) subset(r,which(names(r)==x)))

suavizado <- list()
fechas_completas <- seq(min(fechas), max(fechas), by = "day")

for (i in seq_along(index_r)) {
  
  index <- index_r[[i]]
  
  suavizado[[i]] <- app(index,\(y){
    dias <- as.numeric(fechas)
    data <- data.frame(x=dias,y=y)
    model <- gam(y ~ s(x), data = data,method = "REML")
    new_data <- data.frame(x=seq(min(dias),max(dias)))
    predict(model,new_data) |> as.numeric()
  })
  
  names(suavizado[[i]]) <- fechas_completas
  
}

names(suavizado) <- index_name

for (x in seq_along(fechas_completas)) {
  index_r <- list()
  for (y in seq_along(suavizado)) {
    index_r[[y]] <- suavizado[[y]][[x]]
    names(index_r[[y]]) <- index_name[y]
  }
  
  fecha_name <- gsub('-','_',fechas_completas[x])
  writeRaster(rast(index_r),paste0('data/processed/espacial/raster/',
                                   'biopar_smooth/biopar_smooth_rio_claro_',fecha_name,'.tif'))
}

# Rio Claro 2023-2024

files <- grep('rio_claro',all_files[all_fechas > '2023-06-01'],value=T)
fechas <- substr(files,nchar(files)-11,nchar(files)-4)
fechas <- as.Date(gsub('_','-',paste0(substr(fechas, 1, 4), "_", 
                                      substr(fechas, 5, 6), "_", 
                                      substr(fechas, 7, 8))))

r <- rast(files)

index_r <- lapply(index_name, function(x) subset(r,which(names(r)==x)))

suavizado <- list()
fechas_completas <- seq(min(fechas), max(fechas), by = "day")

for (i in seq_along(index_r)) {
  
  index <- index_r[[i]]
  
  suavizado[[i]] <- app(index,\(y){
    dias <- as.numeric(fechas)
    data <- data.frame(x=dias,y=y)
    model <- gam(y ~ s(x), data = data,method = "REML")
    new_data <- data.frame(x=seq(min(dias),max(dias)))
    predict(model,new_data) |> as.numeric()
  })
  
  names(suavizado[[i]]) <- fechas_completas
  
}

names(suavizado) <- index_name

for (x in seq_along(fechas_completas)) {
  index_r <- list()
  for (y in seq_along(suavizado)) {
    index_r[[y]] <- suavizado[[y]][[x]]
    names(index_r[[y]]) <- index_name[y]
  }
  
  fecha_name <- gsub('-','_',fechas_completas[x])
  writeRaster(rast(index_r),paste0('data/processed/espacial/raster/',
                                   'biopar_smooth/biopar_smooth_rio_claro_',fecha_name,'.tif'))
}

