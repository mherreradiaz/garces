source('script/funciones/paquetes.R')
library(mgcv)
library(parallel)
library(future.apply)

all_files <- sort(list.files('data/processed/espacial/raster/index_raw/',full.names=T))
all_fechas <- gsub('_','-',substr(all_files,nchar(all_files)-13,nchar(all_files)-4))

index_name <- c("ndwi", "ndmi", "msi", "gci", "ndvi", "nbr", "nmdi", "dwsi", "b_i")

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
                                   'vi_smooth/vi_smooth_la_esperanza',fecha_name,'.tif'))
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
                                   'vi_smooth/vi_smooth_la_esperanza',fecha_name,'.tif'))
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
