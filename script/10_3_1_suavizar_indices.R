source('script/funciones/paquetes.R')
library(mgcv)
library(parallel)

all_files <- sort(list.files('data/processed/espacial/raster/vi_raw/',full.names=T))
all_fechas <- gsub('_','-',substr(all_files,nchar(all_files)-13,nchar(all_files)-4))

fit_gam_model <- function(var_name,data_frame,i,dias) {
  var_data <- data_frame[i,grep(paste0("^",var_name,"$"),colnames(data_frame))]
  var_data <- data.frame(dias = dias,var_value = var_data)  
  colnames(var_data)[2] <- var_name 
  formula <- as.formula(paste(var_name,"~ s(dias)"))
  model <- gam(formula,data = var_data,method = "REML")
  return(model)
}

variables <- c('ndwi','ndmi','msi','gci','ndvi','nbr','nmdi','dwsi','ndvi_705','b_i')

### La Esperanza Temporada 2022-2023

files <- grep('la_esperanza',all_files[all_fechas < '2023-06-01'],value=T)
fechas <- gsub('_','-',substr(files,nchar(files)-13,nchar(files)-4))

r <- values(rast(files))
dias <- as.numeric(as.Date(fechas[fechas < '2023-06-01']))
tb <- tibble(dias = seq(min(dias),max(dias)))

modelos_df <- lapply(variables,function(x) tb)
names(modelos_df) <- paste0(variables,"_df")

fit_all_models <- function(i) {
  modelos <- lapply(variables,function(var) fit_gam_model(var,r,i,dias))
  predicciones <- lapply(modelos,function(model) predict(model,newdata = tb))
  return(predicciones)
}

num_cores <- detectCores() - 1  # Usar todos los núcleos menos uno
cl <- makeCluster(num_cores)
clusterExport(cl,list("fit_gam_model","variables","r","dias","tb","gam"))
clusterEvalQ(cl,library(mgcv))

resultados <- parLapply(cl,1:nrow(r),fit_all_models)

stopCluster(cl)

for (i in 1:nrow(r)) {
  for (j in seq_along(variables)) {
    modelos_df[[j]][[paste0('pixel_',i)]] <- resultados[[i]][[j]]
  }
}

dias_completos <- gsub('-','_',as.character(as.Date(seq(min(dias),max(dias)))))
r <- rast(files)[[1]]

for (x in 1:length(dias_completos)) {
  
  r_list <- list()
  
  for (i in seq_along(variables)) {
    r <- rast(files)[[1]] * NA
    names(r) <- variables[i]
    values(r) <- as.numeric(modelos_df[[i]][x,-1])
    r_list[[i]] <- r
  }
    
  r <- rast(r_list)
  writeRaster(r,
              file.path('data','processed','espacial','raster','index_smooth_iv',
                        glue('index_smooth_la_esperanza_{dias_completos[x]}.tif')))

}

### La Esperanza Temporada 2023-2024

files_le_2023 <- files_le[fechas_le > '2023-06-01']

r_2023 <- values(rast(files_le_2023))

ndwi_list <- list()

dias <- as.numeric(as.Date(fechas_le[fechas_le > '2023-06-01']))

ndwi_df <- tibble(dias = seq(min(dias),max(dias)))
ndmi_df <- tibble(dias = seq(min(dias),max(dias)))
msi_df <- tibble(dias = seq(min(dias),max(dias)))
gci_df <- tibble(dias = seq(min(dias),max(dias)))
ndvi_df <- tibble(dias = seq(min(dias),max(dias)))
nbr_df <- tibble(dias = seq(min(dias),max(dias)))
b_i_df <- tibble(dias = seq(min(dias),max(dias)))

for (i in 1:nrow(r_2023)) {
  
  {
    ndwi_model <- gam(ndwi ~ s(dias),
                      data = data.frame(dias = dias,
                                        ndwi = r_2023[i,grep("^ndwi$",colnames(r_2023))]),
                      method = "REML")
    ndmi_model <- gam(ndwi ~ s(dias),
                      data = data.frame(dias = dias,
                                        ndwi = r_2023[i,grep("^ndmi$",colnames(r_2023))]),
                      method = "REML")
    msi_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2023[i,grep("^msi$",colnames(r_2023))]),
                     method = "REML")
    gci_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2023[i,grep("^gci$",colnames(r_2023))]),
                     method = "REML")
    ndvi_model <- gam(ndwi ~ s(dias),
                      data = data.frame(dias = dias,
                                        ndwi = r_2023[i,grep("^ndvi$",colnames(r_2023))]),
                      method = "REML")
    nbr_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2023[i,grep("^nbr$",colnames(r_2023))]),
                     method = "REML")
    b_i_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2023[i,grep("^b_i$",colnames(r_2023))]),
                     method = "REML")
  }
  
  ndwi_df[[paste0('pixel_',i)]] <- predict(ndwi_model,newdata= ndwi_df)
  ndmi_df[[paste0('pixel_',i)]] <- predict(ndmi_model,newdata= ndmi_df)
  msi_df[[paste0('pixel_',i)]] <- predict(msi_model,newdata= msi_df)
  gci_df[[paste0('pixel_',i)]] <- predict(gci_model,newdata= gci_df)
  ndvi_df[[paste0('pixel_',i)]] <- predict(ndvi_model,newdata= ndvi_df)
  nbr_df[[paste0('pixel_',i)]] <- predict(nbr_model,newdata= nbr_df)
  b_i_df[[paste0('pixel_',i)]] <- predict(b_i_model,newdata= b_i_df)
  
}

dias_completos <- gsub('-','_',as.character(as.Date(seq(min(dias),max(dias)))))
r <- rast(files_le_2023)[[1]]

for (x in 1:length(dias_completos)) {
  
  ndwi_r <- r
  ndmi_r <- r
  msi_r <- r
  gci_r <- r
  ndvi_r <- r
  nbr_r <- r
  b_i_r <- r
  
  values(ndwi_r) <- as.numeric(ndwi_df[x,-1])
  values(ndmi_r) <- as.numeric(ndmi_df[x,-1])
  values(msi_r) <- as.numeric(msi_df[x,-1])
  values(gci_r) <- as.numeric(gci_df[x,-1])
  values(ndvi_r) <- as.numeric(ndvi_df[x,-1])
  values(nbr_r) <- as.numeric(nbr_df[x,-1])
  values(b_i_r) <- as.numeric(b_i_df[x,-1])
  
  r_index <- c(ndwi_r,ndmi_r,msi_r,gci_r,ndvi_r,nbr_r,b_i_r)
  names(r_index) <- c('ndwi','ndmi','msi','gci','ndvi','nbr','b_i') 
  
  r_index
  
  writeRaster(r_index,glue('data/processed/espacial/raster/index_smooth_la_esperanza_{dias_completos[x]}.tif'))
  
}

## rio claro

files_rc <- grep('/index_rio_claro',files,value=T)
fechas_rc <- gsub('_','-',substr(files_rc,nchar(files_rc)-13,nchar(files_rc)-4))

### Temporada 2022-2023

files_rc_2022 <- files_rc[fechas_rc < '2023-06-01']

r_2022 <- values(rast(files_rc_2022))

dias <- as.numeric(as.Date(fechas_rc[fechas_rc < '2023-06-01']))

ndwi_df <- tibble(dias = seq(min(dias),max(dias)))
ndmi_df <- tibble(dias = seq(min(dias),max(dias)))
msi_df <- tibble(dias = seq(min(dias),max(dias)))
gci_df <- tibble(dias = seq(min(dias),max(dias)))
ndvi_df <- tibble(dias = seq(min(dias),max(dias)))
nbr_df <- tibble(dias = seq(min(dias),max(dias)))
b_i_df <- tibble(dias = seq(min(dias),max(dias)))

for (i in 1:nrow(r_2022)) {
  
  {
    ndwi_model <- gam(ndwi ~ s(dias),
                      data = data.frame(dias = dias,
                                        ndwi = r_2022[i,grep("^ndwi$",colnames(r_2022))]),
                      method = "REML")
    ndmi_model <- gam(ndwi ~ s(dias),
                      data = data.frame(dias = dias,
                                        ndwi = r_2022[i,grep("^ndmi$",colnames(r_2022))]),
                      method = "REML")
    msi_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2022[i,grep("^msi$",colnames(r_2022))]),
                     method = "REML")
    gci_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2022[i,grep("^gci$",colnames(r_2022))]),
                     method = "REML")
    ndvi_model <- gam(ndwi ~ s(dias),
                      data = data.frame(dias = dias,
                                        ndwi = r_2022[i,grep("^ndvi$",colnames(r_2022))]),
                      method = "REML")
    nbr_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2022[i,grep("^nbr$",colnames(r_2022))]),
                     method = "REML")
    b_i_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2022[i,grep("^b_i$",colnames(r_2022))]),
                     method = "REML")
  }
  
  ndwi_df[[paste0('pixel_',i)]] <- predict(ndwi_model,newdata= ndwi_df)
  ndmi_df[[paste0('pixel_',i)]] <- predict(ndmi_model,newdata= ndmi_df)
  msi_df[[paste0('pixel_',i)]] <- predict(msi_model,newdata= msi_df)
  gci_df[[paste0('pixel_',i)]] <- predict(gci_model,newdata= gci_df)
  ndvi_df[[paste0('pixel_',i)]] <- predict(ndvi_model,newdata= ndvi_df)
  nbr_df[[paste0('pixel_',i)]] <- predict(nbr_model,newdata= nbr_df)
  b_i_df[[paste0('pixel_',i)]] <- predict(b_i_model,newdata= b_i_df)
  
  if (i %% 100 == 0) {print(i)}
  
}

dias_completos <- gsub('-','_',as.character(as.Date(seq(min(dias),max(dias)))))
r <- rast(files_rc_2022)[[1]]

for (x in 1:length(dias_completos)) {
  
  ndwi_r <- r
  ndmi_r <- r
  msi_r <- r
  gci_r <- r
  ndvi_r <- r
  nbr_r <- r
  b_i_r <- r
  
  values(ndwi_r) <- as.numeric(ndwi_df[x,-1])
  values(ndmi_r) <- as.numeric(ndmi_df[x,-1])
  values(msi_r) <- as.numeric(msi_df[x,-1])
  values(gci_r) <- as.numeric(gci_df[x,-1])
  values(ndvi_r) <- as.numeric(ndvi_df[x,-1])
  values(nbr_r) <- as.numeric(nbr_df[x,-1])
  values(b_i_r) <- as.numeric(b_i_df[x,-1])
  
  r_index <- c(ndwi_r,ndmi_r,msi_r,gci_r,ndvi_r,nbr_r,b_i_r)
  names(r_index) <- c('ndwi','ndmi','msi','gci','ndvi','nbr','b_i') 
  
  r_index
  
  writeRaster(r_index,glue('data/processed/espacial/raster/index_smooth_rio_claro_{dias_completos[x]}.tif'))
  
}

### Temporada 2023-2024

files_rc_2023 <- files_rc[fechas_rc > '2023-06-01']

r_2023 <- values(rast(files_rc_2023))

dias <- as.numeric(as.Date(fechas_rc[fechas_rc > '2023-06-01']))

ndwi_df <- tibble(dias = seq(min(dias),max(dias)))
ndmi_df <- tibble(dias = seq(min(dias),max(dias)))
msi_df <- tibble(dias = seq(min(dias),max(dias)))
gci_df <- tibble(dias = seq(min(dias),max(dias)))
ndvi_df <- tibble(dias = seq(min(dias),max(dias)))
nbr_df <- tibble(dias = seq(min(dias),max(dias)))
b_i_df <- tibble(dias = seq(min(dias),max(dias)))

for (i in 1:nrow(r_2023)) {
  
  {
    ndwi_model <- gam(ndwi ~ s(dias),
                      data = data.frame(dias = dias,
                                        ndwi = r_2023[i,grep("^ndwi$",colnames(r_2023))]),
                      method = "REML")
    ndmi_model <- gam(ndwi ~ s(dias),
                      data = data.frame(dias = dias,
                                        ndwi = r_2023[i,grep("^ndmi$",colnames(r_2023))]),
                      method = "REML")
    msi_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2023[i,grep("^msi$",colnames(r_2023))]),
                     method = "REML")
    gci_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2023[i,grep("^gci$",colnames(r_2023))]),
                     method = "REML")
    ndvi_model <- gam(ndwi ~ s(dias),
                      data = data.frame(dias = dias,
                                        ndwi = r_2023[i,grep("^ndvi$",colnames(r_2023))]),
                      method = "REML")
    nbr_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2023[i,grep("^nbr$",colnames(r_2023))]),
                     method = "REML")
    b_i_model <- gam(ndwi ~ s(dias),
                     data = data.frame(dias = dias,
                                       ndwi = r_2023[i,grep("^b_i$",colnames(r_2023))]),
                     method = "REML")
  }
  
  ndwi_df[[paste0('pixel_',i)]] <- predict(ndwi_model,newdata= ndwi_df)
  ndmi_df[[paste0('pixel_',i)]] <- predict(ndmi_model,newdata= ndmi_df)
  msi_df[[paste0('pixel_',i)]] <- predict(msi_model,newdata= msi_df)
  gci_df[[paste0('pixel_',i)]] <- predict(gci_model,newdata= gci_df)
  ndvi_df[[paste0('pixel_',i)]] <- predict(ndvi_model,newdata= ndvi_df)
  nbr_df[[paste0('pixel_',i)]] <- predict(nbr_model,newdata= nbr_df)
  b_i_df[[paste0('pixel_',i)]] <- predict(b_i_model,newdata= b_i_df)
  
}

dias_completos <- gsub('-','_',as.character(as.Date(seq(min(dias),max(dias)))))
r <- rast(files_rc_2023)[[1]]

for (x in 1:length(dias_completos)) {
  
  ndwi_r <- r
  ndmi_r <- r
  msi_r <- r
  gci_r <- r
  ndvi_r <- r
  nbr_r <- r
  b_i_r <- r
  
  values(ndwi_r) <- as.numeric(ndwi_df[x,-1])
  values(ndmi_r) <- as.numeric(ndmi_df[x,-1])
  values(msi_r) <- as.numeric(msi_df[x,-1])
  values(gci_r) <- as.numeric(gci_df[x,-1])
  values(ndvi_r) <- as.numeric(ndvi_df[x,-1])
  values(nbr_r) <- as.numeric(nbr_df[x,-1])
  values(b_i_r) <- as.numeric(b_i_df[x,-1])
  
  r_index <- c(ndwi_r,ndmi_r,msi_r,gci_r,ndvi_r,nbr_r,b_i_r)
  names(r_index) <- c('ndwi','ndmi','msi','gci','ndvi','nbr','b_i') 
  
  r_index
  
  writeRaster(r_index,glue('data/processed/espacial/raster/index_smooth/index_smooth_rio_claro_{dias_completos[x]}.tif'))
  
}
