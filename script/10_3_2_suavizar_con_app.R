library(fs)
library(tidyverse)
files <- dir_ls('data/processed/espacial/raster/index_raw/',regexp = 'tif$')

files_le_2020 <- str_subset(files[fechas < "2023-06-01"],'la_esperanza.*(2022|2023)')
fechas <- ymd(str_extract(basename(files_le_2020),'[0-9]{4}_[0-9]{2}_[0-9]{2}'))

library(terra)
library(mgcv)
library(tictoc)

le_s2 <- rast(files_le_2020) |> subset(seq(1,378,7))
tic()
suavizado <- app(le_s2,\(y){
  dias <- as.numeric(fechas)
  data <- data.frame(x=dias,y=y)
  model <- gam(y ~ s(x), data = data,method = "REML")
  new_data <- data.frame(x=seq(min(dias),max(dias)))
  predict(model,new_data) |> as.numeric()
})
toc()
#157.161 sec elapsed

# método Abel

tic()
#source('script/funciones/paquetes.R')
library(mgcv)

### Temporada 2022-2023

r_2022 <- values(rast(files_le_2020))

ndwi_list <- list()

dias <- as.numeric(ymd(fechas))

ndwi_df <- tibble(dias = seq(min(dias),max(dias)))
# ndmi_df <- tibble(dias = seq(min(dias),max(dias)))
# msi_df <- tibble(dias = seq(min(dias),max(dias)))
# gci_df <- tibble(dias = seq(min(dias),max(dias)))
# ndvi_df <- tibble(dias = seq(min(dias),max(dias)))
# nbr_df <- tibble(dias = seq(min(dias),max(dias)))
# b_i_df <- tibble(dias = seq(min(dias),max(dias)))

for (i in 1:nrow(r_2022)) {
  
    ndwi_model <- gam(ndwi ~ s(dias), 
                      data = data.frame(dias = dias,
                                        ndwi = r_2022[i, grep("^ndwi$", colnames(r_2022))]), 
                      method = "REML")
  #   ndmi_model <- gam(ndwi ~ s(dias), 
  #                     data = data.frame(dias = dias,
  #                                       ndwi = r_2022[i, grep("^ndmi$", colnames(r_2022))]), 
  #                     method = "REML")
  #   msi_model <- gam(ndwi ~ s(dias), 
  #                    data = data.frame(dias = dias,
  #                                      ndwi = r_2022[i, grep("^msi$", colnames(r_2022))]), 
  #                    method = "REML")
  #   gci_model <- gam(ndwi ~ s(dias), 
  #                    data = data.frame(dias = dias,
  #                                      ndwi = r_2022[i, grep("^gci$", colnames(r_2022))]), 
  #                    method = "REML")
  #   ndvi_model <- gam(ndwi ~ s(dias), 
  #                     data = data.frame(dias = dias,
  #                                       ndwi = r_2022[i, grep("^ndvi$", colnames(r_2022))]), 
  #                     method = "REML")
  #   nbr_model <- gam(ndwi ~ s(dias), 
  #                    data = data.frame(dias = dias,
  #                                      ndwi = r_2022[i, grep("^nbr$", colnames(r_2022))]), 
  #                    method = "REML")
  #   b_i_model <- gam(ndwi ~ s(dias), 
  #                    data = data.frame(dias = dias,
  #                                      ndwi = r_2022[i, grep("^b_i$", colnames(r_2022))]), 
  #                    method = "REML")
  # }
  
  ndwi_df[[paste0('pixel_',i)]] <- predict(ndwi_model,newdata= ndwi_df)
  # ndmi_df[[paste0('pixel_',i)]] <- predict(ndmi_model,newdata= ndmi_df)
  # msi_df[[paste0('pixel_',i)]] <- predict(msi_model,newdata= msi_df)
  # gci_df[[paste0('pixel_',i)]] <- predict(gci_model,newdata= gci_df)
  # ndvi_df[[paste0('pixel_',i)]] <- predict(ndvi_model,newdata= ndvi_df)
  # nbr_df[[paste0('pixel_',i)]] <- predict(nbr_model,newdata= nbr_df)
  # b_i_df[[paste0('pixel_',i)]] <- predict(b_i_model,newdata= b_i_df)
  # 
}
toc()
#349.126 sec elapsed