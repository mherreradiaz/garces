source('script/funciones/paquetes.R')

files_le <- list.files('data/raw/sentinel/sentinel_2a_la_esperanza',full.names=T)
files_rc <- list.files('data/raw/sentinel/sentinel_2a_rio_claro',full.names=T)

vi_names <- c('ndwi','ndmi','msi','gci','ndvi','nbr','nmdi','dwsi','ndvi_705','ndci','b_i')

for (x in 1:length(files_le)) {
  
  fecha_le <- gsub('-','_',substr(files_le[x],nchar(files_le[x])-13,nchar(files_le[x])-4))
  fecha_rc <- gsub('-','_',substr(files_rc[x],nchar(files_rc[x])-13,nchar(files_rc[x])-4))
  
  r_le <- rast(files_le[x])
  r_rc <- rast(files_rc[x])
  
  ndwi_le <- (r_le[['B03']]-r_le[['B08']])/(r_le[['B03']]+r_le[['B08']]) # Normalized Difference Water Index
  ndmi_le <- (r_le[['B08']]-r_le[['B11']])/(r_le[['B08']]+r_le[['B11']]) # Normalized Difference Moisture Index
  msi_le <- r_le[['B11']]/r_le[['B08']] # Moisture Stress Index
  gci_le <- (r_le[['B09']]/r_le[['B03']])-1 # Green Coverage Index
  ndvi_le <- (r_le[['B08']]-r_le[['B04']])/(r_le[['B08']]+r_le[['B04']]) #NDVI
  nbr_le <- (r_le[['B08']]-r_le[['B12']])/(r_le[['B08']]+r_le[['B12']]) # Normalized Burn Ratio
  nmdi_le <- (r_le[['B08']]-(r_le[['B11']]-r_le[['B12']]))/(r_le[['B08']]+(r_le[['B11']]-r_le[['B12']])) # Normalized Multi-Band Drought Index
  dwsi_le <- (r_le[['B08']]+r_le[['B03']])/(r_le[['B11']]+r_le[['B04']]) # Disease and Water Stress Index
  ndvi_705_le <- (r_le[['B06']]-r_le[['B05']])/(r_le[['B06']]+r_le[['B05']]) #NDVI Red Edge
  ndci_le <- (r_le[['B05']]-r_le[['B04']])/(r_le[['B05']]+r_le[['B04']]) # Normalized Difference Chlorophyll Index
  b_i_le <- r_le[['B11']]/r_le[['B12']] # Índice B11 Y B12
  
  ndwi_rc <- (r_rc[['B03']]-r_rc[['B08']])/(r_rc[['B03']]+r_rc[['B08']]) # Normalized Difference Water Index
  ndmi_rc <- (r_rc[['B08']]-r_rc[['B11']])/(r_rc[['B08']]+r_rc[['B11']]) # Normalized Difference Moisture Index
  msi_rc <- r_rc[['B11']]/r_rc[['B08']] # Moisture Stress Index
  gci_rc <- (r_rc[['B09']]/r_rc[['B03']])-1 # Green Coverage Index
  ndvi_rc <- (r_rc[['B08']]-r_rc[['B04']])/(r_rc[['B08']]+r_rc[['B04']]) #NDVI
  nbr_rc <- (r_rc[['B08']]-r_rc[['B12']])/(r_rc[['B08']]+r_rc[['B12']]) # Normalized Burn Ratio
  nmdi_rc <- (r_rc[['B08']]-(r_rc[['B11']]-r_rc[['B12']]))/(r_rc[['B08']]+(r_rc[['B11']]-r_rc[['B12']])) # Normalized Multi-Band Drought Index
  dwsi_rc <- (r_rc[['B08']]+r_rc[['B03']])/(r_rc[['B11']]+r_rc[['B04']]) # Disease and Water Stress Index
  ndvi_705_rc <- (r_rc[['B06']]-r_rc[['B05']])/(r_rc[['B06']]+r_rc[['B05']]) #NDVI Red Edge
  ndci_rc <- (r_rc[['B05']]-r_rc[['B04']])/(r_rc[['B05']]+r_rc[['B04']]) # Normalized Difference Chlorophyll Index
  b_i_rc <- r_rc[['B11']]/r_rc[['B12']] # Índice B11 Y B12
  
  r_le_index <- c(ndwi_le,ndmi_le,msi_le,gci_le,ndvi_le,nbr_le,ndmi_le,dwsi_le,ndvi_705_le,ndci_le,b_i_le)
  names(r_le_index) <- vi_names
  
  r_rc_index <- c(ndwi_rc,ndmi_rc,msi_rc,gci_rc,ndvi_rc,nbr_rc,ndmi_rc,dwsi_rc,ndvi_705_rc,ndci_rc,b_i_rc)
  names(r_rc_index) <- vi_names
  
  writeRaster(r_le_index,glue('data/processed/espacial/raster/vi_raw/index_la_esperanza_{fecha_le}.tif'))
  writeRaster(r_rc_index,glue('data/processed/espacial/raster/vi_raw/index_rio_claro_{fecha_le}.tif'))
  
}
