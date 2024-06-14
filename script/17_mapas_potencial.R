library(tidyverse)
library(terra)
library(fs)
library(sf)
library(tmap)
library(colorspace)

le <- read_sf('data/processed/espacial/sitios/la_esperanza.gpkg','cuartel') |> 
  st_transform(32719)
rc <- read_sf('data/processed/espacial/sitios/rio_claro.gpkg','cuartel') |> 
  st_transform(32719)

# Rio Claro
files_rc <- dir_ls('data/processed/espacial/raster/potencial_predict',regexp = 'rio_claro')

swp_rc <- rast(files_rc)

swp_rc <- mask(swp_rc,rc)
swp_rc <- trim(swp_rc)

cov_rc <- global(swp_rc,\(x) sd(x,na.rm  =TRUE)/mean(x,na.rm = TRUE)) |> 
  rownames_to_column(var = 'dates') |> 
  rename(cov = global) |> 
  as_tibble()

dates_max_cov_rc <- cov_rc |> 
  arrange(desc(cov)) |> 
  slice_head(n = 6) |> 
  pull(dates)

ids <- match(dates_max_cov_rc,names(swp_rc))

swp_rc_mxcov <- subset(swp_rc,ids)

map1 <- tm_shape(-swp_rc_mxcov*0.1) +
  tm_raster(title = 'SWP (MPa)',style = 'cont', palette = diverging_hcl(palette = 'Green-Brown',n=7) ) +
  tm_facets(nrow = 2,,free.scales = TRUE) +
  tm_layout(legend.outside = FALSE)

tmap_save(map1,'output/figs/mapa_potencial_max_cov_rio_claro.png')

# La Esperanza
files_le <- dir_ls('data/processed/espacial/raster/potencial_predict',regexp = 'la_esperanza')

swp_le <- rast(files_le)

swp_le <- mask(swp_le,le)
swp_le <- trim(swp_le)

cov_le <- global(swp_le,\(x) sd(x,na.rm  =TRUE)/mean(x,na.rm = TRUE)) |> 
  rownames_to_column(var = 'dates') |> 
  rename(cov = global) |> 
  as_tibble()

dates_max_cov_le <- cov_le |> 
  arrange(desc(cov)) |> 
  slice_head(n = 5) |> 
  pull(dates)

ids <- match(dates_max_cov_le,names(swp_le))

swp_le_mxcov <- subset(swp_le,ids)

map2 <- tm_shape(-swp_le_mxcov*0.1) +
  tm_raster(title = 'SWP (MPa)',style = 'cont', palette = diverging_hcl(palette = 'Green-Brown',n=7) ) +
  tm_facets(nrow = 2,,free.scales = TRUE) +
  tm_layout(legend.outside = FALSE,legend.position = c('right','top'))

tmap_save(map2,'output/figs/mapa_potencial_max_cov_la_esperanza.png')
