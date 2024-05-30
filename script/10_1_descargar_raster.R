source('script/funciones/paquetes.R')
library(beepr)

# descargar imagenes (hacerlo dos veces, una para cada temporada)

edl_netrc(username = 'frzambra@gmail.com',password = 'Traplozx398#')
with_gdalcubes()

sitio <- 'rio_claro'
layers <- st_layers(glue('data/processed/espacial/{sitio}.gpkg'))
pol <- read_sf(glue('data/processed/espacial/{sitio}.gpkg'),layer = 'cuartel')

bb <- st_bbox(pol) |> 
  as.numeric()

# inicio <- "2022-08-20"
# fin <- "2023-01-01"

# inicio <- "2022-12-20"
# fin <- "2023-05-01"

# inicio <- "2023-08-20"
# fin <- "2024-01-01"

inicio <- "2023-12-20"
fin <- "2024-05-01"

url <- "https://planetarycomputer.microsoft.com/api/stac/v1"

items <- stac(url) |> 
  stac_search(collections = "sentinel-2-l2a",
              bbox = bb,
              datetime = paste(inicio,fin, sep = "/")) |>
  post_request() |>
  items_sign(sign_fn = sign_planetary_computer()) |> 
  items_fetch()

bb <- pol |> 
  st_transform(32719) |> 
  st_bbox() |> 
  as.numeric()

v = cube_view(srs = "EPSG:32719",
              extent = list(t0 = as.character(inicio), 
                            t1 = as.character(fin),
                            left = bb[1], right = bb[3],
                            top = bb[4], bottom = bb[2]),
              dx = 10, dy = 10, dt = "P5D")

col <- stac_image_collection(items$features)

cloud_mask <- image_mask("SCL", values=c(3,8,9))

dir_out <- 'data/raw/sentinel'

raster_cube(col, v, mask=cloud_mask) |>
  write_tif(glue('{dir_out}/sentinel_{sitio}'))

beep(10)

# Sentinel 1
source('script/funciones/paquetes.R')

edl_netrc(username = 'frzambra@gmail.com',password = 'Traplozx398#')
with_gdalcubes()

sitio <- 'rio_claro'
layers <- st_layers(glue('data/processed/espacial/{sitio}.gpkg'))
pol <- read_sf(glue('data/processed/espacial/{sitio}.gpkg'),layer = 'cuartel')

bb <- st_bbox(pol) |> 
  as.numeric()

# inicio <- "2022-08-20"
# fin <- "2023-01-01"

# inicio <- "2022-12-20"
# fin <- "2023-05-01"

# inicio <- "2023-08-20"
# fin <- "2024-01-01"

inicio <- "2023-12-20"
fin <- "2024-05-01"

url <- "https://planetarycomputer.microsoft.com/api/stac/v1"

items <- stac(url) |> 
  stac_search(collections = 'sentinel-1-rtc',
              bbox = bb,
              datetime = paste(inicio,fin, sep = "/")) |>
  post_request() |>
  items_sign(sign_fn = sign_planetary_computer()) |> 
  items_fetch()

bb <- pol |> 
  st_transform(32719) |> 
  st_bbox() |> 
  as.numeric()

v = cube_view(srs = "EPSG:32719",
              extent = list(t0 = as.character(inicio), 
                            t1 = as.character(fin),
                            left = bb[1], right = bb[3],
                            top = bb[4], bottom = bb[2]),
              dx = 10, dy = 10, dt = "P5D")

col <- stac_image_collection(items$features)

dir_out <- 'data/raw/sentinel'

raster_cube(col, v) |>
  write_tif(glue('{dir_out}/sen_1_{sitio}'))
