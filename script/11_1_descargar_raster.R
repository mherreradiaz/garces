source('script/00_paquetes.R')

# descargar imagenes (hacerlo dos veces, una para cada temporada)

edl_netrc(username = 'frzambra@gmail.com',password = 'Traplozx398#')
with_gdalcubes()

sitio <- 'rio_claro'
layers <- st_layers(glue('data/processed/spatial/{sitio}.gpkg'))
pol <- read_sf(glue('data/processed/spatial/{sitio}.gpkg'),layer = 'borde_cuartel')

bb <- st_bbox(pol) |> 
  as.numeric()

inicio <- "2023-09-01"
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

dir_out <- 'data/raw/raster'

raster_cube(col, v, mask=cloud_mask) |>
  write_tif(glue('{dir_out}/sentinel_{sitio}'))

# renombrar

r.file <- list.files('data/raw/raster/sentinel_rio_claro/')

names <- paste0('rio_claro',gsub('-','',substr(r.file,18,27)),'.tif')

for (i in seq_along(r.file)) {
  file.rename(from = file.path('data/raw/raster/sentinel_rio_claro', r.file[i]),
              to = file.path('data/raw/raster', names[i]))
}