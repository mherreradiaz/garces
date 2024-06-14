library(tidyverse)
library(terra)
library(fs)
library(sf)

le <- read_sf('data/processed/espacial/sitios/la_esperanza.gpkg','tratamientos') |> 
  st_transform(32719)
rc <- read_sf('data/processed/espacial/sitios/rio_claro.gpkg','tratamientos') |> 
  st_transform(32719)

# La Esperanza
files_le <- dir_ls('data/processed/espacial/raster/potencial_predict',regexp = 'la_esperanza')

swp_le <- rast(files_le)
data_le <- extract(swp_le,le,fun='mean',na.rm = TRUE)

data_le2 <- data_le |> 
  pivot_longer(-ID) |> 
  mutate(date = ymd(name),
         swp = -value*0.1,
         treatment = factor(ID,labels = c('T3','T1','T4','T2','T0')) |> 
           fct_relevel("T0","T1","T2","T3","T4")) |>
  select(-name,-value,-ID) |>
  mutate(season = case_when(between(date,ymd(20221001),ymd(20230430))~"2022-2023",
                            between(date,ymd(20231001),ymd(20240430))~"2023-2024",
                            .default = NA)
  ) |> drop_na()

files_rc <- dir_ls('data/processed/espacial/raster/potencial_predict',regexp = 'rio_claro')

swp_rc <- rast(files_rc)
data_rc <- extract(swp_rc,rc,fun='mean',na.rm = TRUE)

data_rc2 <- data_rc |> 
  pivot_longer(-ID) |> 
  mutate(date = ymd(name),
         swp = -value*0.1,
         treatment = factor(ID,labels = c('T3','T1','T4','T2','T0')) |> 
           fct_relevel("T0","T1","T2","T3","T4")) |>
  select(-name,-value,-ID) |>
  mutate(season = case_when(between(date,ymd(20221001),ymd(20230430))~"2022-2023",
                            between(date,ymd(20231001),ymd(20240430))~"2023-2024",
                            .default = NA)
  ) |> drop_na()

data_le2 <- data_le2 |> mutate(sitio = 'la_esperanza')
data_rc2 <- data_rc2 |> mutate(sitio = 'rio_claro')

data <- bind_rows(data_le2,data_rc2)

data |> 
  # group_by(date,season) |>
  # summarize(swp = mean(swp)) |> 
  ggplot(aes(date,swp,color =treatment)) +
  geom_point(size=.05) +
  # geom_line() +
  scale_color_viridis_d() +
  geom_smooth(span=.1,se= FALSE) + 
  facet_grid(sitio~season,scales = 'free') +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme_bw()
ggsave('output/figs/tendencias_potencial_estimado_trat_sitios.png',scale = 2,width=10)

#boxplot mensuales
data |> 
  mutate(mes = floor_date(date,'month')) |> 
  ggplot(aes(factor(mes),swp,fill=treatment)) + 
  geom_boxplot() +
  # geom_point(size=.2) + 
  # geom_line(lwd=.1) +
  scale_fill_viridis_d() +
  #scale_x_date(date_labels = "%b") +
  facet_grid(sitio~season,scales = 'free') +
  #scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme_bw()
ggsave('output/figs/boxplot_potencial_estimado_mes_trat_sitios.png',scale = 2,width=10)


#Rio Claro
#
       
  

#serie de tiempo diaria
#
data_rc2 |> 
  # group_by(date,season) |>
  # summarize(swp = mean(swp)) |> 
  ggplot(aes(date,swp,color =treatment)) +
  geom_point(size=.05) +
  # geom_line() +
  geom_smooth(span=.1,se= FALSE) + 
  facet_grid(.~season,scales = 'free_x') +
  theme_bw()

#boxplot mensuales
data_rc2 |> 
  mutate(mes = floor_date(date,'month')) |> 
  ggplot(aes(factor(mes),swp,fill=treatment)) + 
  geom_boxplot() +
  # geom_point(size=.2) + 
  # geom_line(lwd=.1) +
  scale_fill_viridis_d() +
  #scale_x_date(date_labels = "%b") +
  facet_wrap(.~season,scales = 'free_x') +
  theme_bw()
