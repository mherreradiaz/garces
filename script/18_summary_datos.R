library(tidyverse)

data <- read_rds('data/processed/modelo_potencial.rds')

data |> 
  drop_na(potencial_bar) |> 
  group_by(sitio,temporada,fecha) |> 
  summarize(across(potencial_bar:vv,\(x) mean(x,na.rm = TRUE))) |> 
  mutate(across(potencial_bar:vv,scale)) |> 
  select(-(B01:B8A),-pp,-vv,-vh) |> 
  pivot_longer(-(sitio:fecha)) |> 
  ggplot(aes(name,fecha,fill=value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_discrete(labels = c('b_i','ET0','GCI','MSI',
                              'NBR','NDMI','NDVI','NDWI',
                                    '\Psi_s','RH','T','VPD')) +
  facet_grid(.~sitio) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
