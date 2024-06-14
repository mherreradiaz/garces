source('script/funciones/paquetes.R')

# predicho

data <- read_rds('data/processed/potencial_xgb_predict.rds')

data |> 
  mutate(fecha = as.Date(fecha)) |>  
  ggplot(aes(fecha,potencial,color=tratamiento)) +
  geom_point(alpha = 0.5, size = .5) +  
  geom_smooth(method = "gam", se = FALSE) + 
  facet_grid(sitio~temporada, scales = "free",labeller=as_labeller(names)) +
  labs(title = "Series Diarias de Potencial Predicho (XgBoost)",
       x = "mes",
       y = "potencial (kPa)",
       color = "Tratamiento") +
  scale_x_date(labels = date_format("%b. %Y")) +
  theme_light() +
  theme(strip.text = element_text(size = 10))
ggsave(paste0('output/figs/series_potencial.png'),scale =3:5)

# obs

data <- read_rds('data/processed/potencial.rds')

data |> 
  mutate(fecha = as.Date(fecha),
         potencial = potencial_bar) |>  
  ggplot(aes(fecha,potencial,color=tratamiento)) +
  geom_point(alpha = 0.5, size = .5) +  
  geom_smooth(method = "loess", se = FALSE) + 
  facet_grid(sitio~temporada, scales = "free",labeller=as_labeller(names)) +
  labs(title = "Series Diarias de Potencial Predicho (XgBoost)",
       x = "mes",
       y = "potencial (kPa)",
       color = "Tratamiento") +
  scale_x_date(labels = date_format("%b. %Y")) +
  theme_light() +
  theme(strip.text = element_text(size = 10))
ggsave(paste0('output/figs/series_potencial.png'),scale =3:5)