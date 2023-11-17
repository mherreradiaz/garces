
library(tidyverse)
library(highcharter)
library(gt)
library(readr)

#leer data
data_fluo <- read_rds('data/data_processed/rds/data_fluorescencia.rds') |>
  select(sitio, fecha, codigo, 'Fv/Fm')

data_potencial <- read_rds('data/data_processed/rds/data_potencial.rds') |>
  select(sitio, fecha, codigo, potencial_bar)

data_sm <- read_rds('data/data_processed/rds/data_zim_sm.rds') |>
  filter(!codigo %in% c('H86A7', 'H79A7','H67A5','H62A8','H61A4')) |>
  filter(hour(fecha) == 14) |>
  ungroup() |>
  mutate(codigo = paste0(tratamiento,codigo),
         VWC = value,
         fecha = date(fecha)) |>
  select(sitio, fecha, codigo, VWC)

data_turgor <- read_rds('data/data_processed/rds/data_zim_turgor.rds') |>
  filter(hour(fecha) %in% 13:14) |>
  ungroup() |>
  mutate(codigo = substr(paste0(tratamiento,codigo),1,str_length(codigo)),
         fecha = date(fecha)) |>
  group_by(fecha,sitio,codigo) |>
  summarise(parche = mean(value,na.rm=T))

# matriz de correlación
data_join <- data_sm |> 
  left_join(data_turgor, by = c('sitio','fecha','codigo')) |>
  left_join(data_fluo, by = c('sitio','fecha','codigo')) |>
  left_join(data_potencial, by = c('sitio','fecha','codigo'))

for (i in c('la_esperanza','rio_claro')) {
matriz_cor <- data_join |>
  filter(sitio == i) |>
  select(VWC:potencial_bar) |> 
  na.omit() |>
  cor()

(ggplot(data = reshape2::melt(matriz_cor), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  # Agregar etiquetas
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  labs(x = "", y = "")) %>%
  ggsave(paste0('reporte/png/matriz_cor_',i,'.png'), ., width = 7, height = 6, units = "in")
}

