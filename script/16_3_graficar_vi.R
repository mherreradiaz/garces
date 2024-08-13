source('script/funciones/paquetes.R')
library(viridis)

Sys.setlocale("LC_TIME", "C")
convert_to_sentence_case <- function(string) {
  s <- strsplit(string, " ")[[1]]
  s <- tolower(s) # Primero convertimos toda la cadena a minúsculas
  s <- paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  return(s)
}
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

vi_names <- c('NDVI','EVI','GCI','NBR','NDWI','NDMI','MSI','NMDI','DWSI','CIr','CIg','NDRE1','NDRE2','NDCI','mSR705','RESI')

# graficar

data <- read_rds('data/processed/sentinel_vi_smooth.rds') |> 
  group_by(sitio,temporada,fecha) |> 
  summarize(across(c(cig:resi), mean, na.rm = TRUE)) |> 
  ungroup() |> 
  pivot_longer(cols = cig:resi,names_to = "indice",values_to = "valor") |> 
  rowwise() |> 
  mutate(fecha = as.Date(fecha),
         sitio = convert_to_sentence_case(gsub('_',' ',sitio)),
         sitio = factor(sitio,levels=c('Rio Claro','La Esperanza'))) |> 
  filter(!(temporada == '2022-2023' & fecha > '2023-05-01'),
         !(temporada == '2023-2024' & fecha > '2024-05-01')) |> 
  mutate(valor = ifelse(valor > 10,NA,valor),
         indice = toupper(indice),
         indice = ifelse(indice == 'CIG','CIg',indice),
         indice = ifelse(indice == 'CIR','CIr',indice),
         indice = ifelse(indice == 'MSR705','mSR705',indice),
         indice = factor(indice,levels=rev(vi_names)))

data |> 
  group_by(sitio,temporada,indice) |>
  mutate(valor = normalize(valor)) |>
  ungroup() |>
  na.omit() |> 
  ggplot(aes(x = fecha, y = indice, fill = valor)) +
  geom_tile() +
  scale_fill_viridis(option = "viridis", name = "normalized VI value") +
  labs(x = "month",
       y = "VI") +
  theme_light() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(sitio~temporada,scales='free_x')

ggsave('output/figs/series_vi_smooth_norm.png', width = 8, height = 5)


stats <- data |> 
  group_by(sitio,temporada,indice) |> 
  summarise(min = min(valor,na.rm=T),
            max = max(valor,na.rm=T),
            mean = mean(valor,na.rm=T),
            dif = max-min,
            sd = sd(valor,na.rm=T)) |> 
  arrange(dif)

stats |> filter(indice == vi_names[i])

data |> 
  filter(indice == vi_names[i]) |> 
  mutate(fecha = as.Date(ifelse(temporada == '2023-2024',fecha-years(1),fecha))) |> 
  ggplot(aes(fecha,valor)) +
  geom_point() +
  facet_grid(temporada~sitio, scales = 'free_x') +
  labs(y=vi_names[i])
