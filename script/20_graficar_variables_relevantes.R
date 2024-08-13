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
reclasificar <- function(x, y) {
  y <- as.data.frame(y)
  x_reclassified <- x
  
  for (i in seq_len(nrow(y))) {
    x_reclassified[x == y[i, 1]] <- y[i, 2]
  }
  
  return(x_reclassified)
}

# datos

var_names <- c('eto','vpd_medio','t_media','rh_media','cab','msi','dwsi','msr705','ndmi','nmdi','pp')
var_names_plot <- c('ET0','VPD','T','RH','CCC','MSI','DWSI','mSR705','NDMI','NMDI','PP')

data <- read_rds('data/processed/modelo_potencial_smooth.rds') |> 
  select(sitio:potencial_bar,all_of(var_names))

# variabiliadd temporal

data_tmap <- data |> 
  group_by(sitio,temporada,fecha) |> 
  reframe(across(c(eto:pp), mean, na.rm = T)) |> 
  pivot_longer(cols = eto:pp,names_to = "variable",values_to = "valor") |> 
  rowwise() |> 
  mutate(fecha = as.Date(fecha),
         sitio = factor(convert_to_sentence_case(gsub('_',' ',sitio)),
                        ,levels=c('Rio Claro','La Esperanza'))) |> 
  filter(!between(month(fecha),5,9)) |> 
  mutate(variable = reclasificar(variable,tibble(var_names,var_names_plot)),
         variable = factor(variable,levels=rev(var_names_plot))) |> 
  group_by(sitio,temporada,variable) |> 
  mutate(valor = scale(valor)) |> 
  ungroup() |> 
  na.omit()

data_tmap |> 
  ggplot(aes(x = fecha, y = variable, fill = valor)) +
  geom_tile() +
  scale_fill_viridis(option = "viridis", name = "standarized values",
                     limits = c(-3, 3), 
                     oob = scales::squish) +
  labs(x = "month",
       y = "variables") +
  theme_light() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(sitio~temporada,scales='free_x')
ggsave('output/figs/series_vi_smooth_signific.png', width = 8, height = 5)  

# correlación

data_cor <- data |>
  group_by(sitio, temporada) |> 
  reframe(across(var_names, ~ cor(.x, potencial_bar, use = "complete.obs"), 
                   .names = "{.col}")) |> 
  pivot_longer(cols = eto:pp,names_to = "variable",values_to = "cor") |> 
  rowwise() |> 
  mutate(cor = ifelse(is.na(cor),0,cor),
         sitio = factor(convert_to_sentence_case(gsub('_',' ',sitio)),
                        ,levels=c('Rio Claro','La Esperanza')),
         variable = reclasificar(variable,tibble(var_names,var_names_plot)),
         variable = factor(variable,levels=rev(var_names_plot)))
  
data_cor |> 
  ggplot(aes(temporada,variable,fill=cor)) +
  geom_tile() +
  geom_text(aes(label = round(cor, 2)), color = "white", size = 3) + # Añade los valores de cor
  
  scale_fill_viridis(option = "viridis", name = "r", 
                     limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  facet_grid(~sitio) +
  theme_light() +
  labs(y = 'variable',
       x = 'season')

ggsave(paste0('output/figs/cor_variables_signif.png'), width = 5, height = 5)

# correlación meteo

meteo_names <- c('eto','vpd_medio','t_media','rh_media','pp')

cor_meteo <- data |>
  group_by(sitio, temporada,fecha) |>
  reframe(across(c(potencial_bar,all_of(meteo_names)), ~ mean(.x,na.rm=T),
                 .names = '{.col}')) |> 
  group_by(sitio, temporada) |> 
  reframe(across(eto:pp, ~ cor(.x, potencial_bar, use = "complete.obs"), 
                 .names = '{.col}')) |> 
  pivot_longer(cols = eto:pp,names_to = "variable",values_to = "cor") |> 
  rowwise() |> 
  mutate(cor = ifelse(is.na(cor),0,cor),
         sitio = factor(convert_to_sentence_case(gsub('_',' ',sitio)),
                        ,levels=c('Rio Claro','La Esperanza')),
         variable = reclasificar(variable,tibble(var_names,var_names_plot)),
         variable = factor(variable,levels=rev(var_names_plot)))

cor_meteo |> 
  ggplot(aes(temporada,variable,fill=cor)) +
  geom_tile() +
  geom_text(aes(label = round(cor, 2)), color = "black", size = 3) +
  scale_fill_gradientn(colors = paletteer_c("ggthemes::Red-Blue-White Diverging", 30),
                       limits = c(-1, 1), 
                       breaks = seq(-1, 1, by = 0.5), name = "r") +
  facet_grid(~sitio) +
  theme_light() +
  labs(y = 'variable',
       x = 'season')

ggsave(paste0('output/figs/cor_meteo.png'), width = 5, height = 5)

# correlacion vi

vi_names <- var_names <- c('cab','msi','dwsi','msr705','ndmi','nmdi')

cor_vi <- data |>
  na.omit() |> 
  group_by(sitio, temporada,fecha) |> 
  reframe(across(all_of(vi_names), ~ cor(.x, potencial_bar, use = "complete.obs"), 
                 .names = "{.col}")) |> 
  pivot_longer(cols = cab:nmdi,names_to = "variable",values_to = "cor") |> 
  rowwise() |> 
  mutate(cor = ifelse(is.na(cor),0,cor),
         sitio = factor(convert_to_sentence_case(gsub('_',' ',sitio)),
                        ,levels=c('Rio Claro','La Esperanza')),
         variable = reclasificar(variable,tibble(var_names,var_names_plot)),
         variable = factor(variable,levels=rev(var_names_plot)),
         fecha = as.Date(fecha))

cor_vi |> 
  ggplot(aes(fecha,variable,fill=cor)) +
  geom_tile() +
  # geom_text(aes(label = round(cor, 2)), color = "white", size = 3) +
  scale_fill_viridis(option = "viridis", name = "r", 
                     limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  labs(x = "month",
       y = "variables") +
  theme_light() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(sitio~temporada,scales='free_x')

cor_vi |> 
  mutate(grupo = case_when(
    variable %in% c("MSI", "NMDI") ~ "Grupo 1",
    TRUE ~ "Grupo 2")) |> 
  ggplot(aes(fecha,cor,color=variable)) +
  geom_line(alpha=.6) +
  # geom_smooth(aes(group = grupo, color = grupo), 
              # method = "loess", span =.3,se = F, alpha=.4) +
  geom_point(alpha=.7) +
  labs(x = "month",
       y = "r") +
  theme_light() +
  ylim(-2,2) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(sitio~temporada,scales='free_x')

ggsave(paste0('output/figs/cor_variables_signif.png'), width = 5, height = 5)



tif <- list.files(glue('data/raw/sentinel/sentinel_2a_{sitio}'),full.names = T)
fecha <- unlist(lapply(tif,function(x) {substr(x,nchar(x)-13,nchar(x)-4)}))
n <- c(length(which(fecha < '2023-06-01')),length(which(fecha > '2023-06-01')))
names(n) <- c('2022-2023','2023-2024')
n
