source('script/00_setup.R')
library(viridis)
Sys.setlocale("LC_TIME", "C")
convert_to_sentence_case <- function(string) {
  s <- strsplit(string, " ")[[1]]
  s <- tolower(s)
  s <- paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  return(s)
}

vi_names <- c('T','RH','VPD','ET0','PP','NDVI','EVI','GCI','NBR','NDWI','NDMI','MSI',
              'NMDI','DWSI','CIr','CIg','NDRE1','NDRE2','NDCI','mSR705','RESI')

data <- read_rds('data/processed/modelo_potencial.rds') |> 
  select(sitio:resi,t_media:pp)

data_cor <- data |> 
  group_by(sitio, temporada) |> 
  summarise(across(cig:pp, ~ cor(.x, potencial_bar, use = "complete.obs"), 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=cig:pp,names_to='var',values_to='cor') |> 
  rowwise() |> 
  mutate(cor = ifelse(is.na(cor),0,cor),
         var = toupper(var),
         var = ifelse(var == 'CIG','CIg',var),
         var = ifelse(var == 'CIR','CIr',var),
         var = ifelse(var == 'MSR705','mSR705',var),
         var = ifelse(var == 'T_MEDIA','T',var),
         var = ifelse(var == 'RH_MEDIA','RH',var),
         var = ifelse(var == 'VPD_MEDIO','VPD',var),
         var = ifelse(var == 'ETO','ET0',var),
         var = factor(var,levels=rev(vi_names)),
         sitio = convert_to_sentence_case(gsub('_',' ',sitio)),
         sitio = factor(sitio,levels=c('Rio Claro','La Esperanza')))

data_cor |> 
  mutate(sitio = as.factor(sitio)) |> 
  ggplot(aes(temporada,var,fill=cor)) +
  geom_tile() +
  geom_text(aes(label = round(cor, 2)), color = "white", size = 3) + # Añade los valores de cor
  
  scale_fill_viridis(option = "viridis", name = "r", 
                     limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  facet_grid(~sitio) +
  theme_light() +
  labs(y = 'variable',
       x = 'season')

ggsave(paste0('output/figs/cor_variables.png'), width = 5, height = 5)
