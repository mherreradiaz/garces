library(drc)
library(xlsx)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)

codigos <- excel_sheets('data/data_raw/pv/la_esperanza_pv.xlsx')[-7]

df <- data.frame(sitio = 'la_esperanza',
                 codigo = codigos,
                 TLP = NA)

datos <- read_xlsx('data/data_raw/pv/la_esperanza_pv.xlsx', sheet = codigos[n]) |>
  mutate(potencial = -bar/10,
         agua_hoja = peso_cosechado-peso_seco,
         minus_y = -1/potencial)

ggplot(datos[1:(nrow(datos)-x),], aes(x = agua_hoja, y = potencial)) +
  geom_point()

datos$RWD <- 100-datos$agua_hoja/lm(agua_hoja ~ potencial, datos[1:(nrow(datos)-x+1),])$coefficients[1]*100

int <- lm(minus_y ~ RWD, datos[(nrow(datos)-x+1):nrow(datos),])$coefficients[1]
slp <- lm(minus_y ~ RWD, datos[(nrow(datos)-x+1):nrow(datos),])$coefficients[2]

ggplot(datos,aes(x = RWD,y = minus_y)) +
  geom_point() +
  geom_abline(intercept = int, slope = slp, color = "red") +
  labs(x = 'Deficit de agua relativo (%RWD)',
       y = expression(paste("Inversa del potencial hídrico ", -1/psi, " (MPa"^-1,')'))) +
  theme_light()
  
datos$Yo <- -1/(int+slp*datos$RWD)

df[n,3] <- datos$Yo[nrow(datos)-x+1]

write_rds(df,'data/data_postprocessed/tlp.rds')

