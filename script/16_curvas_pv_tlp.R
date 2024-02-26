library(drc)
library(xlsx)
library(readxl)

codigos <- excel_sheets('data/data_raw/pv/la_esperanza_pv.xlsx')[-8]

df <- data.frame(sitio = 'rio_claro',
                 codigo = codigos,
                 TLP = NA)

datos <- read.xlsx('data/data_raw/pv/rio_claro_pv_pr.xlsx', sheetName = codigos[n]) |>
  mutate(potencial = -bar/10,
         agua_hoja = peso_cosechado-peso_seco,
         minus_y = -1/potencial)

ggplot(datos[1:(nrow(datos)-x),], aes(x = agua_hoja, y = potencial)) +
  geom_point()

datos$RWD <- 100-datos$agua_hoja/lm(agua_hoja ~ potencial, datos[1:(nrow(datos)-x),])$coefficients[1]*100

df[n,3] <- -1/(lm(minus_y ~ RWD, datos[(x+1):nrow(datos),])$coefficients[1]+
      lm(minus_y ~ RWD, datos[(x+1):nrow(datos),])$coefficients[2]*datos$RWD[x+1])

write_rds(df,'data/data_postprocessed/tlp.rds')

