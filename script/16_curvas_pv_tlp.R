library(drc)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)

data_pv <- read_xlsx('data/data_raw/pv/data_pv.xlsx')

data_tlp <- read_csv2('data/metadata/codigos_arboles.csv') |>
  mutate(tratamiento = substr(codigo,1,2),
         codigo = substr(codigo,3,nchar(codigo)),
         unidad = factor(unidad, levels = 1:3),
         .before = codigo)

codigos <- data_pv |>
  left_join(data_tlp, by= NULL) |>
  select(sitio,tratamiento,codigo, unidad) |>
  distinct()

for (n in 1:nrow(codigos)) {
  
  cat("Sitio: ", codigos$sitio[n], "\n","Unidad: ", codigos$codigo[n], "\n")
  
  data <- data_pv |>
    filter(sitio == codigos$sitio[n], codigo == codigos$codigo[n]) |>
    mutate(potencial = -bar/10,
           agua_hoja = peso_cosechado-peso_seco,
           minus_y = -1/potencial)
  
  for (a in 1:100) {
    
    if (a == 1) {x = 0}
    
    plot <- ggplot(data, aes(x = agua_hoja, y = potencial)) +
      geom_point() +
      labs(title = 'Efecto maseta y contenido de agua saturada',
           x = 'Agua de hoja (g)',
           y = 'Potencial (MPa)') +
      theme_light()
    
    print(plot)
    
    if (a == 1) {x <- as.numeric(readline(prompt = "Ingresar n: "))}
    
    data$RWD <- 100-data$agua_hoja/lm(agua_hoja ~ potencial, data[1:(nrow(data)-x+1),])$coefficients[1]*100
    
    int <- lm(minus_y ~ RWD, data[(nrow(data)-x+1):nrow(data),])$coefficients[1]
    slp <- lm(minus_y ~ RWD, data[(nrow(data)-x+1):nrow(data),])$coefficients[2]
    
    r2 <- cor(int+slp*data[(nrow(data)-x+1):nrow(data),]$RWD,
              data[(nrow(data)-x+1):nrow(data),]$minus_y)^2
    
    texto <- paste('y =', round(slp, 6), 'x +', round(int, 6), "\n", "R2", " =", round(r2, 6))
    
    ylim <- mean(range(data$minus_y))
    xlim <- max(data$RWD)-diff(range(data$RWD))*.2
    
    plot2 <- ggplot(data,aes(x = RWD,y = minus_y)) +
      geom_point() +
      geom_line() +
      geom_abline(intercept = int, slope = slp, color = "blue", linewidth = .7) +
      geom_point() +
      geom_point(data = data[(nrow(data)-x+1):nrow(data),],aes(x = RWD, y = minus_y), 
                 size = 2, color = 'red') +
      geom_text(x = xlim, y = ylim, label = texto, hjust = .5) +
      labs(x = 'Deficit de agua relativo (%RWD)',
           y = expression(paste(-1/psi, " (MPa"^-1,')'))) +
      theme_light()
    
    print(plot2)
    
    data$Yo <- -1/(int+slp*data$RWD)
    
    cat("El valor del TLP es de: ", data$Yo[nrow(data)-x+1], "\n")
    
    respuesta <- readline(prompt = "¿Volver a ajustar n? (s/n): ")
    if (respuesta == 'n') {break} else {
      x <- as.numeric(readline(prompt = "Ingresar n: "))
    }
  }
  
  respuesta <- readline(prompt = "¿Continuar? (s/n): ")
  if (respuesta != 's') {break} 
  
  df[[s]][n,3] <- data$Yo[nrow(data)-x+1]
  
  png(paste0('repote/plots/06_tlp/tlp_',
             codigos$sitio[n],'_',
             codigos$tratamiento[n],'_',
             codigos$unidad[n]),'.png')
  print(plot2)
  dev.off()
  
}


write_rds(df,'data/data_processed/tlp.rds')

