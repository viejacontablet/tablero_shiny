library(readxl) 
library(lubridate)
library(tidyverse)
library(kableExtra)
library(plotly)
library(crosstalk)
library(scales)
library(tidyr)
library(shiny)
library(rsconnect)


#Cargo los datos------------------------------

datos_series <- read_excel("base_series.xlsx")

datos_series <- datos_series %>% mutate(dias_ult_act = as.numeric(-difftime(ultima_actualizacion, Sys.Date(), units = "days")))#saco el tiempo desde la ultima actua


datos_series <- datos_series %>% mutate(estado = "")#creo una columna vacia para el for

#es para establecer si está actualizado o desactualizado si mensual y si mas 30 días desactualizado, etc.

for(e in c(1:nrow(datos_series))){
if (datos_series[[e,9]] == "Mensual") {
  if(as.numeric(datos_series[[e,17]])>30){
    datos_series[e,18] <- "Desactualizado"
  } else {
    datos_series[e,18] <- "Actualizado"
  }
} else{
  if (datos_series[[e,9]] == "Anual") {
    if(as.numeric(datos_series[[e,17]])>365){
      datos_series[e,18] <- "Desactualizado"
    } else {
      datos_series[e,18] <- "Actualizado"
    }
  } else{
    if (datos_series[[e,9]]=="Trimestral") {
      if(as.numeric(datos_series[[e,17]])>90){
        datos_series[e,18] <- "Desactualizado"
      } else {
        datos_series[e,18] <- "Actualizado"
      }
    }
  }
}
}


#esto es para las valluebox 2

estados <- datos_series %>% group_by(estado) %>%
  summarise(cant_estados = n())
act <- estados[[1,2]]
desact <- estados[[2,2]]

#tabla 1


tabla1 <- datos_series[,c(1,4,6,9,14,16,17,18)]

tabla1 <- tabla1%>% mutate(dias_ult_dat= as.numeric(-difftime(as.Date(fecha_ult_dato,format = "%Y-%m-%d"), Sys.Date(), units = "days")))


tabla1$ultima_actualizacion <- as.Date(tabla1$ultima_actualizacion, format = "%Y-%m-%d")



#grafico 1


#esta parte es de juguete hasta que armemos bien como hacerlo


#invento un numero de errores para poder hacer el tablero

errores <- 3

#grafico 2 erorres

df3 <- read_excel("df3.xlsx")


df3 <- left_join(df3,base_errores)

df3$dias<-as.Date(df3$dias)



#df3 <- SharedData$new(df3)



