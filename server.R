

library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(kableExtra)
library(plotly)
library(scales)

color1 <- "#000000"
color2 <- "#40abed"

server <- function(input, output) {
  
 
  
  
  
output$num_series <- renderText({
    
 
  
    nrow( base_series_estado())
    
  })


base_series_estado <- reactive({
  
  base_series <- read.csv("base_series.csv",check.names = FALSE)
  
  base_series$fecha_ult_dato <- as.Date(base_series$fecha_ult_dato)
  
  
  
 base_series$dias_ult_act <-round( as.numeric( difftime(Sys.Date(), base_series$fecha_ult_dato, units = "days")),0)
 
  
  base_series$estado <- ""#creo una columna vacia para el for
  
  #es para establecer si está actualizado o desactualizado si mensual y si mas 30 días desactualizado, etc.
  
  
  
  for(e in c(1:nrow(base_series))){
    if (base_series[[e,9]] == "Mensual") {
      if(as.numeric(base_series[[e,17]])>30){
        base_series[e,18] <- "Desactualizado"
      } else {
        base_series[e,18] <- "Actualizado"
      }
    } else{
      if (base_series[[e,9]] == "Anual") {
        if(as.numeric(base_series[[e,17]])>365){
          base_series[e,18] <- "Desactualizado"
        } else {
          base_series[e,18] <- "Actualizado"
        }
      } else{
        if (base_series[[e,9]]=="Trimestral") {
          if(as.numeric(base_series[[e,17]])>90){
            base_series[e,18] <- "Desactualizado"
          } else {
            base_series[e,18] <- "Actualizado"
          }
        }
      }
    }
  }
  
  
  base_series
})

output$num_desactualizadas <- renderText({
  
  base_series_estado2 <- base_series_estado()

  conteo <- sum(grepl("Desactualizado", base_series_estado2$estado, fixed = TRUE))
  
  conteo
})


  base_errores <- reactive({
    base_errores <- data.frame(
      fecha         = c(Sys.Date()-10,Sys.Date()-14,Sys.Date()-20,Sys.Date()-30,Sys.Date()-45,Sys.Date()-50,Sys.Date()-60,Sys.Date()-65,Sys.Date()-70,Sys.Date()-86,Sys.Date()-90,Sys.Date()-100,Sys.Date()-115,Sys.Date()-123,Sys.Date()-135,Sys.Date()-150,Sys.Date()-160),
      id_error      = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","ñ","o","p"),
      tipo_de_error = c("Link caido", "Cambio de estructura", "Archivo vacío", "Cambio de formato","Link caido", "Cambio de estructura", "Archivo vacío", "Cambio de formato","Link caido", "Cambio de estructura", "Archivo vacío", "Cambio de formato","Cambio de formato","Link caido", "Cambio de estructura", "Archivo vacío", "Cambio de formato"),
      id_serie      = c(1,5,3,5,7,3,8,9,3,4,3,4,5,7,3,8,1),
      error         = c("","","","","","","","","","","","","","","","",""),
      warning       = c("","","","","","","","","","","","","","","","","")
    )
    
    
    min_dias <- input$filter2[1]
    max_dias <- input$filter2[2]

    base_errores <- base_errores %>%
        mutate(dias_desde_fecha = as.integer(Sys.Date() - fecha)) %>%
        filter(dias_desde_fecha >= min_dias & dias_desde_fecha <= max_dias)

    
    base_errores
  })

  
  base_actualizaciones <- reactive({
   base_actualizaciones <-   data.frame(
      fecha               = c(Sys.Date()-10,Sys.Date()-14,Sys.Date()-20,Sys.Date()-30,Sys.Date()-45,Sys.Date()-50,Sys.Date()-60,Sys.Date()-65,Sys.Date()-70,Sys.Date()-86,Sys.Date()-90,Sys.Date()-100,Sys.Date()-115,Sys.Date()-123,Sys.Date()-135,Sys.Date()-150,Sys.Date()-160,Sys.Date()-18,Sys.Date()-28,Sys.Date()-38,Sys.Date()-48,Sys.Date()-58,Sys.Date()-68,Sys.Date()-78,Sys.Date()-13,Sys.Date()-33,Sys.Date()-53,Sys.Date()-73,Sys.Date()-93,Sys.Date()-113,Sys.Date()-133,Sys.Date()-10,Sys.Date()-14,Sys.Date()-20,Sys.Date()-30,Sys.Date()-45,Sys.Date()-50,Sys.Date()-60,Sys.Date()-10,Sys.Date()-14,Sys.Date()-20,Sys.Date()-30,Sys.Date()-45,Sys.Date()-50,Sys.Date()-60,Sys.Date()-10,Sys.Date()-14,Sys.Date()-20,Sys.Date()-30,Sys.Date()-45,Sys.Date()-50,Sys.Date()-60),
      id_serie            = c(1,5,3,5,7,3,8,9,3,4,3,4,5,7,3,8,1,1,5,3,5,7,3,8,9,2,6,4,2,6,2,3,8,9,2,6,4,2,6,2,5,3,5,7,3,8,9,3,4,3,4,5),
      series_actualizadas = c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""),
      completada          = c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
   )
    
    min_dias <- input$filter2[1]
    max_dias <- input$filter2[2]
    
    base_actualizaciones <- base_actualizaciones %>%
      mutate(dias_desde_fecha = as.integer(Sys.Date() - fecha)) %>%
      filter(dias_desde_fecha >= min_dias & dias_desde_fecha <= max_dias)
    
    base_actualizaciones
    
  })
  
  output$num_errores_ult_mes <- renderText({
    errores  <- base_errores()
    # Calcular la fecha de hace una semana.
    fecha_hace_una_semana <- Sys.Date() - weeks(4)
    
    # Filtrar para obtener solo las filas de la última semana.
    errores_ultima_semana <- errores %>% 
      filter(fecha > fecha_hace_una_semana)
   nrow(errores_ultima_semana)
  
  })
  
  output$plot1 <- renderPlot({
    # Obtenemos el valor del filtro
    filtro1 <- input$filter1
    df_aux1 <- base_actualizaciones() 
    
    # Filtramos los datos en base a la opción seleccionada
    if(filtro1 == "Todo") {
      datos_filtrados <- df_aux1
    } else {
      # Si no, filtramos los datos en base a la opción seleccionada
      datos_filtrados <- df_aux1[as.numeric(df_aux1$id_serie) == as.numeric(filtro1),]
    }
    
    datos_filtrados$fecha <- as.Date(datos_filtrados$fecha)
    
  datos_filtrados  <- datos_filtrados %>% dplyr::group_by(fecha) %>% dplyr::summarise(cantidad = n())
    
    graf_act<-ggplot(datos_filtrados,mapping = aes(x=fecha, y=cantidad))+
      geom_col(fill=color2) + 
      scale_x_date("")+
      scale_y_continuous("Actualizaciones")+
      theme(panel.background = element_rect(fill = color1),) +
      theme(plot.background = element_rect(fill = color1,colour = color1),
            axis.title.x = element_text(colour = color2),
            axis.title.y = element_text(colour = color2, size = 12),
            axis.text.x = element_text(colour = color2,size = 15),
            axis.text.y = element_text(colour = color2,size = 12),
            axis.line.x = element_line(color = color1, size = 1),
            axis.line.y = element_line(color = color1, size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            #panel.border = element_rect(colour = "red", fill=NA, size=2)
      )
    
    
    graf_act
  }, height = 200)
  
  output$plot2 <- renderPlot({
    filtro  <- input$filter1
    df_aux2 <- base_errores()
    if(filtro == "Todo") {
      datos_filtrados <- df_aux2
    } else {
      # Si no, filtramos los datos en base a la opción seleccionada
      datos_filtrados <- df_aux2[as.numeric(df_aux2$id_serie) == filtro,]
    }
    datos_filtrados  <- datos_filtrados %>% dplyr::group_by(fecha) %>% dplyr::summarise(cantidad = n())
    
    # Aquí va el código para tu segundo gráfico
    graf_error<-ggplot(datos_filtrados,mapping = aes(x=fecha, y=cantidad))+
      geom_col(fill=color2) + 
      scale_x_date("")+
      scale_y_continuous("Errores")+
      theme(panel.background = element_rect(fill = color1),) +
      theme(plot.background = element_rect(fill = color1,colour = color1),
            axis.title.x = element_text(colour = color2),
            axis.title.y = element_text(colour = color2, size = 12),
            axis.text.x = element_text(colour = color2,size = 15),
            axis.text.y = element_text(colour = color2,size = 12),
            axis.line.x = element_line(color = color1, size = 1),
            axis.line.y = element_line(color = color1, size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
            #panel.border = element_rect(colour = "red", fill=NA, size=2)
      )
    
    
    graf_error
  }, height = 210)
  
  
  output$plot3 <- renderPlotly({
    
    # Obtenemos el valor del filtro
    filtro <- input$filter1
    df_aux3 <- base_errores()
    # Filtramos los datos en base a la opción seleccionada
    if(filtro == "Todo") {
      datos_filtrados <- df_aux3
    } else {
      # Si no, filtramos los datos en base a la opción seleccionada
      datos_filtrados <- df_aux3[as.numeric(df_aux3$id_serie) == as.numeric(filtro),]
    }
    datos_filtrados  <- datos_filtrados %>% dplyr::group_by(tipo_de_error) %>% dplyr::summarise(cantidad = n())
    
    # Aquí va el código para tu tercer gráfico
    fig <- datos_filtrados %>% plot_ly(labels = ~tipo_de_error, values = ~cantidad,
                                        marker = list(colors = c(color2,color2,color2,color2,color2,color2,color2,color2,color2),
                                                      line = list(color = color1, width = 2)),
                                        width = 280, 
                                        height = 215,
                                        textinfo = 'label+percent')
    fig <- fig %>% add_pie(hole = 0.4)
    
    graf_tipodeerrores <- fig %>% layout(title = "",  showlegend = F,
                                         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                         paper_bgcolor = color1,
                                         autosize = F, 
                                         font = list(color = color1, size = 9)
    )
    
    
    graf_tipodeerrores
    
  })
  
  
  datos_filtrados_tabla <- reactive({
    # Obtiene los valores del control deslizante
    dias_min <- input$filter2[1]
    dias_max <- input$filter2[2]
    
    base_series <- base_series_estado()
    
    # Filtra los datos
    datos_series_filtrados <- base_series %>%
      filter(dias_ult_act >= dias_min & dias_ult_act <= dias_max)
    
    datos_series_filtrados
  })
  
  # Definición de las tablas
  output$tabla1 <- renderUI({
    
    # Supongamos que datos_series_tabla es el dataframe que quieres visualizar
    datos_series_tabla <- datos_filtrados_tabla()[,c(1,4,9,14,16,17,18,6)]
    
    # Aquí se genera la tabla
    datos_series_tabla %>%
      kable(col.names = tools::toTitleCase(gsub("_", " ", colnames(datos_series_tabla))), 
            format.args = list(decimal.mark = ",", big.mark = "."),
            align = c("l", "l", "r", "r", "r", "r")) %>% 
      add_footnote(notation = "symbol", "") %>% 
      kable_styling(full_width = F) %>% 
      row_spec(0, bold = F, color = "white", background = color1, align = "c") %>% 
      row_spec(which(datos_series_tabla$estado == "Desactualizado"), background = color1, bold = F, color = color2) %>% 
      row_spec(which(datos_series_tabla$estado == "Actualizado"), background = color1, bold = F, color = "white") %>% 
      row_spec(which(datos_series_tabla$id_serie == input$filter1), background = color2, bold = F, color = "white") %>%
      as.character() %>% 
      HTML()
  })
  
  

  
  
  output$tabla2 <- renderUI({
    
    datos_series_tabla <- base_errores()
    # Aquí se genera la tabla
    datos_series_tabla %>%
      kable(col.names = tools::toTitleCase(gsub("_", " ", colnames(datos_series_tabla))), 
            format = "html",  # Asegúrate de especificar el formato HTML
            format.args = list(decimal.mark = ",", big.mark = "."),
            align = c("l", "l", "r", "r", "r", "r")) %>%
      kable_styling(full_width = FALSE,  # Aquí aplicas la configuración a toda la tabla
                    font_size = 12,  # Añadir si deseas cambiar el tamaño de la fuente
                    position = "left") %>%
      row_spec(0, bold = FALSE, color = "white", background = color1, align = "c") %>%  # Encabezado
      row_spec(1:nrow(datos_series_tabla), background = color1, bold = FALSE, color = "white") %>%  # Resto de filas
      row_spec(which(datos_series_tabla$id_serie == input$filter1), background = color2, bold = FALSE, color = "white") %>% 
      add_footnote(notation = "symbol", "") %>%
      as.character() %>% 
      HTML()
  })
  
  
  output$tabla3 <- renderUI({
    
    datos_series_tabla <- base_actualizaciones()
    # Aquí se genera la tabla
    datos_series_tabla %>%
      kable(col.names = tools::toTitleCase(gsub("_", " ", colnames(datos_series_tabla))), 
            format = "html",  # Asegúrate de especificar el formato HTML
            format.args = list(decimal.mark = ",", big.mark = "."),
            align = c("l", "l", "r", "r", "r", "r")) %>%
      kable_styling(full_width = FALSE,  # Aquí aplicas la configuración a toda la tabla
                    font_size = 12,  # Añadir si deseas cambiar el tamaño de la fuente
                    position = "left") %>%
      row_spec(0, bold = FALSE, color = "white", background = color1, align = "c") %>%  # Encabezado
      row_spec(1:nrow(datos_series_tabla), background = color1, bold = FALSE, color = "white") %>%  # Resto de filas
      row_spec(which(datos_series_tabla$id_serie == input$filter1), background = color2, bold = FALSE, color = "white") %>% 
      add_footnote(notation = "symbol", "") %>%
      as.character() %>% 
      HTML()
  })
  
  
}