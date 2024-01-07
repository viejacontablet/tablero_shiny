
library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(kableExtra)
library(plotly)
library(crosstalk)
library(scales)
library(tidyr)



source("stats_tablero_pagina.R")


base_series
str(base_series)

server <- function(input, output) {
  
  # Definición de las infoBoxes
  
  
  # Definición de los gráficos
  output$plot1 <- renderPlot({
    # Obtenemos el valor del filtro
    filtro1 <- input$filter1
    df_aux1 <- df3  # Assuming df3 is a regular R data frame
    # Filtramos los datos en base a la opción seleccionada
    if(filtro1 == "Todo") {
      datos_filtrados <- df_aux1
    } else {
      # Si no, filtramos los datos en base a la opción seleccionada
      datos_filtrados <- df_aux1[as.numeric(df_aux1$id_serie) == as.numeric(filtro1),]
    }
    
    datos_filtrados$dias <- as.Date(datos_filtrados$dias)
    
    graf_act<-ggplot(datos_filtrados,mapping = aes(x=dias, y=cantidad_actualizaciones))+
      geom_col(fill="#40abed") + 
      scale_x_date("")+
      scale_y_continuous("Actualizaciones")+
      theme(panel.background = element_rect(fill = "#282829"),) +
      theme(plot.background = element_rect(fill = "#282829",colour = "#282829"),
            axis.title.x = element_text(colour = "#40abed"),
            axis.title.y = element_text(colour = "#40abed", size = 12),
            axis.text.x = element_text(colour = "#40abed",size = 15),
            axis.text.y = element_text(colour = "#40abed",size = 12),
            axis.line.x = element_line(color = "#282829", size = 1),
            axis.line.y = element_line(color = "#282829", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            #panel.border = element_rect(colour = "red", fill=NA, size=2)
      )
    
    
    graf_act
  }, height = 200)
  
  output$plot2 <- renderPlot({
    filtro  <- input$filter1
    df_aux2 <- df3
    if(filtro == "Todo") {
      datos_filtrados2 <- df_aux2
    } else {
      # Si no, filtramos los datos en base a la opción seleccionada
      datos_filtrados2 <- df_aux2[as.numeric(df_aux2$id_serie) == filtro,]
    }
    # Aquí va el código para tu segundo gráfico
    graf_error<-ggplot(datos_filtrados2,mapping = aes(x=dias, y=cantidad_errores))+
      geom_col(fill="#40abed") + 
      scale_x_date("")+
      scale_y_continuous("Errores")+
      theme(panel.background = element_rect(fill = "#282829"),) +
      theme(plot.background = element_rect(fill = "#282829",colour = "#282829"),
            axis.title.x = element_text(colour = "#40abed"),
            axis.title.y = element_text(colour = "#40abed", size = 12),
            axis.text.x = element_text(colour = "#40abed",size = 15),
            axis.text.y = element_text(colour = "#40abed",size = 12),
            axis.line.x = element_line(color = "#282829", size = 1),
            axis.line.y = element_line(color = "#282829", size = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
            #panel.border = element_rect(colour = "red", fill=NA, size=2)
      )
    
    
    graf_error
  }, height = 210)
  
  output$plot3 <- renderPlotly({
    
    # Obtenemos el valor del filtro
    filtro <- input$filter1
    df_aux3 <- df3
    # Filtramos los datos en base a la opción seleccionada
    if(filtro == "Todo") {
      datos_filtrados3 <- df_aux3
    } else {
      # Si no, filtramos los datos en base a la opción seleccionada
      datos_filtrados3 <- df_aux3[as.numeric(df_aux3$id_serie) == as.numeric(filtro),]
    }
      
    # Aquí va el código para tu tercer gráfico
    fig <- datos_filtrados3 %>% plot_ly(labels = ~tipo_de_error, values = ~cantidad_errores,
                                        marker = list(colors = c("#40abed", "#b7d1f7", "#40abed", "#b7d1f7","#40abed", "#b7d1f7","#40abed", "#b7d1f7","#40abed", "#b7d1f7","#40abed", "#b7d1f7","#40abed", "#b7d1f7"),
                                                      line = list(color = '#282829', width = 2)),
                                        width = 280, 
                                        height = 215,
                                        textinfo = 'label+percent')
    fig <- fig %>% add_pie(hole = 0.4)
    
    graf_tipodeerrores <- fig %>% layout(title = "",  showlegend = F,
                                         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                         paper_bgcolor = "#282829",
                                         autosize = F, 
                                         font = list(color = '#282829', size = 9)
    )
    
    
    graf_tipodeerrores
    
  })
  
  datos_filtrados_tabla <- reactive({
    # Obtiene los valores del control deslizante
    dias_min <- input$filter2[1]
    dias_max <- input$filter2[2]
    
    # Filtra los datos
    datos_series_filtrados <- datos_series %>%
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
      row_spec(0, bold = F, color = "white", background = "#282829", align = "c") %>% 
      row_spec(which(datos_series_tabla$estado == "Desactualizado"), background = "#282829", bold = F, color = "#40abed") %>% 
      row_spec(which(datos_series_tabla$estado == "Actualizado"), background = "#282829", bold = F, color = "white") %>% 
      row_spec(which(datos_series_tabla$id_serie == input$filter1), background = "#40abed", bold = F, color = "white") %>%
      as.character() %>% 
      HTML()
  })
  
  
  
  
  output$table2 <- renderDataTable({
    # Aquí va el código para tu segunda tabla
  })
  
  observe({
    # Recoger la opción seleccionada en el filtro de selección múltiple
    filtro_series_actualizar <- input$filtro_series_actualizar
    
    auxi<- "Todos"
    if( auxi %in% filtro_series_actualizar) {
      series_a_actualizar <- 1:nrow(datos_series)
      write.csv(series_a_actualizar,row.names = FALSE, "filtro_series_a_actualizar.csv" )
    } else {
      write.csv(filtro_series_actualizar, row.names = FALSE, "filtro_series_a_actualizar.csv" )
    }
    
    # Imprime en consola las selecciones del filtro
    print(paste0("Series a actualizar seleccionadas: ", paste(filtro_series_actualizar, collapse = ", ")))
  })
  
  
    observeEvent(input$cargar_btn, {
      source("codigo_carga.R")
       })

  
    
    observe({
      # Recoger la opción seleccionada en el filtro de selección única
      filtro_serie_cambio_nombre <- input$serie_cambio_nombre 
      write.csv(filtro_serie_cambio_nombre,row.names = FALSE,"filtro_serie_cambio_nombre.csv")
    })
    
    observe({
      # Recoger el texto introducido por el usuario
      texto <- input$texto # Supongamos que 'texto' es el ID de tu caja de texto
      
      # Imprime en consola el texto introducido
      write.csv(texto, row.names = FALSE, "nombre_nuevo.csv")
    })
    observeEvent(input$cargar_btn2, {
      texto <- input$texto # Supongamos que 'texto' es el ID de tu caja de texto
      
      # Imprime en consola el texto introducido
      write.csv(texto, row.names = FALSE, "nombre_nuevo.csv")
      
      source("cambiar_nombre.R")
    })
  
}