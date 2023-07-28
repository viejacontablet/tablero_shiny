
library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(kableExtra)
library(plotly)
library(crosstalk)
library(scales)
library(tidyr)





server <- function(input, output) {
  
  # Definición de las infoBoxes
  output$info1 <- renderInfoBox({
    infoBox(
      title = "",
      value = nrow(datos_series),
      subtitle = "Cantidad de series",
      icon = icon("list")
    )
  })
  
  output$info2 <- renderInfoBox({
    infoBox(
      "Información 2", "2", icon = icon("arrow-down"), color = "red"
    )
  })
  
  output$info3 <- renderInfoBox({
    infoBox(
      "Información 3", "3", icon = icon("arrow-right"), color = "blue"
    )
  })
  
  # Definición de los gráficos
  output$plot1 <- renderPlot({
    # Obtenemos el valor del filtro
    filtro <- input$filter1
    df_aux1 <- df3
    # Filtramos los datos en base a la opción seleccionada
    if(filtro == "Todo") {
      datos_filtrados <- df_aux1
    } else {
      # Si no, filtramos los datos en base a la opción seleccionada
      datos_filtrados <- df_aux1[as.numeric(df_aux1$id_serie) == as.numeric(filtro),]
    }
    
    datos_filtrados$dias <- as.Date(datos_filtrados$dias)
    
    graf_act<-ggplot(datos_filtrados,mapping = aes(x=dias, y=cantidad_actualizaciones))+
      geom_col(fill="#f36b39") + 
      scale_x_date("")+
      scale_y_continuous("Actualizaciones")+
      theme(panel.background = element_rect(fill = "#282829"),) +
      theme(plot.background = element_rect(fill = "#282829",colour = "#282829"),
            axis.title.x = element_text(colour = "#f36b39"),
            axis.title.y = element_text(colour = "#f36b39", size = 12),
            axis.text.x = element_text(colour = "#f36b39",size = 15),
            axis.text.y = element_text(colour = "#f36b39",size = 12),
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
      datos_filtrados2 <- df_aux2[as.numeric(df_aux2$id_serie) == 1,]
    }
    # Aquí va el código para tu segundo gráfico
    graf_error<-ggplot(datos_filtrados2,mapping = aes(x=dias, y=cantidad_errores))+
      geom_col(fill="#f36b39") + 
      scale_x_date("")+
      scale_y_continuous("Errores")+
      theme(panel.background = element_rect(fill = "#282829"),) +
      theme(plot.background = element_rect(fill = "#282829",colour = "#282829"),
            axis.title.x = element_text(colour = "#f36b39"),
            axis.title.y = element_text(colour = "#f36b39", size = 12),
            axis.text.x = element_text(colour = "#f36b39",size = 15),
            axis.text.y = element_text(colour = "#f36b39",size = 12),
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
                                        marker = list(colors = c("#f36b39", "#d88f01", "#c99b39", "#f36b39","#c99b39" ,"#f36b39","#f36b39", "#d88f01", "#c99b39", "#f36b39","#c99b39" ,"#f36b39","#d88f01", "#c99b39", "#f36b39","#c99b39" ,"#f36b32"),
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
  
  # Definición de las tablas
  output$tabla1 <- renderUI({
    
    # Supongamos que datos_series_tabla es el dataframe que quieres visualizar
    datos_series_tabla <- datos_series[,c(1,4,6,9,14,16,17,18)]
    
    # Aquí se genera la tabla
    datos_series_tabla %>%
      kable(col.names = tools::toTitleCase(gsub("_", " ", colnames(datos_series_tabla))), 
            format.args = list(decimal.mark = ",", big.mark = "."),
            align = c("l", "l", "r", "r", "r", "r")) %>% 
      add_footnote(notation = "symbol", "") %>% 
      kable_styling(full_width = F) %>% 
      row_spec(0, bold = F, color = "white", background = "#282829", align = "c") %>% 
      row_spec(which(datos_series_tabla$estado == "Desactualizado"), background = "#f36b39", bold = F, color = "white") %>% 
      row_spec(which(datos_series_tabla$estado == "Actualizado"), background = "#282829", bold = F, color = "white") %>% 
      row_spec(which(datos_series_tabla$dias_ult_act == 0), background = "green", bold = F, color = "gray") %>%
      as.character() %>% 
      HTML()
  })
  
  output$table2 <- renderDataTable({
    # Aquí va el código para tu segunda tabla
  })
  
}