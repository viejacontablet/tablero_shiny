library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(plotly)
library(scales)
library(tidyr)

datos_series <- read.csv("base_series.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Tablero de Control"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    includeCSS("style3.css"),
    fluidRow(
      column(3, 
             box(width = 12,
                 div(
                   class = "box-series",
                   div( 
                     class = "text-section",
                     div(
                       class = "value",
                       textOutput("num_sereies")
                     ),
                     div(
                       class = "subtitle",
                       "Series operativas")
                   ),
                   div(
                     class = "icon-section",
                     icon("list", class = "fa-4x")
                   )
                 )),
             box(width = 12,
                 div(
                   class = "box-desact",
                   div( 
                     class = "text-section",
                     div(
                       class = "value",
                       desact
                     ),
                     div(
                       class = "subtitle",
                       "Series desactualizadas")
                   ),
                   div(
                     class = "icon-section",
                     icon("exclamation", class = "fa-4x")
                   )
                 )),
             box(width = 12,
                 div(
                   class = "box-errores",
                   div( 
                     class = "text-section",
                     div(
                       class = "value",
                       errores
                     ),
                     div(
                       class = "subtitle",
                       "Errores última semana")
                   ),
                   div(
                     class = "icon-section",
                     icon("skull-crossbones", class = "fa-4x")
                   )
                 )),
             
             box(width = 12, height = "100", selectInput("filter1", "Selecciondá una serie:", choices = c("Todo",1:9))),
             box(width = 12, height = "100", sliderInput("filter2", "Seleccioná cantidad de días", min = 0, max = 365, value = c(0, 365)))
      ),
      column(3,
             box(width = 12, height = "220", plotOutput("plot1")),
             box(width = 12, height = "230", plotOutput("plot2")),
             box(width = 12, height = "230", plotlyOutput("plot3", height = "100%", width = "100%"))
      ),
      column(6,
             box(width = 12, 
                 tabBox(
                   id = "tabset1", width = 12, height = "647",
                   tabPanel("Series", htmlOutput("tabla1")),
                   tabPanel("Errores", dataTableOutput("table2")),
                   tabPanel("Zona de carga",
                            div(class="zona_carga",
                                fluidRow(
                                  box(width = 12, div(
                                    class = "mi_div",
                                    selectInput("filtro_series_actualizar", 
                                                "Series a actualizar", 
                                                choices = c("Todos",c(1:nrow(datos_series))), 
                                                multiple = TRUE),
                                    actionButton("cargar_btn", "Actualizar series"),
                                    style = "margin-bottom: 20px" # Agrega un poco de espacio entre las dos filas
                                  ),style = "margin: 20px 0;"),
                                  box(width = 12, div(
                                    class = "mi_div",
                                    selectInput("serie_cambio_nombre", 
                                                "Serie a actualizar nombress", 
                                                choices = 1:nrow(datos_series)),
                                    textInput("texto", "Introduce texto aquí"),
                                    actionButton("cargar_btn2", "Cambiar nombre")
                                  ),style = "margin: 20px 0;")
                                )
                                
                                
                            )
                   )
                 )))
      
      ))
)

