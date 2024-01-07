library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(kableExtra)
library(plotly)
library(scales)
library(tidyr)

n_series<- nrow(read.csv("base_series.csv",check.names = FALSE))



ui <- dashboardPage(
  dashboardHeader(title = "Tablero de control"),
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
                textOutput("num_series")
              ),
              div(
                class = "subtitle",
                "Cantidad de series")
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
                textOutput("num_desactualizadas")
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
                textOutput("num_errores_ult_mes")
              ),
              div(
                class = "subtitle",
                "Errores último mes")
            ),
            div(
              class = "icon-section",
              icon("skull-crossbones", class = "fa-4x")
            )
          )),
      box(width = 12, height = "100", selectInput("filter1", "Selecioná una  serie:", choices = c("Todo",1:9))),
      box(width = 12, height = "100", sliderInput("filter2", "Seleccinoná cantidad de días", min = 0, max = 800, value = c(0, 800)))
      
    ),
    column(3,
           box(width = 12, height = "220", plotOutput("plot1")),
           box(width = 12, height = "230", plotOutput("plot2")),
           box(width = 12, height = "230", plotlyOutput("plot3", height = "50%", width = "100%"))
           
    ),
    column(6,
           box(width = 12,  height = "690",
               tabBox(
                 id = "tabset1", width = 12, height = "647",
                 tabPanel("Series", htmlOutput("tabla1")),
                 tabPanel("Errores", htmlOutput("tabla2")),
                 tabPanel("Actualizaciones", 
                          fluidRow(
                            box(width = 12, 
                                div(
                                 class = "mi_div",
                                 selectInput("filtro_series_actualizar", 
                                          "Series a actualizar", 
                                          choices = c("Todos",c(1:n_series))), 
                                 multiple = TRUE),
                                 actionButton("cargar_btn", "Actualizar serie", style = "margin: auto; display: block;"),
                                 style = "margin-bottom: 20px" # Agrega un poco de espacio entre las dos filas
                            ),style = "margin: 20px;"),
                          
                          htmlOutput("tabla3")),
                 tabPanel("Nueva serie",
                                box(width = 12,
                                  actionButton("cargar_btn2", "Agregar series")
                                ),
                                box(width = 12,
                                  textInput("texto1", "Organismo"),
                                  textInput("texto2", "Fuente"),
                                  textInput("texto3", "Fuente de la información"),
                                  textInput("texto4", "Link de la fuente"),
                                  textInput("texto5", "Link de la serie"),
                                  textInput("texto7", "Tags"),
                                  textInput("texto8", "Periodicidad"),
                                  textInput("texto9", "Descestacionalizada"),
                                  textInput("texto10", "Unidad de medida"),
                                  textInput("texto11", "Tipo de resumen"),
                                  textInput("texto12", "Nombre descarga"),
                                  textInput("texto13", "Fecha ultimo dato"),
                                  textInput("texto14", "Nombre archivo")
                                
                              )
                              
                              
                          
                 )
               ))
    )
  )
)
)