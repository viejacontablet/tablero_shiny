
library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(kableExtra)
library(plotly)
library(crosstalk)
library(scales)
library(tidyr)




ui <- dashboardPage(
  dashboardHeader(title = "Tablero de Control"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    includeCSS("style2.css"),
    fluidRow(
      column(3, 
             box(width = 12,
                 div(
                   class = "box-series",
                   div( 
                     class = "text-section",   # Añadido este div
                     div(
                       class = "value",
                       nrow(datos_series)
                     ),
                     div(
                       class = "subtitle",
                       "Series operativas")
                   ),
                   div(
                     class = "icon-section",  # Añadido este div
                     icon("list", class = "fa-4x")
                   )
                 )),
             box(width = 12,
                 div(
                   class = "box-desact",
                   div( 
                     class = "text-section",   # Añadido este div
                     div(
                       class = "value",
                       desact
                     ),
                     div(
                       class = "subtitle",
                       "Series desactualizadas")
                   ),
                   div(
                     class = "icon-section",  # Añadido este div
                     icon("exclamation", class = "fa-4x")
                   )
                 )),
             box(width = 12,
                 div(
                   class = "box-errores",
                   div( 
                     class = "text-section",   # Añadido este div
                     div(
                       class = "value",
                       errores
                     ),
                     div(
                       class = "subtitle",
                       "Errores última semana")
                   ),
                   div(
                     class = "icon-section",  # Añadido este div
                     icon("skull-crossbones", class = "fa-4x"),
                   )
                 )),
             box(width = 12, height = "100", selectInput("filter1", "Seleccioná una serie:", choices = c("Todo",1:9))),
             box(width = 12, height = "100", sliderInput("filter2", "Seleccioná cantidad de días", min = 0, max = 100, value = c(25, 75))),
             box(width = 12, height = "100", selectInput("n", "N:", 1:5),
                 uiOutput("select_ui"))
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
                   tabPanel("Tabla 1", htmlOutput("tabla1")),
                   tabPanel("Tabla 2", dataTableOutput("table2"))
                 )))))
)
