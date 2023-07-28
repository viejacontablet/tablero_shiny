
library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(kableExtra)
library(plotly)
library(crosstalk)
library(scales)
library(tidyr)


source("ui.R")
source("server.R")

# Ejecuta la aplicación
shinyApp(ui = ui, server = server, timeout = 1000)