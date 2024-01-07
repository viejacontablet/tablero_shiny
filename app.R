
source("ui.R")
source("server.R")

# Ejecuta la aplicación
shinyApp(ui = ui, server = server, timeout = 1000)