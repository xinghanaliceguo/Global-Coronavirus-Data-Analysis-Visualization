APP.R
library(shiny)
# Source UI and server files
source("my_ui.R", echo = FALSE)
source("my_server.R", echo = FALSE)


# Create Shiny app 
shinyApp(ui = my_ui, server = my_server)
