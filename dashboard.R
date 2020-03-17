pacman::p_load("shiny", "shinydashboard", "tidyverse", "DT", "plotly", "scales")

source("ui_elements/sidebar.R")
source("ui_elements/body.R")

ui <-
  dashboardPage(
    dashboardHeader(title = "F2P Economics"),
    sidebar,
    body
  )

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
