pacman::p_load("shiny", "shinydashboard", "tidyverse", "DT", "plotly", "scales")

sidebar <-
dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Data",
             tabName = "Data",
             icon = icon("th"),
             badgeColor = "green"
             )
    )
)
body <-
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content")
              ),

      tabItem(tabName = "data",
              h2("Widgets tab content")
              )
      )
    )

ui <-
  dashboardPage(
    dashboardHeader(title = "F2P Economics"),
    sidebar,
    body
  )

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
