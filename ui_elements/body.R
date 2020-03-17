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