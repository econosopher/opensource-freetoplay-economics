sidebar <-
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",
               tabName = "dashboard",
               icon = icon("dashboard")
               ),
      menuItem("Data",
               tabName = "data",
               icon = icon("th")
               )
      )
    )