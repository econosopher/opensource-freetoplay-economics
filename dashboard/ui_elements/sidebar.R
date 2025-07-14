sidebar <-
  dashboardSidebar(
    sidebarMenu(
      menuItem("Finance", tabName = "finance", icon = icon("chart-pie")),
      menuItem("Metric Picker", tabName = "metric_picker", icon = icon("table")),
      menuItem("Hard Currency", tabName = "hard_currency", icon = icon("dollar-sign")),
      menuItem("Blog", href = "http://freetoplayeconomics.com/", icon = icon("i-cursor"))
    )
  )
