sidebar <-
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cross-Firm", startExpanded = TRUE,
               menuSubItem("Summary", tabName = "activision", icon = icon("bullseye")),
               menuSubItem("Metric Picker", tabName = "activision", icon = icon("table"))
      ),
      menuItem("Activision", startExpanded = TRUE,
               menuSubItem("Summary", tabName = "activision", icon = icon("bullseye")),
               menuSubItem("Metric Picker", tabName = "activision", icon = icon("table"))
      ),
      menuItem("EA", startExpanded = TRUE,
               menuSubItem("Summary", tabName = "activision", icon = icon("bullseye")),
               menuSubItem("Metric Picker", tabName = "activision", icon = icon("table"))
      ),
      menuItem("Blog", tabName = "blog", icon = icon("i-cursor")),
      menuItem("Hard Currency", tabName = "hard_currency", icon = icon("dollar-sign"))
      #menuItem("Fortnite", tabName = "fortnite", icon = icon("dollar-sign"))
      #menuItem("Progression Curves", tabName = "progression_curves", icon = icon("dollar-sign"))
      )
    )
