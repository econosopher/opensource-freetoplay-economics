source("ui_elements/tabs/tab_blog.R")
source("ui_elements/tabs/tab_fortnite.R")

sidebar <-
  dashboardSidebar(
    sidebarMenu(
      menuItem("Hard Currency", tabName = "hard_currency", icon = icon("dollar-sign")),
      menuItem("Blog", tabName = "blog", icon = icon("dollar-sign"))
     # menuItem("Cosmetic Economies",
     #          menuSubItem("Fortnite"),
     #          tabName = "fortnite",
     #          startExpanded =  TRUE,
     #          icon = icon('line-chart')
     #          )
      )
    )
