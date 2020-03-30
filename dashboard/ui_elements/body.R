source("ui_elements/tabs/tab_hard_currency.R")
source("ui_elements/tabs/tab_fortnite.R")
source("ui_elements/tabs/tab_blog.R")

body <-
  dashboardBody(
    tabItems(tab_hard_currency),
    tabItems(tab_blog),
    tabItems(tab_fortnite)
    )
