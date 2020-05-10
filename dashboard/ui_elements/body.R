source("ui_elements/tabs/tab_hard_currency.R")
source("ui_elements/tabs/tab_fortnite.R")
source("ui_elements/tabs/tab_blog.R")
source("ui_elements/tabs/tab_progression_curves.R")

body <-
  dashboardBody(
    tabItems(
      tab_hard_currency,
      tab_progression_curves,
      tab_fortnite,
      tab_blog
      )
    )
