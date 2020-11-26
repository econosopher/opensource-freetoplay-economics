source("ui_elements/tabs/tab_hard_currency.R")
source("ui_elements/tabs/tab_atvi.R")
source("ui_elements/tabs/tab_ea.R")
source("ui_elements/tabs/tab_metric_picker.R")
source("ui_elements/tabs/tab_finance.R")
source("ui_elements/tabs/tab_daily_financial_summary.R")
source("ui_elements/tabs/tab_weekly_cross_game.R")
#source("ui_elements/tabs/tab_progression_curves.R")
#source("ui_elements/tabs/tab_fortnite.R")

body <-
  dashboardBody(
    tabItems(
      tab_weekly_cross_game,
      tab_daily_financial_summary,
      tab_finance,
      tab_hard_currency,
      tab_atvi,
      tab_ea,
      tab_metric_picker
      #tab_progression_curves,
      #tab_fortnite,
      )
    )
