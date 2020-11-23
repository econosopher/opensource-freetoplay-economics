tab_daily_financial_summary <-
  tabItem(tabName = "daily_financial_summary",
          fluidRow(
            column(12,
                   gt_output(outputId = "daily_summary")
                   )
            )
          )
