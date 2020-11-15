tab_finance <-
  tabItem(tabName = "finance",
          fluidRow(
            column(6,
                   h4("Share Price"),
                   plotlyOutput("share_price")
                   ),
            column(6,
                   h4("% Change in Share Price"),
                   plotlyOutput("index_share_price")
                   )
            ),
          fluidRow(
            column(12,
                   h4("Market Capitalization"),
                   plotlyOutput("market_cap")
                   )
            )
          )
