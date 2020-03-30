tab_hard_currency <-
  tabItem(tabName = "hard_currency",
          h4("Discount Scaling Across Hard Currency SKUs"),
          plotlyOutput("discount_over_base"),
          hr(),
          DTOutput("hard_currency_table")
          )
