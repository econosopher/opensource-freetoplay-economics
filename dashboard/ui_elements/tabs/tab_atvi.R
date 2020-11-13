tab_atvi <-
  tabItem(tabName = "activision",
          h4("Discount Scaling Across Hard Currency SKUs"),
          plotlyOutput("segment_mau"),
          hr(),
          plotlyOutput("segment_revenue")
          )
