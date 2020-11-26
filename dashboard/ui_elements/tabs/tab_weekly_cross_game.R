tab_weekly_cross_game <-
  tabItem(tabName = "weekly_cross_game",
          fluidRow(
            column(6,
                   gt_output(outputId = "weekly_cross_game_gt")
                   ),
            column(6,
                   h4("Discount Scaling Across Hard Currency SKUs"),
                   )
            )
          )
