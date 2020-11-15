tab_ea <-
  tabItem(tabName = "ea",
          fluidRow(
            column(6,
                   h4("Bookings by Segment"),
                   plotlyOutput("composition_bookings")
                   ),
            column(6,
                   h4("Bookings by Segment Share"),
                   plotlyOutput("composition_bookings_share")
                   ),
            ),
          fluidRow(
            column(6,
                   h4("Bookings by Platform"),
                   plotlyOutput("composition_platform")
                   ),
            column(6,
                   h4("Bookings by Platform Share"),
                   plotlyOutput("composition_platform_share")
                   )
            )
          )
