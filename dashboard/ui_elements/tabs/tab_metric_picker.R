source('data/import_finance_atvi.R')

tab_metric_picker <-
  tabItem(tabName = "metric_picker",
          fluidRow(
            column(4,
                   selectizeInput('sheet',
                               label = h3("Sheet"),
                               choices = unique(merge_df$sheet),
                               selected = 'Operating Metrics'
                               )
                   ),
            column(4,
                   selectizeInput('metric',
                               label = h3("Metric"),
                               choices = unique(merge_df$metric),
                               selected = 'Net Bookings 1'
                               )
                   )
            ),
          fluidRow(
            column(12,
                   h4(""),
                   plotlyOutput("metric_picker")
                   )
            )
          )
