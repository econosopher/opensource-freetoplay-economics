library("shiny")
library("shinydashboard")
library("tidyverse")
library("DT")
library("plotly")
library("scales")

source("ui_elements/sidebar.R")
source("ui_elements/body.R")
source("data/sku_data.R")

ui <-
  dashboardPage(
    dashboardHeader(title = "F2P Economics"),
    sidebar,
    body
  )

server <- function(input, output) {

sku_prices_df <-
  read.table(text = sku_prices, header = T)

sku_prices_df <-
  sku_prices_df %>%
  group_by(game) %>%
  mutate(
    `$1 USD Buys X HC` = hard_currency / usd_price,
    `Min EX Rate` = min(`$1 USD Buys X HC`),
    `% Discount Over Base SKU` = (`$1 USD Buys X HC` - `Min EX Rate`) / `Min EX Rate`
    )

output$discount_over_base <- renderPlotly(

  sku_prices_df %>%
  ungroup(game) %>%
  ggplot(aes(x = usd_price, y = `% Discount Over Base SKU`, group = game, color = game)) +
    geom_line() +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(labels = dollar) +
    labs(x = "SKU Price (USD)")

  )

output$hard_currency_table <- renderDT(
  sku_prices_df %>%
    mutate(
      `$1 USD Buys X HC` = round(`$1 USD Buys X HC`, 2),
      `% Discount Over Base SKU` = percent(`% Discount Over Base SKU`),
      usd_price = dollar(usd_price)
    ) %>%
    datatable(
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        searching = TRUE,
        buttons = c('copy', 'csv', 'excel')
        )
      ),
    server = FALSE #ya, it's weird. This is in the renderDT call, not datatable.
  )

output$discount_over_base <- renderPlotly(

  sku_prices_df %>%
  ungroup(game) %>%
  ggplot(aes(x = usd_price, y = `% Discount Over Base SKU`, group = game, color = game)) +
    geom_line() +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(labels = dollar) +
    labs(x = "SKU Price (USD)")

  )

output$segment_mau <- renderPlotly(

  merge_df %>%
    filter(metric == 'Monthly Active Users 3') %>%
    ggplot(aes(x = year_quarter, y = numeric, group = character, color = character)) +
    labs(title = 'MAUs', y = 'Average Quarterly MAU (millions)', x = '', color = "Segment") +
    geom_point() +
    geom_smooth(se = FALSE)

  )

output$segment_revenue <- renderPlotly(

  merge_df %>%
    filter(metric == 'Net Bookings 1') %>%
    ggplot(aes(x = year_quarter, y = numeric, group = character, color = character)) +
    labs(title = 'Quarterly Bookings', y = 'Quarterly Bookings (USD, millions)', x = '', color = "Segment") +
    geom_point() +
    geom_line() +
    geom_smooth(se = FALSE)

  )

}

shinyApp(ui = ui, server = server)
