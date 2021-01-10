library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggthemes)
library(tidyverse)
library(DT)
library(plotly)
library(scales)
library(tsibble)
library(stringr)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(tidyquant)
library(gt)
library(glue)

source("ui_elements/sidebar.R")
source("ui_elements/body.R")
source("data/raw_data.R")
source("data/import_finance_atvi.R")
source("data/import_finance_ea.R")
source("data/daily_financial_summary.R")
#source("../../sdi-analyst-etls/compendium/cross_game/table.R")

rep_clean <- theme_clean() %+replace%
  theme(legend.position = "bottom")

theme_set(rep_clean)

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
    ggplot(aes(x = year_quarter, y = numeric, color = character)) +
      labs(y = 'Average Quarterly MAU (millions)', x = '', color = "Segment") +
      geom_point(alpha = 1/2) +
      geom_smooth(se = FALSE) +
      theme(legend.position = "bottom")

  )

output$segment_revenue <- renderPlotly(

  merge_df %>%
    filter(metric == 'Segment net revenues:') %>%
    ggplot(aes(x = year_quarter, y = numeric, color = character)) +
      labs(y = 'Quartery Revenue (USD)', x = '', color = "Segment") +
      scale_y_continuous(labels = dollar_format()) +
      geom_point(alpha = 1/2) +
      geom_smooth(se = FALSE)

)

output$segment_arpu <- renderPlotly(

  merge_df %>%
    filter(metric %in% c('Monthly Active Users 3', 'Segment net revenues:')) %>%
    mutate(
      character = case_when(
        character == 'Total MAUs' ~ 'Total',
        character == 'Reportable segments total' ~ 'Total',
        TRUE ~ character
        )) %>%
    select(-sheet) %>%
    pivot_wider(names_from = metric, values_from = numeric) %>%
    mutate(qARPU = `Segment net revenues:` / (`Monthly Active Users 3`*3)) %>%
    ggplot(aes(x = year_quarter, y = qARPU, color = character)) +
      labs(y = 'Quartery ARPU (USD)', x = '', color = "Segment") +
      scale_y_continuous(labels = dollar_format()) +
      geom_point(alpha = 1/2) +
      geom_smooth(se = FALSE)

)

output$bookings <- renderPlotly(

  merge_df %>%
    filter(metric == 'Net Bookings 1') %>%
    ggplot(aes(x = year_quarter, y = numeric, color = character)) +
      labs(y = 'Quarterly Bookings (USD, millions)', x = '', color = "Segment") +
      geom_point(alpha = 1/2) +
      geom_smooth(se = FALSE)

  )

output$distribution <- renderPlotly(

  merge_df %>%
    filter(sheet == 'Rev Mix by Distribution',
           metric == 'Net Revenues by Distribution Channel',
           character != 'Total consolidated net revenues '
           ) %>%
    mutate(year_quarter = as.character(year_quarter)) %>%
    group_by(character, year_quarter) %>%
    summarise(revenue = sum(numeric)) %>%
    ggplot(aes(x = year_quarter, y = revenue, group = character, fill = character)) +
      geom_bar(position = "fill", stat = 'identity') +
      labs(x = '', y = 'Revenue Share', fill = 'Segment') +
      scale_y_continuous(labels = percent_format())

  )

output$region <- renderPlotly(

  merge_df %>%
    filter(sheet == 'Rev Mix by Geographic Region',
           character %in% c('Americas', 'Asia Pacific', 'EMEA1')) %>%
    group_by(character, year_quarter) %>%
    mutate(year_quarter = as.character(year_quarter)) %>%
    summarise(revenue = sum(numeric)) %>%
    ggplot(aes(y = revenue, x = year_quarter, fill = character)) +
      geom_bar(position = "fill", stat = "identity") +
      labs(x = '', y = 'Revenue Share', fill = 'Segment') +
      scale_y_continuous(labels = percent_format())

  )

metric_picker_df <- reactive({

  merge_df %>%
    filter(sheet == input$sheet, metric == input$metric)

 })

output$metric_picker <- renderPlotly({

  metric_picker_df() %>%
  ggplot(aes(x = year_quarter, y = numeric, color = character)) +
    geom_point(alpha = 1/2) +
    geom_line(se = FALSE)

 })

stock_prices <- readRDS(file = "data/stock_prices.rds")
market_cap <- readRDS(file = "data/market_cap.rds")

output$share_price <- renderPlotly(

  stock_prices %>%
    filter(symbol != '^IXIC') %>%
    ggplot(aes(x = date, y = close, color = symbol)) +
      geom_point(alpha = 1/100) +
      geom_line(se = FALSE) +
      labs(x = '', y = 'Share Price (USD, Daily Closing)', color = 'Symbol') +
      scale_y_continuous(labels = dollar_format())

)

output$index_share_price <- renderPlotly(

  stock_prices %>%
    mutate(symbol = if_else(symbol == '^IXIC', 'S&P 500', symbol)) %>%
    group_by(symbol) %>%
    mutate(index = first(close),
           index_pct_change = (close - index)/index
           ) %>%
    ggplot(aes(x = date, y = index_pct_change, color = symbol)) +
      geom_point(alpha = 1/100) +
      geom_line(se = FALSE) +
      labs(x = '', y = '% Change (from 2000-01-01 Share Price)', color = 'Symbol') +
      scale_y_continuous(labels = percent_format(big.mark = ','))

)


output$market_cap <- renderPlotly(

  market_cap %>%
  ggplot(aes(x = date, y = market_cap, colour = symbol)) +
    geom_point(alpha = 1/2) +
    geom_line(se = FALSE) +
    scale_y_continuous(labels = dollar_format(suffix = "B", scale = .000000001, accuracy = 2)) +
    labs(y = "Market Cap (USD, Billions)", x = '', colour = 'Symbol')
)

output$composition_bookings <- renderPlotly(

  merge_df_ea %>%
    filter(metric == 'NET REVENUE BY COMPOSITION', numeric > 1) %>%
    ggplot(aes(x = year_quarter, y = numeric, color = character)) +
      labs(y = 'Quarterly Bookings (millions)', x = '', color = "Segment") +
      geom_point(alpha = 1/2) +
      geom_smooth(se = FALSE) +
      scale_y_continuous(labels = dollar_format(suffix = "M", scale = .1, accuracy = 2))

  )

output$composition_bookings_share <- renderPlotly(

  merge_df_ea %>%
    filter(metric == 'NET REVENUE BY COMPOSITION', numeric > 1, character != 'Total net revenue') %>%
    mutate(year_quarter = as.character(year_quarter)) %>%
    ggplot(aes(x = year_quarter, y = numeric, fill = character)) +
      geom_bar(position = "fill", stat = "identity") +
      labs(x = '', y = 'Revenue Share', fill = 'Segment') +
      scale_y_continuous(labels = percent_format())

  )

output$composition_platform <- renderPlotly(

  merge_df_ea %>%
    filter(metric == 'NET REVENUE BY PLATFORM ') %>%
    ggplot(aes(x = year_quarter, y = numeric, color = character)) +
      labs(y = 'Quarterly Bookings (millions)', x = '', color = "Segment") +
      geom_point(alpha = 1/2) +
      geom_smooth(se = FALSE) +
      scale_y_continuous(labels = dollar_format(suffix = "M", scale = .1, accuracy = 2))

  )

output$composition_platform_share <- renderPlotly(

  merge_df_ea %>%
    filter(metric == 'NET REVENUE BY PLATFORM ', character != 'Total net revenue') %>%
    mutate(year_quarter = as.character(year_quarter)) %>%
    ggplot(aes(x = year_quarter, y = numeric, fill = character)) +
      geom_bar(position = "fill", stat = "identity") +
      labs(x = '', y = 'Revenue Share', fill = 'Segment') +
      scale_y_continuous(labels = percent_format())

  )

output$daily_summary <- render_gt(tbl_daily_summary)
output$weekly_cross_game_gt <- render_gt(tbl_weekly_cross_game)

}

shinyApp(ui = ui, server = server)

