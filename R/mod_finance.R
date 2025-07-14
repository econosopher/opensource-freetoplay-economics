#' R/mod_finance.R
#'
#' @description
#' Shiny module for the main Finance tab. This module displays historical
#' share prices and market capitalization for the target companies.

# --- Module UI ---

#' finance_ui
#'
#' @param id A character string giving the namespace for the module.
#'
#' @return A UI definition for the Finance tab.
#'
finance_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "finance",
    h2("Comparative Financial Performance"),
    fluidRow(
      box(
        title = "Share Price (USD)",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput(ns("share_price_plot"))
      ),
      box(
        title = "% Change in Share Price (Indexed)",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput(ns("index_share_price_plot"))
      )
    ),
    fluidRow(
      box(
        title = "Historical Market Capitalization",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("market_cap_plot"))
      )
    )
  )
}

# --- Module Server ---

#' finance_server
#'
#' @param id A character string giving the namespace for the module.
#' @param tickers A reactive character vector of stock tickers.
#'
finance_server <- function(id, tickers) {
  moduleServer(id, function(input, output, session) {

    # --- Reactive Data ---
    price_data <- reactive({
      req(tickers())
      sfa_load_shareprices(
        ticker = tickers(),
        start = today() - years(5), # Fetch last 5 years of daily data
        end = today()
        )
    })

    # --- Outputs ---

    # Render share price plot
    output$share_price_plot <- renderPlotly({
        price_data() %>%
        ggplot(aes(x = `Date`, y = `Last Closing Price`, color = ticker)) +
        geom_line() +
        labs(y = "Share Price (USD, Daily Closing)", x = "Date", color = "Symbol") +
        scale_y_continuous(labels = dollar_format())
    })

    # Render indexed share price plot
    output$index_share_price_plot <- renderPlotly({
      price_data() %>%
        group_by(ticker) %>%
        mutate(
          index = first(`Last Closing Price`),
          index_pct_change = (`Last Closing Price` - index) / index
        ) %>%
        ggplot(aes(x = `Date`, y = index_pct_change, color = ticker)) +
        geom_line() +
        labs(y = "% Change from Start Date", x = "Date", color = "Symbol") +
        scale_y_continuous(labels = percent_format(accuracy = 1))
    })

    # Render market cap plot
    output$market_cap_plot <- renderPlotly({
      price_data() %>%
        mutate(market_cap = `Common Shares Outstanding` * `Last Closing Price`) %>%
        ggplot(aes(x = `Date`, y = market_cap, colour = ticker)) +
        geom_line() +
        scale_y_continuous(labels = dollar_format(suffix = "B", scale = 1e-9)) +
        labs(y = "Market Cap (USD, Billions)", x = "Date", colour = "Symbol")
    })

  })
} 