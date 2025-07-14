#' R/mod_hard_currency.R
#'
#' @description
#' Shiny module for the Hard Currency tab. This module includes UI and server
#' logic to display SKU price comparisons and discounts.

# --- Module UI ---

#' hard_currency_ui
#'
#' @param id A character string giving the namespace for the module.
#'
#' @return A UI definition for the hard currency tab.
#'
hard_currency_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "hard_currency",
    h2("Hard Currency SKU Analysis"),
    fluidRow(
      box(
        title = "Discount Over Base SKU",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("discount_plot"))
      ),
      box(
        title = "Hard Currency SKU Details",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        gt_output(ns("currency_table"))
      )
    )
  )
}

# --- Module Server ---

#' hard_currency_server
#'
#' @param id A character string giving the namespace for the module.
#'
hard_currency_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # --- Data Preparation ---
    # This data is static and defined here to make the module self-contained.
    sku_prices_data_string <- "
    game usd_price hard_currency type
    battlefield_5 5 500 currency
    battlefield_5 10 1050 currency
    battlefield_5 20 2200 currency
    battlefield_5 30 3500 currency
    battlefield_5 50 6000 currency
    apex 10 1000 currency
    apex 20 2150 currency
    apex 40 4350 currency
    apex 60 6700 currency
    apex 100 11500 currency
    overwatch 2 2 loot_boxes
    overwatch 10 11 loot_boxes
    overwatch 20 24 loot_boxes
    overwatch 40 50 loot_boxes
    r6:seige 5 600 currency
    r6:seige 10 1200 currency
    r6:seige 20 2670 currency
    r6:seige 35 4920 currency
    r6:seige 50 7560 currency
    r6:seige 100 16000 currency
    heroes_of_the_storm 5 500 currency
    heroes_of_the_storm 10 1040 currency
    heroes_of_the_storm 20 2100 currency
    heroes_of_the_storm 35 3710 currency
    heroes_of_the_storm 50 5500 currency
    heroes_of_the_storm 100 11500 currency
    league_of_legends 5 650 currency
    league_of_legends 10 1380 currency
    league_of_legends 20 2800 currency
    league_of_legends 35 5000 currency
    league_of_legends 50 7200 currency
    league_of_legends 100 15000 currency
    fortnite 10 1000 currency
    fortnite 25 2800 currency
    fortnite 40 5000 currency
    fortnite 100 13500 currency
    far_cry_5 5 500 currency
    far_cry_5 10 1050 currency
    far_cry_5 20 2400 currency
    far_cry_5 35 4550 currency
    far_cry_5 50 7250 currency
    cod_ww2 2 200 currency
    cod_ww2 10 1100 currency
    cod_ww2 20 2400 currency
    cod_ww2 40 5000 currency
    cod_ww2 75 9500 currency
    cod_ww2 100 13000 currency
    "

    sku_prices_df <-
      read.table(text = sku_prices_data_string, header = T) %>%
      group_by(game) %>%
      mutate(
        `$1 USD Buys X HC` = hard_currency / usd_price,
        `Min EX Rate` = min(`$1 USD Buys X HC`),
        `% Discount Over Base SKU` = (`$1 USD Buys X HC` - `Min EX Rate`) / `Min EX Rate`
      )

    # --- Outputs ---

    # Render the discount plot
    output$discount_plot <- renderPlotly({
      p <- sku_prices_df %>%
        ggplot(aes(
          x = usd_price,
          y = `% Discount Over Base SKU`,
          color = game,
          text = paste(
            "Game:", game,
            "<br>SKU Price:", dollar(usd_price),
            "<br>Discount:", percent(`% Discount Over Base SKU`, accuracy = 0.1)
          )
        )) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 2) +
        scale_y_continuous(labels = percent) +
        scale_x_continuous(labels = dollar) +
        scale_color_viridis_d(option = "C") +
        labs(
          title = "Bonus Hard Currency by SKU Price Point",
          subtitle = "Discount calculated against the lowest 'exchange rate' SKU for each game.",
          x = "SKU Price (USD)",
          y = "% Discount vs. Base SKU",
          color = "Game"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")

      ggplotly(p, tooltip = "text")
    })

    # Render the currency table using gt
    output$currency_table <- render_gt({
      sku_prices_df %>%
        select(game, usd_price, hard_currency, `$1 USD Buys X HC`, `% Discount Over Base SKU`) %>%
        gt() %>%
        tab_header(title = "Virtual Currency Exchange Rates") %>%
        fmt_currency(columns = vars(usd_price), currency = "USD") %>%
        fmt_number(columns = vars(hard_currency, `$1 USD Buys X HC`), decimals = 2) %>%
        fmt_percent(columns = vars(`% Discount Over Base SKU`), decimals = 2) %>%
        cols_label(
          game = "Game",
          usd_price = "Price (USD)",
          hard_currency = "Hard Currency",
          `$1 USD Buys X HC` = "$1 USD Buys X HC",
          `% Discount Over Base SKU` = "% Discount Over Base"
        )
    })

  })
} 