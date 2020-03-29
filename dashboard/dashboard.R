library("shiny")
library("shinydashboard")
library("tidyverse")
library("DT")
library("plotly")
library("scales")

source("ui_elements/sidebar.R")
source("ui_elements/body.R")

ui <-
  dashboardPage(
    dashboardHeader(title = "F2P Economics"),
    sidebar,
    body
  )

server <- function(input, output) {

sku_prices <- "
game	usd_price	hard_currency	type
battlefield_5	5	500	currency
battlefield_5	10	1050	currency
battlefield_5	20	2200	currency
battlefield_5	30	3500	currency
battlefield_5	50	6000	currency
apex	10	1000	currency
apex	20	2150	currency
apex	40	4350	currency
apex	60	6700	currency
apex	100	11500	currency
overwatch	2	2	loot_boxes
overwatch	10	11	loot_boxes
overwatch	20	24	loot_boxes
overwatch	40	50	loot_boxes
r6:seige	5	600	currency
r6:seige	10	1200	currency
r6:seige	20	2670	currency
r6:seige	35	4920	currency
r6:seige	50	7560	currency
r6:seige	100	16000	currency
heroes_of_the_storm	5	500	currency
heroes_of_the_storm	10	1040	currency
heroes_of_the_storm	20	2100	currency
heroes_of_the_storm	35	3710	currency
heroes_of_the_storm	50	5500	currency
heroes_of_the_storm	100	11500	currency
league_of_legends	5	650	currency
league_of_legends	10	1380	currency
league_of_legends	20	2800	currency
league_of_legends	35	5000	currency
league_of_legends	50	7200	currency
league_of_legends	100	15000	currency
fortnite	10	1000	currency
fortnite	25	2800	currency
fortnite	40	5000	currency
fortnite	100	13500	currency
far_cry_5	5	500	currency
far_cry_5	10	1050	currency
far_cry_5	20	2400	currency
far_cry_5	35	4550	currency
far_cry_5	50	7250	currency
cod_ww2	2	200	currency
cod_ww2	10	1100	currency
cod_ww2	20	2400	currency
cod_ww2	40	5000	currency
cod_ww2	75	9500	currency
cod_ww2	100	13000	currency
"

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

}

shinyApp(ui = ui, server = server)
