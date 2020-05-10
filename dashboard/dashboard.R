library("shiny")
library("shinydashboard")
library("tidyverse")
library("DT")
library("plotly")
library("scales")

source("ui_elements/sidebar.R")
source("ui_elements/body.R")
source("data.R")

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

linear_df <- reactive({

  tibble(levels = seq(1, input$levels)) %>%
    mutate(marginal_hours = input$levels / input$hours,
           total_hours = cumsum(marginal_hours)
    )
})

exo_df <- reactive({

  tibble(levels = seq(1, input$levels)) %>%
    mutate(marginal_hours = input$levels / input$hours,
           total_hours = cumsum(marginal_hours)
    )
})

scurve <- function(x, ymin, ymax, x50L, x50U) {
    a = (x50L + x50U) / 2
    b = 2 / abs(x50L - x50U)
    c = ymin
    d = ymax - c
    y = c + ( d / ( 1 + exp( b * (x - a) ) ) )
    return(y)
 }

progression_df <- reactive({

level_sequence <-
  tibble(levels = seq(1, input$sequence_length/2)) %>%
  mutate(sigmoid_progression = scurve(levels, ymin = input$ymin, ymax = input$ymax, x50L = input$x50L, x50U = input$x50U)) %>%
  arrange(sigmoid_progression) %>%
  mutate(levels = seq(1, input$sequence_length/2)) %>%
  add_row(
    levels = seq((input$sequence_length/2)+1, input$sequence_length),
    sigmoid_progression= rev(.$sigmoid_progression)
    ) %>%
  .$sigmoid_progression

progression_df <-
  rep(level_sequence, times = (input$sequences)) %>%
  tibble::enframe(name = NULL) %>%
  mutate(
    levels = row_number(),
    total_time_to_level = cumsum(value),
    ) %>%
  rename(time_to_level = value)

})


output$progression_plot <- renderPlotly({

  progression_df() %>%
    ggplot(aes(x = levels, y = total_time_to_level)) +
      geom_line()

})

}

shinyApp(ui = ui, server = server)
