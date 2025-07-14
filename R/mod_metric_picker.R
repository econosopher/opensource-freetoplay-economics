#' R/mod_metric_picker.R
#'
#' @description
#' Shiny module for the Metric Picker tab. Allows users to dynamically
#' select and plot any financial metric from the available statements.

# --- Module UI ---

#' metric_picker_ui
#'
#' @param id A character string giving the namespace for the module.
#'
#' @return A UI definition for the Metric Picker tab.
#'
metric_picker_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "metric_picker",
    h2("Dynamic Metric Explorer"),
    fluidRow(
      # Dynamic UI for dropdowns will be rendered from the server
      box(
        title = "Controls",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        uiOutput(ns("statement_selector_ui")),
        uiOutput(ns("metric_selector_ui"))
      )
    ),
    fluidRow(
      box(
        title = "Metric Plot",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("metric_plot"))
      )
    )
  )
}

# --- Module Server ---

#' metric_picker_server
#'
#' @param id A character string giving the namespace for the module.
#' @param tickers A reactive character vector of stock tickers.
#'
metric_picker_server <- function(id, tickers) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # --- Reactive Data ---
    all_statements_data <- reactive({
      req(tickers())
      sfa_load_statements_all(ticker = tickers())
    })

    # --- Dynamic UI ---

    # Render the dropdown for selecting the financial statement
    output$statement_selector_ui <- renderUI({
      choices <- unique(all_statements_data()$statement)
      selectizeInput(ns("selected_statement"),
        label = h3("Financial Statement"),
        choices = choices,
        selected = "Income Statement"
      )
    })

    # Render the dropdown for selecting the metric/account
    output$metric_selector_ui <- renderUI({
      req(input$selected_statement)
      filtered_data <- all_statements_data() %>%
        filter(statement == input$selected_statement)
      choices <- unique(filtered_data$account)
      selectizeInput(ns("selected_metric"),
        label = h3("Metric"),
        choices = choices,
        selected = "Revenue"
      )
    })
    
    # --- Filtered Data for Plot ---
    
    plot_data <- reactive({
        req(input$selected_statement, input$selected_metric)
        all_statements_data() %>%
            filter(
                statement == input$selected_statement,
                account == input$selected_metric
                )
    })

    # --- Plot Output ---
    output$metric_plot <- renderPlotly({
      req(plot_data())
      if (nrow(plot_data()) == 0) return(NULL)

      p <- ggplot(plot_data(), aes(
        x = report_date, 
        y = value, 
        color = ticker,
        text = paste(
          "Date:", report_date,
          "<br>Value:", dollar(value),
          "<br>Ticker:", ticker
        )
      )) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_color_viridis_d(option = "D") +
        scale_y_continuous(labels = dollar_format()) +
        labs(
          title = paste("Trend for", input$selected_metric),
          subtitle = paste("Source: SimFin API. Statement:", input$selected_statement),
          y = "Value (USD)",
          x = "Date",
          color = "Ticker"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
        
      ggplotly(p, tooltip = "text")
    })

  })
} 