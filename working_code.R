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

# find the formating codes and specically the bold codes for each workbook
for (workbook in unique(quarterly_sheets$workbook)) {

  formats <-
    quarterly_sheets %>%
    filter(workbook = workbook) %>%
    xlsx_formats(workbook)

}