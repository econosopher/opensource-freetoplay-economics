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


tbl_daily_summary <-
metrics_t %>%
  select(-'rown') %>%
  filter(!name %in% c('Trade Time')) %>%
  gt(rowname_col = "name", groupname_col = "group_label") %>%
  tab_header(title = md("Daily Financial Summary"),
             subtitle = glue::glue("Data: {data_from}")
             ) %>%
  tab_options(
    table.border.top.style = "none",
    table_body.border.top.style = "none",
    table_body.border.bottom.color = "#0000001A",
    table.font.size = 12,

    column_labels.font.size = 14,
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    column_labels.border.bottom.width = 1,
    column_labels.border.bottom.color = "#334422",

    row_group.border.right.style = "none",
    stub.border.style = "none",

    row_group.font.size = 14,

    data_row.padding = px(2)
  ) %>%
  cols_align(
    columns = vars(EA, ATVI, TTWO, ZNGA),
    align = "right"
    ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "#0000001A"
        ),
      cell_borders(
        sides = "bottom",
        color = "#FFFFFF"
        ),
      cell_text(
        weight = "bold"
        )
      ),
    locations = cells_stub(rows = TRUE)
    ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "#0000001A"
        ),
      cell_text(
        weight = "bolder"
        )
      ),
    locations = cells_row_groups()
    ) %>%
   opt_row_striping()