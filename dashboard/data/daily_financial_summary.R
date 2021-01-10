what_metrics <-
  yahooQF(c(
    "Name (Long)",

    "Open",
    "Days High",
    "Days Low",

    "Volume",


    "52-week High",
    "52-week Low",
    "Percent Change From 52-week High",
    "Percent Change From 52-week Low",

    "50-day Moving Average",
    "Change From 50-day Moving Average",
    "Percent Change From 50-day Moving Average",

    "200-day Moving Average",
    "Change From 200-day Moving Average",
    "Percent Change From 200-day Moving Average",

    "Market Capitalization",
    "P/E Ratio",
    "Shares Outstanding",
    "Earnings/Share"
    ))

tickers <- c("EA", "ATVI", "TTWO", "ZNGA")

metrics <-
  getQuote(tickers, what = what_metrics) %>%
  as.data.frame() %>%
  rename(Firm = NameLong)

metrics_t <-
metrics %>%
  rownames_to_column(var = 'ticker') %>%
  mutate(across(Open:`Days Low` | starts_with("Change From") | "P/E Ratio" | contains('-'),  ~(formattable::currency(.)))) %>%
  mutate(across("Market Capitalization", ~scales::dollar(., suffix = 'B', scale = .000000001, trim = TRUE))) %>%
  mutate(across(starts_with('%'), ~(formattable::percent(., digits = 1)))) %>%
  mutate(across("Volume" | "Shares Outstanding", ~(formattable::accounting(., format = 'd')))) %>%
  mutate_all(as.character) %>%
  pivot_longer(-ticker) %>%
  pivot_wider(names_from = ticker) %>%
  mutate(
    rown = row_number(),
    group_label = case_when(
      #rown == 2 ~ "Firm",
      rown > 2 & rown <= 8 ~ "Today's Summary",
      rown >= 9 & rown <= 12 ~ "52-week Summary",
      rown >= 13 & rown <= 15 ~ "50-day Moving Averages",
      rown >= 16 & rown <= 18 ~ "200-day Moving Averages",
      rown >= 19 & rown <= 27 ~ "Topline Summary",
      TRUE ~ ''
      ))

data_from <- today()-1

#tbl_daily_summary <-
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


