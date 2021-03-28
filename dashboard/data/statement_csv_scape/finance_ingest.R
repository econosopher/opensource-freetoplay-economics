pacman::p_load('tidyquant', 'tidyverse', 'gt', 'scales', 'fmpcloudr')

fmpc_set_token('923505d159c60cd4f88f475ff2ce0d6d')

firm_symbols <- c(
  "EA",
  "ATVI"  ,
  "TTWO",
  "ZNGA",
  "UBSFY",
  "GLUU",
  "NTDOY",
  "NTES",
  "TCEHY",
  "PLTK",
  "CCOEF",
  "OTGLY",
  "NEXOF",
  "NCBDF",
  "SQNXF",
  "THQQF",
  "PRXXF"
)

today <- ymd(today() - 4)

week_start <- floor_date(today, unit = "week") + 1

quarter_start <-
  floor_date(today, unit = "quarter") %>%
  floor_date("week", 1)

# Market doesn't open until the 4th
year_start <-
  floor_date(today, unit = "year") %>%
  floor_date("week", 1)

stock_prices <-
  tq_get(firm_symbols,  get = "stock.prices", from = year_start, to = today + 1)
  select(symbol, date, close, adjusted)

market_cap_list <- list()

for (symbol in firm_symbols){

  cap <- read_html(paste0("https://ycharts.com/companies/", symbol, "/market_cap"))

  tables <-
    cap %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)

  market_cap_list <- c(market_cap_list, tables)

}

  cap_df <-
  tables %>%
    bind_rows(tables) %>% # make a huge df
    select(1:3) %>%
    filter_all(any_vars(!is.na(.))) %>%
    mutate(
      `Data for this Date Range`= coalesce(`Data for this Date Range`, `...1`)
      ) %>%
    select(-`...1`, Date = `Data for this Date Range`, market_cap = `...2`)


  }

stock_prices %>%
  filter(date %in% c(today, week_start + 1, quarter_start, year_start)) %>%
  group_by(symbol) %>%
  mutate(
    WTD = (lag(close) - close) / close,
    QTD = (lag(close, 2) - close) / close,
    YTD = (lag(close, 3) - close) / close,
    market_cap = case_when(
      symbol == EA & Date
    ) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  gt() %>%
  cols_hide(columns = vars(date)) %>%
  tab_header(
    title = md("Daily Financial Summary")
    ) %>%
  fmt_currency(
    columns = vars(close)
  ) %>%
  fmt_percent(
    columns = ends_with("TD"),
    decimals = 0
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
    row_group.font.size = 14,
    data_row.padding = px(2)
  ) %>%
  cols_align(
    columns = vars(symbol),
    align = "left"
    ) %>%
  data_color(
    columns = ends_with("TD"),
    apply_to = "text",
    colors = scales::col_numeric(
      palette = c("darkred", "darkorange", "gold", "darkgreen"),
      domain = NULL
    )
  )

stock_prices

market_cap <- fmp_market_cap(firm_symbols, historical = TRUE)


saveRDS(stock_prices, file = "dashboard/data/stock_prices.rds")

market_cap <-
  fmp_key_metrics(firm_symbols, quarterly = TRUE) %>%
  select(symbol, market_cap, date)

saveRDS(market_cap, file = "dashboard/data/market_cap.rds")
