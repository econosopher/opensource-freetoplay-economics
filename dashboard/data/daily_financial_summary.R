pacman::p_load('tidyquant', 'tidyverse', 'gt', 'scales')

what_metrics <-
  yahooQF(c(
    "Name (Long)",

    "Open",
    "Close",
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

tickers <- c(
  "EA",
  "ATVI",
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
  "PRXXF",
  "^GSPC"
  )

metrics <-
  getQuote(tickers, what = what_metrics) %>%
  as.data.frame() %>%
  rename(Firm = NameLong)

data_from <- today() - 1

tbl_daily_summary <-
metrics %>%
  rownames_to_column() %>%
  select(
    Ticker = rowname,
    Firm,
    `Market Capitalization`,
    Open,
    `% Change From 52-week High`,
    `% Change From 200-day MA`,
    `P/E Ratio`,
    `Earnings/Share`
  ) %>%
  mutate(
    Firm = if_else(Ticker == "^GSPC", "S&P 500", Firm),
    Region = case_when(
      Ticker %in% c("EA", "ATVI", "ZNGA", "GLUU",  "TTWO") ~ "North America",
      Ticker %in% c("PLTK", "THQQF", "UBSFY", "PRXXF", "OTGLY") ~ "Europe",
      Ticker %in% c("TCEHY", "NTDOY", "NTES",  "SQNXF", "CCOEF", "NCBDF", "NEXOF") ~ "Asia",
      TRUE ~ "Other"
    )
  ) %>%
  arrange(desc(`Market Capitalization`)) %>%
  gt(
    rowname_col = "Firm",
    groupname_col = "Region"
    ) %>%
  tab_header(
    title = md(""),
    subtitle = glue::glue("Data: {data_from}")
    ) %>%
  fmt_currency(
    columns = vars(Open, `P/E Ratio`, `Earnings/Share`),
    decimals = 0
  ) %>%
  fmt_currency(
    columns = vars(`Earnings/Share`),
    decimals = 2
  ) %>%
  fmt_currency(
    columns = vars(`Market Capitalization`),
    suffixing = TRUE
  ) %>%
  fmt_percent(
    columns = starts_with("%"),
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
    columns = vars(Firm, Ticker),
    align = "left"
    ) %>%
  data_color(
    columns = starts_with("%"),
    apply_to = "text",
    autocolor_text = TRUE,
    colors = scales::col_numeric(
      palette = c("red", "darkorange", "grey", "darkgreen"),
      domain = c(-.5, 0, .5)
      )
  )

tbl_daily_summary
