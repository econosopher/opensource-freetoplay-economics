what_metrics <-
  yahooQF(c(
    "Name (Long)",
    "Change",
    "Open",
    "Days High",
    "Days Low",
    "Volume",
    "Change in Percent",
    "Previous Close",
    "Change From 52-week Low",
    "Percent Change From 52-week Low",
    "Change From 52-week High",
    "Percent Change From 52-week High",
    "52-week Low",
    "52-week High",
    "50-day Moving Average",
    "Change From 50-day Moving Average",
    "Percent Change From 50-day Moving Average",
    "200-day Moving Average",
    "Change From 200-day Moving Average",
    "Percent Change From 200-day Moving Average",
    "Market Capitalization",
    "P/E Ratio",
    "Book Value",
    "Shares Outstanding",
    "Dividend/Share",
    "Dividend Yield",
    "Earnings/Share"
    ))

tickers <- c("EA", "ATVI", "TTWO", "ZNGA")

metrics <-
  getQuote(tickers, what = what_metrics) %>%
  as.data.frame()

# transpose data
metrics_t <-
  metrics %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "row_labels") %>%
  mutate_all(as.character)

column_names <- colnames(metrics_t)

# format data
format_specs <-
  data.frame(row = row_labels) %>% # name column with row labels
  mutate(
    type = case_when(
      str_detect(row, '%') ~ 'p',
      row %in% c('Shares Outstanding', 'Volume') ~ 'n',
      row %in% c('Trade Time', 'NameLong') ~ 'c',
      TRUE ~ 'd'
      ),
    decimals = case_when(
      type == 'p' ~ 1,
      type == 'd' ~ 4,
      TRUE ~ 0
      ),
    scale = case_when(
      row %in% c('Market Capitalization') ~ .000000001,
      row %in% c('Volume', 	'Shares Outstanding' ) ~ .000001,
      TRUE ~ 1
      ),
    accuracy = case_when(
       row %in% c('Market Capitalization') ~ 2,
       row %in% c('Volume', 	'Shares Outstanding' ) ~ 1,
       TRUE ~ .01
      ),
    suffix = case_when(
      row %in% c('Market Capitalization') ~ 'B',
      row %in% c('Volume', 	'Shares Outstanding' ) ~ 'M',
      TRUE ~ ''
      ),
    row = as.character(row)
    )

myfmt <- function(data, cols, row_spec) {
  reduce(row_spec$row, function(x, y) {
    row_spec <- filter(row_spec, row == y)
    fmt(x, columns = cols,
        rows = which(x[["_data"]][["row_labels"]] == y),
        fns = function(x) switch(row_spec$type,
                                 n = scales::number(as.numeric(x), accuracy = row_spec$accuracy, big.mark = "",
                                                    suffix = row_spec$suffix, scale = row_spec$scale),
                                 p = scales::percent(as.numeric(x), accuracy = 10^(-row_spec$decimals)),
                                 d = scales::dollar(as.numeric(x), accuracy = row_spec$accuracy,
                                                    negative_parens = TRUE, scale = row_spec$scale, suffix = row_spec$suffix)
                                 )
        )
    }, .init = data)
}

#tbl_daily_summary <-
metrics_t %>%
  filter(row_labels != 'Trade Time') %>%
  gt(rowname_col = "row_labels") %>%
  myfmt(vars(column_names), format_specs) %>%
  tab_header(title = md("Daily Financial Summary")) %>%
  tab_row_group(
    group = "Market Summary",
    rows = 21:27
  ) %>%
  tab_row_group(
    group = "200 Day Moving Averages",
    rows = 18:20
  ) %>%
  tab_row_group(
    group = "50 Day Moving Averages",
    rows = 15:17
  ) %>%
  tab_row_group(
    group = "52 Week Changes",
    rows = 9:14
  ) %>%
  tab_row_group(
    group = "Today's Summary",
    rows = 2:8
  ) %>%
  tab_row_group(
    group = "",
    rows = 1
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightblue")
      ),
    locations = cells_body(
      columns = vars(EA, ATVI, TTWO, ZNGA),
      rows =  row_labels == 'Change' & row_labels> 3000000)
    )
