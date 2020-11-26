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

metrics_t <-
metrics %>%
  rownames_to_column(var = 'ticker') %>% #head()
  mutate(across(High:Low | starts_with('Change'), ~(formattable::currency(.)))) %>%
  mutate(across(`Market Capitalization`, ~(formattable::currency(.)))) %>%
  #mutate(across(P.:Low, ~(formattable::currency(.)))) %>%
  mutate(across(starts_with('%'), ~(formattable::percent(., digits = 1)))) %>%
  mutate(across(starts_with("WA", ignore.case = FALSE), ~(formattable::accounting(., format = 'd')))) %>%
  mutate_all(as.character) %>%
  pivot_longer(-ticker) %>%
  pivot_wider(names_from = ticker)


tbl_daily_summary <-
metrics_t %>%
  filter(row_labels != 'Trade Time') %>%
  gt(rowname_col = "name") %>%
  tab_header(title = md("Daily Financial Summary"))

# tab_row_group(
#   group = "Market Summary",
#   rows = 21:27
# ) %>%
# tab_row_group(
#   group = "200 Day Moving Averages",
#   rows = 18:20
# ) %>%
# tab_row_group(
#   group = "50 Day Moving Averages",
#   rows = 15:17
# ) %>%
# tab_row_group(
#   group = "52 Week Changes",
#   rows = 9:14
# ) %>%
# tab_row_group(
#   group = "Today's Summary",
#   rows = 2:8
# ) %>%
# tab_row_group(
#   group = "",
#   rows = 1
# ) %>%
# tab_style(
#   style = list(
#     cell_fill(color = "lightblue")
#     ),
#   locations = cells_body(
#     columns = vars(EA, ATVI, TTWO, ZNGA),
#     rows =  row_labels == 'Change' & row_labels> 3000000)
#   )
