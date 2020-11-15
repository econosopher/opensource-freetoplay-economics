library(tidyverse)
library(fmpapi)
library(tidyquant)

api_key <- '923505d159c60cd4f88f475ff2ce0d6d'
fmp_api_key(api_key, overwrite = T)

# reload
readRenviron('~/.Renviron')

firm_symbols <- c('EA','ATVI', 'TTWO', 'ZNGA', '^IXIC')
stock_prices <- tq_get(firm_symbols, from = "2000-01-01", to = today())

saveRDS(stock_prices, file = "dashboard/data/stock_prices.rds")

market_cap <- fmp_key_metrics(firm_symbols[firm_symbols!= '^IXIC'], quarterly = TRUE)

saveRDS(market_cap, file = "dashboard/data/market_cap.rds")
