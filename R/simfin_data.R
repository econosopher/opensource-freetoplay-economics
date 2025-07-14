#' R/simfin_data.R
#'
#' @description
#' This script handles all interactions with the simfinapi package to retrieve
#' financial data for the Shiny dashboard.

# Load required packages
pacman::p_load(simfinapi, tidyverse)

#' Set SimFin API Key
#'
#' @description
#' Sets the SimFin API key from an environment variable.
#' It's recommended to store the API key in a .Renviron file in the project
#' root for security and ease of use.
#'
#' @return
#' Invisibly returns the API key.
#'
set_simfin_api_key <- function() {
  # Get the API key from the environment variable
  api_key <- Sys.getenv("SIMFIN_API_KEY")

  # Check if the API key is available
  if (api_key == "") {
    stop(
      "SimFin API key not found. Please set the 'SIMFIN_API_KEY' environment variable. ",
      "You can get a free key from https://app.simfin.com/login"
    )
  }

  # Set the API key for the simfinapi package
  sfa_set_api_key(api_key)

  # Invisibly return the key
  invisible(api_key)
}

#' Set SimFin Cache Directory
#'
#' @description
#' Sets the cache directory for simfinapi to a project-local directory.
#' This speeds up subsequent data requests by caching results locally.
#'
#' @return NULL
#'
set_simfin_cache <- function() {
  cache_dir <- "simfin_cache"
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }
  sfa_set_cache_dir(cache_dir)
}

#' Fetch Quarterly Earnings Data
#'
#' @description
#' Fetches quarterly income statements for a vector of company tickers.
#'
#' @param tickers A character vector of stock tickers (e.g., c("MSFT", "AAPL")).
#'
#' @return A tidy tibble with quarterly earnings data.
#'
fetch_quarterly_earnings <- function(tickers) {

  # Retrieve quarterly income statements
  earnings_data <- sfa_load_income(
    ticker = tickers,
    variant = "quarterly",
    period = c("Q1", "Q2", "Q3", "Q4")
    )

  # Return the tidy data
  return(earnings_data)
}

#' Fetch Market Capitalization Data
#'
#' @description
#' Fetches the latest daily market capitalization for a vector of tickers.
#'
#' @param tickers A character vector of stock tickers.
#'
#' @return A tidy tibble with market cap data.
#'
fetch_market_cap <- function(tickers) {

  # Retrieve latest share prices and outstanding shares
  market_data <- sfa_load_shareprices(
    ticker = tickers,
    variant = "latest"
  )

  # Calculate market cap and select relevant columns
  market_cap_data <- market_data %>%
    mutate(market_cap_usd = `Common Shares Outstanding` * `Last Closing Price`) %>%
    select(ticker, company_name, market_cap_usd)

  # Return the tidy data
  return(market_cap_data)
} 