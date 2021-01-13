library(shiny)
library(magrittr)
library(shinyWidgets)
source("../R/table_functions.R")
source("../R/plot_functions.R")


full_data_df <- readRDS("./data/jse_top100.rds")
alsi_data_df <- readRDS("./data/alsi.rds")

price_data <- full_data_df %>% 
  dplyr::select(ticker, price_data)

fs_data <- full_data_df %>% 
  dplyr::select(ticker, bs_data, is_data)

# tickers and their names arranged by market cap
config <- full_data_df %>% 
  dplyr::arrange(desc(market_cap)) %>% 
  dplyr::select(ticker, short_name, long_name)

top100_tickers <- config$ticker

max_date_price <- price_data %>% 
  tidyr::unnest() %>%
  dplyr::filter(date == max(date, na.rm = TRUE)) %>% 
  dplyr::distinct(date) %>% 
  dplyr::pull(date)

min_date_price <- price_data %>% 
  tidyr::unnest() %>%
  dplyr::filter(date == min(date, na.rm = TRUE)) %>% 
  dplyr::distinct(date) %>% 
  dplyr::pull(date)

