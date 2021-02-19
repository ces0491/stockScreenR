# This script will scrape all available price, income statement and balance sheet data for the JSE top 100 from Yahoo Finance.
# Be cautious before executing - APPROX 2 HOURS TO RUN

# TODO: caching function to only retrieve data not present in jse_top100.rds

start <- Sys.time()

### get JSE ALSI data
# to get latest data, just make sure you use the url that contains that data
# append the latest to the alsi rds file

end_dt <- Sys.Date()-1
start_dt <- end_dt - 730 # get the last 2 years
freq <- "daily"

period_params <- fractalDataImportR:::get_period_params(freq, start_dt, end_dt)

url_base <- "https://za.investing.com/indices/ftse-jse-all-share-historical-data"
url <- glue::glue("{url}?end_date={period_params$req_end}&interval_sec={freq}&st_date={period_params$req_start}")

jalsh <- url %>%
  xml2::read_html() %>%
  rvest::html_nodes(xpath='//*[@id="js-main-container"]/section[2]/div/section[2]/section[2]/div[1]/div/table') %>%
  rvest::html_table()
jalsh <- jalsh[[1]]

jalsh_df <- jalsh %>% 
  dplyr::mutate(Price = stringr::str_remove_all(Price, ",")) %>% 
  dplyr::mutate(Price = as.numeric(Price)) %>% 
  dplyr::rename(value = Price) %>% 
  dplyr::mutate(day = substr(Date, 5, 6), 
                month = substr(Date, 1, 3),
                year = substr(Date, 9, 12)) %>%
  dplyr::mutate(date = as.Date(paste(year,month,day, sep = "-"), format = "%Y-%b-%d")) %>% 
  dplyr::mutate(variable = "Close") %>% 
  dplyr::select(date, variable, value)

#####################################################################################################################

### get top 100 stocks by market cap
url <- "https://www.sashares.co.za/top-100-jse-companies/"

### get JSE top40 - this table has no short names for some reason
# url <- "https://www.sashares.co.za/jse-top-40/#gs.4rspss"



file_name <- paste0("jse_top100_", format(Sys.Date(), "%Y%m%d"))

jse_co_raw <- url %>%
  xml2::read_html() %>%
  rvest::html_nodes(xpath='//*[@id="table_1"]') %>%
  rvest::html_table()

jse_co_raw <- jse_co_raw[[1]]

reqd_cols <- c("JSE Code", "Short Name", "Full Name", "Market Cap")

fractalAssert::assert_allpresent(names(jse_co_raw), reqd_cols)

jse_companies_df <- jse_co_raw %>%
  dplyr::select(reqd_cols) %>%
  dplyr::rename(ticker = `JSE Code`,
                short_name = `Short Name`,
                long_name = `Full Name`,
                market_cap = `Market Cap`) %>%
  dplyr::mutate(market_cap = stringr::str_remove_all(market_cap, ",")) %>%
  dplyr::mutate(market_cap = as.numeric(market_cap)) %>%
  dplyr::mutate(ticker = paste0(ticker, ".JO")) %>%
  tidyr::drop_na() %>%
  dplyr::arrange(desc(market_cap))

# fractalAssert::assert_true(nrow(jse_companies_df) == 100, "100 rows required")

### get price data

price_data <- fractalDataImportR::get_yahoo_data(jse_companies_df$ticker,
                                                 page = "price",
                                                 frequency = freq,
                                                 start_date = start_dt,
                                                 end_date = end_dt)

price_data_df <- price_data %>%
  dplyr::select(ticker, clean_data) %>%
  tidyr::unnest(clean_data) %>%
  dplyr::group_by(variable) %>%
  tidyr::fill(value, .direction = "down") %>%
  dplyr::ungroup() %>%
  tidyr::nest(-ticker, .key = "price_data") %>%
  dplyr::select(ticker, price_data)

### get financial statement data

fs_data <- fractalDataImportR::get_yahoo_data(jse_companies_df$ticker, page = c("is", "bs"))

fs_data_df <- fs_data %>%
  dplyr::select(ticker, page, clean_data) %>%
  tidyr::spread(page, clean_data) %>%
  dplyr::rename(bs_data = bs,
                is_data = is)

### combine

result <- price_data_df %>%
  dplyr::left_join(fs_data_df, by = "ticker") %>%
  dplyr::left_join(jse_companies_df, by = "ticker")

### diagnostics

missing_data_tickers <- setdiff(jse_companies_df$ticker, result$ticker)
cat("The following tickers returned no data from Yahoo:", missing_data_tickers)

end <- Sys.time()
elapsed <- end - start
message("total run time = ", elapsed, "hours")

save_path <- paste0("./inst/extdata/", file_name, ".rds")

### save to file
print("saving to RDS...")
saveRDS(result, file = save_path)
print("complete")