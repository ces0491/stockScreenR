get_jalsh <- function(freq, start_dt, end_dt) {
  
  period_params <- fdoR:::get_period_params(freq, start_dt, end_dt)
  
  url_base <- "https://za.investing.com/indices/ftse-jse-all-share-historical-data"
  url <- glue::glue("{url}?end_date={period_params$req_end}&interval_sec=daily&st_date={period_params$req_start}")
  
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
  
  jalsh_df <- dateR:::change_periodicity(jalsh_df, freq)
  
  jalsh_df
}

get_live_data <- function(ticker, freq, start_dt, end_dt) {
  
  ### get price data
  
  sp <- splashr::start_splash()
  
  pg <- splashr::render_html(url = 'https://finance.yahoo.com/quote/NTC.JO/history?p=NTC.JO')
  
  splashr::stop_splash(sp)
  
  y <- pg %>% 
    rvest::html_node(xpath='//*[@id="Col1-1-HistoricalDataTable-Proxy"]/section/div[2]/table') %>% 
    rvest::html_table()
  
  price_data <- fdoR::get_yahoo_data(ticker,
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
  
  fs_data <- fdoR::get_yahoo_data(ticker, page = c("is", "bs"))
  
  fs_data_df <- fs_data %>%
    dplyr::select(ticker, page, clean_data) %>%
    tidyr::spread(page, clean_data) %>%
    dplyr::rename(bs_data = bs,
                  is_data = is)
  
  ### combine
  
  result <- price_data_df %>%
    dplyr::left_join(fs_data_df, by = "ticker") 
  
  result
}