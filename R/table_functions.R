calc_equity_vol <- function(price_data) {
  
}

calc_debt <- function(bs_data, mkt_cap) {
  
  debt_vars <- c("Current Debt", "Long Term Debt", "Accounts Payable", "Other long-term liabilities", "Cash And Cash Equivalents")
  
  missing_vars <- tibble::tibble(variable = setdiff(debt_vars, bs_data$variable),
                                 value = 0)
  
  wide_bs <- bs_data %>% 
    dplyr::filter(variable %in% debt_vars) %>%
    dplyr::select(-date) %>% 
    dplyr::bind_rows(missing_vars) %>% 
    tidyr::spread(variable, value)
  
  debt_df <- wide_bs %>% 
    dplyr::mutate(`Book Debt` = `Current Debt` + `Long Term Debt`) %>% 
    dplyr::mutate(`Debt Equivalents` = `Accounts Payable` + `Other long-term liabilities`) %>% 
    dplyr::mutate(`Net Debt and Equivalents` = `Book Debt` + `Debt Equivalents` - `Cash And Cash Equivalents`) %>% 
    dplyr::mutate(Leverage = (1 - mkt_cap) / (mkt_cap + `Book Debt` + `Debt Equivalents`)) %>% 
    dplyr::select(`Book Debt`, `Debt Equivalents`, `Cash And Cash Equivalents`, `Net Debt and Equivalents`, Leverage) %>% 
    tidyr::gather(variable, value)
  
  debt_df
}

build_summary_table <- function(full_data_df, ticker) {
  
  if(length(ticker) > 1) ticker <- ticker[1]
  
  reqd_ticker_data <- full_data_df %>%
    dplyr::arrange(desc(market_cap)) %>% 
    dplyr::mutate(rank = dplyr::row_number()) %>% 
    dplyr::filter(ticker %in% !!ticker)
  
  rank <- tibble::tibble(variable = "Rank",
                         value = dplyr::pull(reqd_ticker_data, rank))
  
  mkt_cap <- tibble::tibble(variable = "Market Cap",
                            value = dplyr::pull(reqd_ticker_data, market_cap)/1000)
  
  bs_vars <- c("Current Debt", "Long Term Debt", "Accounts Payable", "Other long-term liabilities", "Cash And Cash Equivalents", "Total stockholders' equity")
  
  bs_data <- reqd_ticker_data %>% 
    dplyr::select(bs_data) %>% 
    tidyr::unnest() %>% 
    dplyr::filter(date == max(date, na.rm = TRUE)) %>% 
    dplyr::filter(variable %in% bs_vars)
  
  debt_df <- calc_debt(bs_data, mkt_cap$value) %>% 
    dplyr::filter(variable != "Leverage")
  
  nav <- bs_data %>% 
    dplyr::filter(variable == "Total stockholders' equity") %>% 
    dplyr::select(-date) %>% 
    dplyr::mutate(variable = "NAV")
  
  summary_tbl <- mkt_cap %>%
    dplyr::bind_rows(rank) %>% 
    dplyr::bind_rows(debt_df) %>% 
    dplyr::bind_rows(nav)
  
  summary_tbl
}

build_summary_metric_table <- function(full_data_df, ticker) {
  
  if(length(ticker) > 1) ticker <- ticker[1]
  
  reqd_ticker_data <- full_data_df %>%
    dplyr::arrange(desc(market_cap)) %>% 
    dplyr::mutate(rank = dplyr::row_number()) %>% 
    dplyr::filter(ticker %in% !!ticker)
  
  mkt_cap <- tibble::tibble(variable = "Market Cap",
                            value = dplyr::pull(reqd_ticker_data, market_cap)/1000)
  
  bs_vars <- c("Current Debt", "Long Term Debt", "Accounts Payable", "Other long-term liabilities", "Cash And Cash Equivalents", "Total stockholders' equity")
  
  bs_data <- reqd_ticker_data %>% 
    dplyr::select(bs_data) %>% 
    tidyr::unnest() %>% 
    dplyr::filter(date == max(date, na.rm = TRUE)) %>% 
    dplyr::filter(variable %in% bs_vars)
  
  lev_df <- calc_debt(bs_data, mkt_cap$value) %>% 
    dplyr::filter(variable == "Leverage")
  
  nav <- bs_data %>% 
    dplyr::filter(variable == "Total stockholders' equity") %>% 
    dplyr::select(-date) %>% 
    dplyr::mutate(variable = "NAV")
  
  pb_df <- tibble::tibble(variable = "Price to Book",
                       value = mkt_cap$value / nav$value)
  
  summary_metric_tbl <- lev_df %>%
    dplyr::bind_rows(pb_df)
  
  summary_metric_tbl
}