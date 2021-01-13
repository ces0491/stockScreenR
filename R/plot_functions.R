switch_freq <- function(data, freq) {
  
  switch (freq,
          "daily" = data,
          "weekly" = dateR::to_weekly(data),
          "monthly" = dateR::to_monthly(data)
  )
}

plot_rank <- function(full_data_df, ticker) {
  
  if(length(ticker) > 1) ticker <- ticker[1]
  
  mkt_cap_df <- full_data_df %>% 
    dplyr::arrange(desc(market_cap)) %>% 
    dplyr::mutate(rank = dplyr::row_number()) %>% 
    dplyr::select(short_name, long_name, market_cap)
  
  tkr_df <- full_data_df %>%
    dplyr::arrange(desc(market_cap)) %>% 
    dplyr::mutate(rank = dplyr::row_number()) %>% 
    dplyr::filter(ticker == !!ticker) %>% 
    dplyr::select(short_name, long_name, market_cap)
  
  #convert to factor for sorting in chart
  mkt_cap_df$short_name <- factor(mkt_cap_df$short_name, levels = unique(mkt_cap_df$short_name)[order(mkt_cap_df$market_cap, decreasing = FALSE)])  
  tkr_df$short_name <- factor(tkr_df$short_name, levels = unique(tkr_df$short_name)[order(tkr_df$market_cap, decreasing = FALSE)])  
  
  mkt_cap_df$tooltip_txt <- paste('Name: ', mkt_cap_df$long_name,
                                  '<br> Rank: ', mkt_cap_df$rank,
                                  '<br> Market Cap: ', prettyNum(mkt_cap_df$market_cap/1000000, big.mark = ","), " million", sep = " ")
  
  tkr_df$tooltip_txt <- paste('Name: ', tkr_df$long_name,
                              '<br> Rank: ', tkr_df$rank,
                              '<br> Market Cap: ', prettyNum(tkr_df$market_cap/1000000, big.mark = ","), " million", sep = " ")
  
  fig <- plotly::plot_ly(data = mkt_cap_df,
                         showlegend = FALSE)
  fig <- fig %>% plotly::add_markers(x = ~log(market_cap), 
                                     y = ~short_name, 
                                     type = 'scatter', 
                                     mode = "markers+text",
                                     hoverinfo = "text", 
                                     text = ~tooltip_txt)
  fig <- fig %>% plotly::add_markers(data = tkr_df,
                                     x = ~log(market_cap), 
                                     y = ~short_name, 
                                     type = 'scatter', 
                                     mode = "markers+text",
                                     hoverinfo = "text", 
                                     text = ~tooltip_txt,
                                     colors = "orange",
                                     marker = list(size = 12))
  fig <- fig %>% plotly::layout(title = "Ranked Market Caps of JSE Top 100",
                                xaxis = list(title = "LN of Market Cap"),
                                yaxis = list(title = ""))
  fig
}

prep_price_data <- function(price_data, ticker, start_dt, end_dt, freq) {
    
  price_ts_data <- price_data %>% 
    dplyr::filter(ticker %in% !!ticker) %>%
    dplyr::group_by(ticker) %>% 
    dplyr::mutate(freq_adj = purrr::map(price_data, switch_freq, freq)) %>% 
    dplyr::ungroup() %>%
    tidyr::unnest(freq_adj) %>% 
    dplyr::filter(variable == "Close") %>% 
    dplyr::filter(date >= start_dt,
                  date <= end_dt)
  
  price_ts_data
}

prep_alsi_data <- function(alsi_data_df, start_dt, end_dt, freq) {
  
  alsi <- alsi_data_df %>% 
    dplyr::mutate(ticker = "JALSH") %>% 
    dplyr::select(ticker, date, variable, value) %>% 
    dplyr::filter(date >= start_dt,
                  date <= end_dt)
  
  alsi_ts_data <- switch_freq(alsi, freq)
  
  alsi_ts_data
}

prep_line_plot_data <- function(price_ts_data, alsi_ts_data = NULL) {
  
  if(is.null(alsi_ts_data)) {
    plot_data <- price_ts_data
  } else {
  
    alsi_ts_data <- dplyr::filter(alsi_ts_data, date >= min(price_ts_data$date))
  
    plot_data <- price_ts_data %>% 
      dplyr::bind_rows(alsi_ts_data) %>%
      dplyr::distinct() %>% 
      dplyr::group_by(ticker) %>%
      dplyr::arrange(date) %>% 
      dplyr::mutate(start_val = ifelse(date == min(date), value, NA)) %>% 
      tidyr::fill(start_val) %>% 
      dplyr::mutate(value = value / start_val - 1) %>% 
      dplyr::ungroup()  
  }
  
  plot_data
}

plot_line <- function(plot_data, compare_alsi) {
  
  if(compare_alsi) {
    y_title <- "relative value"  
  } else {
    y_title <- "value in thousands"
  }
  
  fig <- plotly::plot_ly(plot_data, x = ~date, y = ~value, mode = 'lines', linetype = ~ticker) 
  
  fig
  
}

prep_metric_data <- function(full_data_df) {
  
  mkt_cap <- full_data_df %>% 
    dplyr::select(ticker, market_cap) %>% 
    dplyr::mutate(market_cap = market_cap / 1000)
  
  bs_vars <- c("Current Debt", "Long Term Debt", "Accounts Payable", "Other long-term liabilities", "Cash And Cash Equivalents", "Total stockholders' equity")
  
  bs_data <- full_data_df %>% 
    dplyr::select(ticker, bs_data) %>% 
    tidyr::unnest() %>% 
    dplyr::group_by(ticker) %>% 
    dplyr::filter(date == max(date, na.rm = TRUE)) %>% 
    dplyr::filter(variable %in% bs_vars) %>% 
    dplyr::ungroup()
  
  nav <- bs_data %>% 
    dplyr::filter(variable == "Total stockholders' equity") %>% 
    dplyr::select(-date, -variable) %>% 
    dplyr::rename("nav" = "value")
  
  pb_df <- mkt_cap %>% 
    dplyr::left_join(nav, by = "ticker") %>% 
    dplyr::mutate(value = market_cap / nav) %>% 
    dplyr::mutate(variable = "Price/Book") %>% 
    dplyr::select(ticker, variable, value)
  
  de_df <- bs_data %>% 
    tidyr::spread(variable, value) %>% 
    dplyr::mutate(`Book Debt` = `Current Debt` + `Long Term Debt`) %>%
    dplyr::mutate(`Debt/Equity` = `Book Debt` / `Total stockholders' equity`) %>% 
    tidyr::gather(variable, value, -ticker, -date) %>% 
    dplyr::select(ticker, variable, value) %>% 
    dplyr::filter(variable == "Debt/Equity")
  
  metric_box_data <- de_df %>%
    dplyr::bind_rows(pb_df)
  
  metric_box_data
}

plot_box <- function(metric_box_data, ticker, metric) {
  
  plot_data <- metric_box_data %>% 
    dplyr::filter(variable == !!metric)
  
  fig <- plotly::plot_ly(plot_data,
                y = ~value,
                type = "box") %>%
    plotly::layout(title = metric)
  
  fig
}