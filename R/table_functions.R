
build_summary_table <- function(input_df, top_ticker) {
  
  summary_tbl <- input_df %>% 
    dplyr::filter(ticker == top_ticker,
                  type == "meta") %>% 
    tidyr::unnest(clean_data) %>% 
    dplyr::select(variable, value) %>% 
    dplyr::arrange(match(variable, c("Name", "Market", "Currency", "Market Cap", "Shares Outstanding", "Beta")), desc(value))
  
  summary_tbl
}