server.stockScreenR <- function(input, output, session) {
  
  output$summDate <- renderText({
   # the summary tables will only display the data for the first selected ticker 
    max_dt <- fs_data %>% 
      dplyr::filter(ticker %in% input$tickerSelect[[1]]) %>% 
      dplyr::select(bs_data) %>% 
      tidyr::unnest() %>% 
      dplyr::filter(date == max(date, na.rm = TRUE)) %>% 
      dplyr::pull(date)
    
    short_nm <- dplyr::filter(config, ticker == input$tickerSelect[[1]]) %>% 
      dplyr::pull(short_name)
    
    dt_txt <- paste0("Summary output for ", short_nm, " as of ", unique(max_dt), " (TTM)")
    HTML(paste(dt_txt, "Units in thousands", sep = "<br/>")) # we use this html output to make use of the linebreak tag
    
  })
  
  output$summaryBSTbl <- renderTable({
    summary_table <- build_summary_table(full_data_df, input$tickerSelect)
    dplyr::mutate(summary_table, value = prettyNum(value, big.mark = ","))
  }, 
  width = '100%',
  align = 'l',
  digits = 0)
  
  output$summaryMetricTbl <- renderTable({
    summary_metric_table <- build_summary_metric_table(full_data_df, input$tickerSelect)
    summary_metric_table
  }, 
  width = '100%',
  align = 'l',
  digits = 2)
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    reqd_ticker_data <- full_data_df %>%
      dplyr::filter(ticker %in% !!input$tickerSelect)
    reqd_ticker_data
  })
  
  # Downloadable xlsx of selected data
  output$downloadData <- downloadHandler(
    filename = function() {
      if(length(input$tickerSelect) > 1) {
        ticker <- input$tickerSelect[1]
        message(glue::glue("multiple tickers have been selected, only the first, {input$tickerSelect[1]}, will be written to xlsx")) 
      } else {
        ticker <- input$tickerSelect
      }
      paste0(ticker,"_data_extract_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      
      file_names <- c("price_data", "bs_data", "is_data")
      
      for (fname in file_names) {
        reqd_ticker_data <- full_data_df %>%
          dplyr::filter(ticker %in% !!ticker) %>% 
          dplyr::select(!!fname) %>% 
          tidyr::unnest()
        
          excelsioR::write_wb(r_data = reqd_ticker_data,
                              sheet_name = fname,
                              clear_sheet = TRUE,
                              wb = NULL,
                              file_directory = file,
                              save_wb = TRUE)
        }
      }  
  )
  
  output$tickerTS <- plotly::renderPlotly({
    
    if(input$jseCompare) {
      alsi_ts_data <- prep_alsi_data(alsi_data_df, start_dt = input$dateRange[1], end_dt = input$dateRange[2], input$freqSelect)
    } else {
      alsi_ts_data <- NULL
    }
    
    price_ts_data <- prep_price_data(price_data, 
                                     input$tickerSelect, 
                                     start_dt = input$dateRange[1], 
                                     end_dt = input$dateRange[2], 
                                     freq = input$freqSelect)
    
    plot_data <- prep_line_plot_data(price_ts_data, alsi_ts_data)
    
    plot_line(plot_data, input$jseCompare)
  })
  
  output$mktCapRank <- plotly::renderPlotly({
    plot_rank(full_data_df, input$tickerSelect)
  })
  
  output$debtEq <- plotly::renderPlotly({
    metric_box_data <- prep_metric_data(full_data_df)
    plot_box(metric_box_data, input$tickerSelect[1], "Debt/Equity")
  })
  
  output$priceBk <- plotly::renderPlotly({
    metric_box_data <- prep_metric_data(full_data_df)
    plot_box(metric_box_data, input$tickerSelect[1], "Price/Book")
  })
}
