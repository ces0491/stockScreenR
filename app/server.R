server.stockScreenR <- function(input, output, session) {
  
  # don't render outputs initially. a relevant reactive event specified later should trigger rendering
  output$searchTickerTbl <- DT::renderDT(NULL)
  output$summaryTbl <- renderTable(NULL)
  output$isTbl <- DT::renderDT(NULL)
  output$bsTbl <- DT::renderDT(NULL)
  output$tickerTS <- plotly::renderPlotly(NULL)
  
  # create a character vector from the single string input in tickerSelect
  tickers <- eventReactive(input$submitBtn, {
    stringr::str_split(stringr::str_replace_all(input$tickerSelect, "\\s", ""), ",")[[1]]
  })
  
  top_ticker <- reactive({
    stringr::str_split(tickers(), "-")[[1]][2] # get the first ticker from input tickerSelect and remove prefix
  })
  
  # retrieve live data
  data <- eventReactive(input$submitBtn, {
    
    reqd_tickers <- tickers()
    start_dt <- input$dateRange[1]
    end_dt <- input$dateRange[2]
    freq <- input$freqSelect
      
    future_promise({fdoR::get_equity_data(tickers = reqd_tickers, 
                                          type = c('price', 'IS', 'BS'), 
                                          start_date = start_dt, 
                                          end_date = end_dt, 
                                          frequency = freq)
      })
    })

  # create summary table - displays data for first ticker regardless of how many tickers are entered
  summ_data <- reactive({
  
    top_ticker <- top_ticker()
    
    data() %...>%
      build_summary_table(., top_ticker)
  })
  
    # render summary table
    output$summaryTbl <- renderTable({summ_data()},
                                     striped = FALSE,
                                     hover = FALSE,
                                     bordered = TRUE, 
                                     width = '100%', 
                                     align = 'l',
                                     digits = 0,
                                     rownames = FALSE,
                                     colnames = FALSE)
  
    # prepare timeseries plot data
    ts_plot_data <- reactive({
      
      price <- data() %...>%
        dplyr::filter(type == "price") %...>%
        tidyr::unnest(clean_data) %...>%
        dplyr::filter(variable == 'AdjClose' | variable == 'Price') %...>% # Yahoo returns closes, investing returns pricex
        dplyr::select(ticker, date, variable, value) %...>%
        prepare_ts_data(., input$freqSelect)
    
    })

    # render timeseries plot of prices
    output$tickerTS <- plotly::renderPlotly({
      
      ttl <- "Historical Price Timeseries"
      x <- list(title = "")
      y <- list(title = "")
      
      ts_plot_data() %...>%
        plotly::plot_ly(., x = ~date, y = ~plot_value, mode = 'lines', linetype = ~ticker,
                        text = ~ticker,
                        hovertemplate = paste0("<b>%{text}</b><br>",
                          "Date: %{x}<br>",
                          "Price: %{y}<br>",
                          "<extra></extra>")
                        ) %...>% 
        plotly::layout(title = ttl, xaxis = x, yaxis = y) %...>%
        plotly::rangeslider(start = input$dateRange[1], end = input$dateRange[2])
      
    })

    # get is data
    is_data <- reactive({
      
      top_ticker <- top_ticker()
      
      data() %...>%
        dplyr::filter(type == "IS") %...>%
        tidyr::unnest(clean_data) %...>%
        dplyr::select(ticker, date, variable, value) %...>%
        dplyr::group_by(ticker) %...>% 
        dplyr::arrange(desc(date), .by_group = TRUE) %...>% 
        dplyr::ungroup() %...>%
        tidyr::spread(date, value) %...>%
        dplyr::rename("Income Statement Items" = "variable")
    })
    
    # render income statement table
    output$isTbl <- DT::renderDT({
      is_data() %...>%
        DT::datatable(., rownames = FALSE)
      })
    
    
    # get bs data
    bs_data <- reactive({
      
      top_ticker <- top_ticker()
      
      data() %...>%
        dplyr::filter(type == "BS") %...>%
        tidyr::unnest(clean_data) %...>%
        dplyr::select(ticker, date, variable, value) %...>%
        dplyr::group_by(ticker) %...>% 
        dplyr::arrange(desc(date), .by_group = TRUE) %...>% 
        dplyr::ungroup() %...>%
        tidyr::spread(date, value) %...>%
        dplyr::rename("Balance Sheet Items" = "variable")
    })
    
    # render balance sheet
    output$bsTbl <- DT::renderDT({
      bs_data() %...>% 
        DT::datatable(., rownames = FALSE)
      })
    
  # Downloadable xlsx of selected data
  output$downloadData <- downloadHandler(
    
    filename = function() {
      if(length(tickers()) > 1) {
        ticker <- "multi_ticker"
      } else {
        ticker <- top_ticker()
      }
      paste0(ticker,"_data_extract_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    
    content = function(file) {
      
      export_list <- data() %...>%
        tidyr::unite("id", c(ticker, type)) %...>%
        dplyr::select(id, clean_data) %...>%
        tibble::deframe()
      
      export_list %...>%   
        excelsioR::write_wb(r_data = .,
                            clear_sheet = TRUE,
                            wb_dir = file,
                            save_wb = TRUE)
      }  
  )
  
  DT::renderDT({
    nm <- input$searchTicker
    future_promise({fdoR::get_ticker_names(nm)}) %...>% # use promise to wait for the data to be available before rendering as DT
      DT::datatable(., options = list(dom = 't'))
  })
  
  # return table of potential tickers by searching investing.com
  observeEvent(input$searchTicker_search, {
    
    output$searchTickerTbl <- DT::renderDT({
      nm <- input$searchTicker
      future_promise({fdoR::get_ticker_names(nm)}) %...>% #use promise to wait for the data to be available before rendering as DT
        DT::datatable(., options = list(dom = 't'))
    })
    
  })
}
