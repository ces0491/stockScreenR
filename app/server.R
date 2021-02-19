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
  
  # retrieve live data
  data <- eventReactive(input$submitBtn, {
    
    reqd_tickers <- tickers()
    start_dt <- input$dateRange[1]
    end_dt <- input$dateRange[2]
    freq <- input$freqSelect
      
    future_promise({fdoR::get_equity_data(tickers = reqd_tickers, 
                                          type = c('price', 'BS', 'IS'), 
                                          start_date = start_dt, 
                                          end_date = end_dt, 
                                          frequency = freq)
      })
    })

  # create summary table - displays data for first ticker regardless of how many tickers are entered
  summ_data <- reactive({
  
    top_ticker <- stringr::str_split(tickers(), "-")[[1]][2] # get the first ticker from input tickerSelect and remove prefix
    
    data() %...>%
      dplyr::filter(ticker == top_ticker,
                    type == "meta") %...>%
      tidyr::unnest(clean_data) %...>%
      dplyr::select(variable, value) %...>%
      dplyr::arrange(match(variable, c("Name", "Market", "Currency", "Market Cap", "Shares Outstanding", "Beta")), desc(value))
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
        prepare_ts_data()
    
    })

    # render timeseries plot of prices
    output$tickerTS <- plotly::renderPlotly({
      plotly::plot_ly(ts_plot_data(), x = ~date, y = ~value, mode = 'lines', linetype = ~ticker)
    })

  # Downloadable xlsx of selected data
  output$downloadData <- downloadHandler(
    filename = function() {
      if(length(input$tickerSelect) > 1) {
        ticker <- "multi_ticker"
      } else {
        ticker <- input$tickerSelect
      }
      paste0(ticker,"_data_extract_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      
      file_names <- c("price_data", "bs_data", "is_data", "meta_data")
      
      for (fname in file_names) {
        reqd_ticker_data <- data() %...>%
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
  

  
  # return table of potential tickers by searching investing.com
  observeEvent(input$searchTicker_search, {
    
    output$searchTickerTbl <- DT::renderDT({
      nm <- input$searchTicker
      future_promise({fdoR::get_ticker_names(nm)}) %...>% #use promise to wait for the data to be available before rendering as DT
        DT::datatable(., options = list(dom = 't'))
    })
    
  })
}
