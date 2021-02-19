getDataUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    textAreaInput(inputId = ns("tickerSelect"), 
                  label = "Select Ticker", 
                  value = "YAH-NTC.JO"),
    
    column(12, align = "left", offset = 0, 
           div(tags$div(HTML('<i class="fa fa-info-circle"></i> Remember to specify the source prefix for your tickers. Yahoo Finance = YAH-, investing.com = INV-')))
    ),
    
    br(),
    
    dateRangeInput(inputId = ns("dateRange"),
                   label = "Select Date Range",
                   start = Sys.Date() - 365,
                   end = Sys.Date()),
    
    selectizeInput(inputId = ns("freqSelect"),
                   label = "Select Frequency",
                   choices = list("Daily" = "daily", "Weekly" = "weekly", "Monthly" = "monthly", "Quarterly" = "quarterly"),
                   selected = "Daily",
                   multiple = FALSE),
    
  )
  
}

getDataServer <- function(id, tkrs) {

  moduleServer(
    id,
  
    function(input, output, session) {
      
      reqd_data <- reactive({
        fdoR::get_equity_data(tickers = tkrs, 
                              type = c('price', 'BS', 'IS'), 
                              start_date = input$dateRange[1], 
                              end_date = input$dateRange[2], 
                              frequency = input$freqSelect)
      })

      return(reqd_data)
    }
  )    
}