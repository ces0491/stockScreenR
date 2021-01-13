# define sidebar
sidebar <- shinydashboard::dashboardSidebar(
  
  shinyjs::useShinyjs(),
  
  width = 350,
  
  selectizeInput(inputId = "tickerSelect", 
                 label = "Select Ticker", 
                 choices = sort(top100_tickers), 
                 multiple = FALSE),
  
  dateRangeInput(inputId = "dateRange",
                 label = "Select Date Range",
                 start = Sys.Date() - 365,
                 end = max_date_price,
                 min = min_date_price,
                 max = max_date_price),
  
  selectizeInput(inputId = "freqSelect",
                 label = "Select Frequency",
                 choices = list("Daily" = "daily", "Weekly" = "weekly", "Monthly" = "monthly"),
                 selected = "Weekly",
                 multiple = FALSE),
  
  materialSwitch(inputId = "jseCompare",
                 label = "Compare series to JSE All Share Index",
                 value = FALSE,
                 status = "primary",
                 right = TRUE),
  
  hr(),
  
  column(12,align = "left",offset = 0, htmlOutput(outputId = "summDate")),
  
  tableOutput(outputId = "summaryBSTbl"),
  
  br(),
  
  tableOutput(outputId = "summaryMetricTbl"),
  
  hr(),
  
  column(12,align = "left",offset = 0, downloadButton(outputId = "downloadData", label = "Download", 
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  
)

# define body
body <- shinydashboard::dashboardBody(

  fluidPage(
    
    plotly::plotlyOutput("tickerTS"),
    
    hr(),
    
    fluidRow(
      column(4, plotly::plotlyOutput("mktCapRank")),
      column(4, plotly::plotlyOutput("debtEq")),
      column(4, plotly::plotlyOutput("priceBk"))
    )
  )
)

# Put them together into a dashboardPage
ui.stockScreenR <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = "JSE Top 100",
    titleWidth = 350),
  sidebar,
  body
)