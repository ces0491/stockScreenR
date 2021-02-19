
# define sidebar
sidebar <- shinydashboard::dashboardSidebar(
  
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Company Overview", tabName = "overview", icon = icon("chart-line")),
    shinydashboard::menuItem("Company Lookup", tabName = "search", icon = icon("search"))
  ),
  
  shinyjs::useShinyjs(),
  
  width = 350,
  
  textAreaInput(inputId = "tickerSelect", 
                label = "Select Ticker", 
                value = "",
                placeholder = "YAH-NTC.JO, INV-TSLA"),
  
  column(12, align = "left", offset = 0, 
         div(tags$div(HTML('<i class="fa fa-info-circle"></i> Remember to specify the source prefix for your tickers. Yahoo Finance = YAH-, investing.com = INV-. Tickers are seperated by a comma (,).')))
  ),
  
  br(),
  
  dateRangeInput(inputId = "dateRange",
                 label = "Select Date Range",
                 start = Sys.Date() - 365,
                 end = Sys.Date()),
  
  selectizeInput(inputId = "freqSelect",
                 label = "Select Frequency",
                 choices = list("Daily" = "daily", "Weekly" = "weekly", "Monthly" = "monthly", "Quarterly" = "quarterly"),
                 selected = "Daily",
                 multiple = FALSE),
  
  actionButton(inputId = "submitBtn", label = "Submit",
               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
  
  hr(),
  
  column(12, align = "left", offset = 0, div(tags$div(HTML('Summary Info')))),
  tableOutput(outputId = "summaryTbl") %>% withSpinner(),
  
  hr(),
  
  column(12, align = "left", offset = 0, 
         downloadButton(outputId = "downloadData", 
                        label = "Download", 
                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  
)

# define body
body <- shinydashboard::dashboardBody(
  
  shinydashboard::tabItems(
    shinydashboard::tabItem(tabName = "overview", h2("Overview"),
            
      fluidPage(
    
      plotly::plotlyOutput("tickerTS") %>% withSpinner(),
    
      hr(),
    
      fluidRow(
        column(6, plotly::plotlyOutput("isTbl") %>% withSpinner()),
        column(6, plotly::plotlyOutput("bsTbl") %>% withSpinner())
        )
      )
    ),
  
    shinydashboard::tabItem(tabName = "search", h2("Company Lookup"),
      fluidPage(
        
        searchInput(inputId = "searchTicker", 
                    label = "Search Investing.com for company identifier by name, ISIN or ticker",
                    placeholder = "Enter text...",
                    btnSearch = icon("search"),
                    btnReset = icon("remove")),
        
        div(tags$div(HTML('<i class="fa fa-info-circle"></i> Tip: For JSE tickers such as CLS (Clicks), Investing.com will append the suffix J while Yahoo Finance appends the suffix .JO, e.g. CLSJ for Investing.com and CLS.JO for Yahoo Finance. Most US tickers such as TSLA do not require a suffix.'))),
        
        DT::dataTableOutput(outputId = "searchTickerTbl") %>% withSpinner()
      )
    )
  )
)

# Put them together into a dashboardPage
ui.stockScreenR <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = "Stock Data Dash",
    titleWidth = 350),
  sidebar,
  body
)