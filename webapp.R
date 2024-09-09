library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(kableExtra)
library(ggplot2)
library(plotly)
library(fmpcloudr)
library(openai)
library(lubridate)
library(Cairo)
library(bslib)
library(shinyauthr)
library(RSQLite)
library(sodium)
library(thematic)
library(shinybrowser)


# load API keys
fmpc_set_token(Sys.getenv("API_FMPC"))
Sys.setenv(OPENAI_API_KEY = Sys.getenv("API_OAI"))

# # shiny Cairo
# options(
#   shiny.usecairo = T,
#   cairo_antialias = 'subpixel',
#   dpi = 150,
#   scipen = 999
# )

thematic_shiny(font = "auto")

# load functions
source('webfunctions.R')
shinyjs:::useShinyjs()
# 
# autosuggest_ <- fmpc_symbols_available() %>% filter(type == "stock" &
#                                                       exchangeShortName %in% c("NYSE", "NASDAQ")) %>% pull(symbol)
# write.csv(autosuggest_, 'availab_symbols.csv')


autosuggest_ <- read.csv('www/availab_symbols.csv') %>% pull(x)
body_width <- 8

# start <- Sys.time()
# 
# fmpc_price_history('AAPL', startDate = -1) %>% select(date, close) %>% mutate(close = round(close,2))
# 
# Sys.time() - start


#### shiny UI ----
ui <- fluidPage(

  tags$head(tags$style(
    HTML(
      "
      @media (min-width: 1340px) {
        .navbar-nav {
          width: 100%;
        }
        .navbar-nav > li:nth-child(9) {
          margin-left: auto;
          margin-right: 10px;
          
        }
      }
      .navbar-nav > li:nth-child(9) > a {
        color: red !important;
      }
      .dashboard-box { border: 1px solid #ddd; padding: 20px; margin-bottom: 25px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); background-color: #f9f9f9; }
      .dashboard-box-content { display: flex; flex-direction: column; height: 100%; }
      .dashboard-box-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px; }
      .dashboard-box-title { margin: 0; color: #333; font-weight: 600; font-size: 1.5em; }
      .dashboard-info-button { background: none; border: none; color: #007bff; font-size: 18px; cursor: pointer; }
      .dashboard-plot-container { flex: 1; background-color: white; border-radius: 5px; padding: 15px; box-shadow: inset 0 0 5px rgba(0,0,0,0.05); }
      .ncontainer { border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 5px; display: flex; } 
      .n-content { flex: 1; }
      .custom-container { width: 100%; background-color: #fcfcfc; /* Light gray background */ padding: 20px; border-radius: 10px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); /* Bottom shade */ margin-bottom: 20px; margin-left: 0px; margin-right: 0px; }
      .news-content { overflow: hidden; /* Prevent overflow */ text-overflow: ellipsis; /* Add ellipsis for overflowed text */ white-space: normal; /* Allow text to wrap */ }
      @media (max-width: 1000px) {
        .news-container {
          flex-direction: column;
          align-items: flex-start;
          }
        .news-image {
          margin-right: 0;
          margin-bottom: 10px;
          flex: 0 0 auto; /* Adjust image size if necessary */
          }
        .news-header {
          flex-direction: column;
          align-items: flex-start;
          }
      }
      .container-fluid {
        padding-left: 0;
        padding-right: 0;
      }
      
    "
    )
  )) 
, 

  
  
  navbarPage(
    
  
    id = "navbar", theme = bs_theme(bootswatch = "minty"), 
    collapsible = TRUE, 
    
    # title = "Value Quant Investment Platform - v1.0",
    
    title = tags$img(src = "convertio.in_logo2.avif", width = "300px;", style = "margin-right: 5px;"),
    
    header = div(
      class = "custom-container",
      fluidRow(
        column(
          width = 4,
          offset = 4,
          style = "margin-left: 5px; margin-top: 10px;",
          
          selectizeInput(
            inputId = "mstrSmbl",
            label = NULL,
            # Removed label for a cleaner look
            choices = c('META', 'MSFT', 'GOOG'),
            multiple = FALSE,
            selected = 'META',
            options = list(
              placeholder = "Type ticker symbol...",
              onInitialize = I('function() { this.setValue(""); }')
            ),
            width = '100%',
            # style = "padding: 10px; font-size: 16px; border-radius: 5px; border: 1px solid #ddd;"
          )
        ),
        
    
        column(
          width = 2,
          offset = 0,
          style = "margin-left: 5px; margin-top: 10px; display: flex; align-items: center;",
          
          actionButton(
            inputId = 'mstrSmblBtn',
            label = "Search",
            style = "padding: 10px; font-size: 16px; border-radius: 5px; border: none; cursor: pointer;"
          )
        )
      )), 
  
  
    footer = fluidRow(column(width = 10, offset = 1, tagList(
      div(
        class = "footer",
        "© 2024 Value Quant Investment.",
        tags$a(href = "#href", "Visit the substack.")
      )
    ))), 
    
    #### tab intro ----
    tabPanel(title = "Intro",
             id = "Intro", 
             div(fluidRow(
      column(width = body_width, offset = (12 - body_width)/2, #### econ/markets/cmmdts ----
             # more styling
             verticalLayout(div(
               class = "dashboard-box", div(
                 class = "dashboard-box-content",
                 div(
                   class = "dashboard-box-header",
                   h4(class = "dashboard-box-title", 'Global Market Index'),
                   actionButton("info_mrkts", "ℹ️", class = "dashboard-info-button")
                 ),
                 div(
                   class = "dashboard-plot-container",
                   style = "max-width: 900px; width: 100%; margin: 0 auto;",
                   plotlyOutput('mrkts', height = '550px')
                 )
               )
             )))
    )), 
  fluidRow(column(width = body_width, offset = (12 - body_width)/2, verticalLayout(div(
    class = "dashboard-box", div(
      class = "dashboard-box-content",
      div(
        class = "dashboard-box-header",
        h4(class = "dashboard-box-title", 'Economic Indicators'),
        actionButton("info_ecn", "ℹ️", class = "dashboard-info-button")
      ),
      div(
        class = "dashboard-plot-container",
        style = "max-width: 1200px; width: 100%; margin: 0 auto;",
        plotlyOutput('ecn', height = '1850px')
      )
    )
  )))), 
  fluidRow(column(width = body_width, offset = (12 - body_width)/2, verticalLayout(div(
    class = "dashboard-box", div(
      class = "dashboard-box-content",
      div(
        class = "dashboard-box-header",
        h4(class = "dashboard-box-title", 'Economic Indicators'),
        actionButton("info_cmmdts", "ℹ️", class = "dashboard-info-button")
      ),
      div(
        class = "dashboard-plot-container",
        style = "max-width: 1200px; width: 100%; margin: 0 auto;",
        plotlyOutput('cmdts', height = '1050px')
      )
    )
  ))
  )),
  #### news/press ----
  fluidRow(column(
    width = body_width,
    offset = (12 - body_width)/2,
    tabsetPanel(
      type = "tabs",
      tabPanel("News", uiOutput("nws")),
      tabPanel("Press Release", uiOutput('prssRls'))
      
    )
  ))
  ), 
  #### tab general ----
  tabPanel(title = 'General', 
           id = "General",
    fluidRow(
             column(width = body_width, offset = (12 - body_width)/2,
    fluidRow(
    column(
      width = 8,
      style = "padding: 25px; padding-top: 5px;",
      verticalLayout(
        div(
          class = "ncontainer",
          style = "width: 100%",
          div(class = "n-content", style = "width: 100%", plotlyOutput('stkP'))
        ),
        div(
          class = "ncontainer",
          style = "width: 100%; overflow-x: auto;",
          div(
            class = "n-content",
            style = "width: 100%; margin: auto;",
            tableOutput("fnnclSmmry")
          )
        ),
        div(class = "ncontainer", div(class = "n-content", uiOutput('dscrpt')))
        
      )
    ),
    column(
      width = 4,
      style = "padding: 25px; padding-top: 5px;",
      verticalLayout(
        tabsetPanel(
          type = "tabs",
          
          tabPanel("News", uiOutput('stkNws')),
          tabPanel("Press Release", uiOutput('stkPrs'))
        ),
        tableOutput("prfl")
        
      )
    )
  ), 
             )
    )
  ), 
  #### tab fundamentals ----
  tabPanel(title = 'Fundamentals',
           id = "Fundamentals",
           
           fluidRow(column(width = body_width, offset = (12 - body_width)/2,
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 'fndmtlsMtrics',
                 "Display Fundamentals: ",
                 choices = c(
                   'revenue',
                   'operatingIncome',
                   'netIncome',
                   'freeCashFlow',
                   'inventory',
                   'dividendsPaid',
                   'weightedAverageShsOutDil',
                   'propertyPlantEquipmentNet',
                   'cashAndCashEquivalents',
                   'totalAssets',
                   'interestExpense',
                   'totalLiabilities',
                   'retainedEarnings',
                   'debtRepayment',
                   'totalDebt',
                   'longTermDebt',
                   'capitalExpenditure',
                   'netRepurchases',
                   'netInvestments',
                   'stockBasedCompensation',
                   'netAcquisitions',
                   'researchAndDevelopmentExpenses'
                 ),
                 multiple = T,
                 selected = c(
                   'freeCashFlow',
                   'netIncome',
                   'OperatingIncome',
                   'dividendsPaid',
                   'stockBasedCompensation'
                 )
               ),
               sliderInput(
                 'fndmtlsSldr',
                 "Window: ",
                 min = 1995,
                 max = as.integer(format(Sys.Date(), '%Y')),
                 value = c(
                   2007,
                   as.integer(format(Sys.Date(), '%Y'))
                 ),
                 sep = '',
                 animate = F,
                 step = 1
               ),
              checkboxInput('fndmtlsLg', 'Log Scale', FALSE),
              selectInput(
                'sctr',
                'Comparison Cohort: ',
                choices = c(
                  # 'Reported Currency',
                  'All Sectors',
                  "Healthcare",
                  "Basic Materials",
                  "Industrials",
                  "Consumer Cyclical",
                  "Technology",
                  "Real Estate",
                  "Consumer Defensive",
                  "Communication Services",
                  "Energy",
                  #"Industrial Goods",
                  "Financial"

                ),
                selected = 'All Sectors'
              )
             ),
           
           ## more styling
           
           mainPanel(verticalLayout(div(
             class = "ncontainer",
             div(class = "n-content", style = "max-width: 1400px; width: 80%; margin: 0 auto;", plotlyOutput('stkFndmt'))
           )))
           )
           )
           ), fluidRow(column(
             width = 10,
             offset = 1,
             style = "align: center;",
             div(class = "ncontainer", div(
               class = "n-content",
               
               plotOutput('stkFndmntlsRltv', width = "95%", height = "700px")
             ))
           )), 
           fluidRow(column(
             width = 10,
             offset = 1,
             style = "align: center;",
             div(class = "ncontainer", div(
               class = "n-content",
               # DTOutput('stkFndmntlsTbl',
               #          width = "95vw",
               #          height = "700px")
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Income", DTOutput(
                   'stkInc_', width = "95%", height = "700px"
                 )),
                 tabPanel("Balance", DTOutput(
                   'stkBal_', width = "95%", height = "700px"
                 )),
                 tabPanel("Cash Flow", DTOutput(
                   'stkCF_', width = "95%", height = "700px"
                 ))
               )
             ))
           ))
           
           ),
  
#### tab model ----
tabPanel(
  title = "Model",
  id = "Model",
  
  fluidRow(column(width = body_width, offset = (12 - body_width)/2,
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "i_stkMdlSldr",
        "Window: ",
        min = as.Date('1995-01-01', '%Y-%m-%d'),
        max = as.Date(Sys.Date(), '%Y-%m-%d'),
        value = c(
          as.Date('2007-01-01', '%Y-%m-%d'),
          as.Date(Sys.Date(), '%Y-%m-%d')
        ),
        animate = F,
        timeFormat = '%Y-%m-%d'
      ),
      radioButtons(
        'i_mdlMtrc',
        "Profit Metric (per Share): ",
        choices = c('Net Income' = 'epsdiluted', 
                    'Free Cash Flow' = 'fcfps', 
                    'Operating Earnings' = 'operatingps', 
                    'Revenue' = 'revenueps'),
        selected = 'epsdiluted',
        inline = TRUE
      ),
      numericInput(
        'i_mdlMltpl',
        'Set Multiple: ',
        value = 0,
        min = 0,
        max = 1000,
        step = 1
      ),
      checkboxInput(
        'i_mdlLg',
        'Log Scale',
        FALSE
      )
      
    ),
    mainPanel(
      verticalLayout(
        div(class = "ncontainer",
            div(class = "n-content",
                
              plotlyOutput('stkMdl')
            )
        ),
      tableOutput('stkMdlTbl')
      
      )
    )
  )
  )
  )
),

#### tab valuation----
tabPanel(
  title = "Valuation",
  id = "Valuation",
  fluidRow(column(width = body_width, offset = (12 - body_width)/2,
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "i_vltnSldr",
        "Select Facets: ",
        choices = c(
          "Free Cash Flow" = "P-FCF",
          "Net Income" = "PE",
          "Enterprise Value" = "EV-FCF",
          "Revenue" = "P-REVENUE",
          "Operating Income" = "P-OP",
          "Dividends" = "P-DIV",
          "Book Value" = "P-BOOK",
          "Debt" = "P-DEBT"
        ),
        selected = c("P-FCF", "PE", "EV-FCF", "P-BOOK", "P-OP"),
        multiple = TRUE,
        selectize = T
      ),
      sliderInput(
        "i_stkVltnSldr",
        "Window: ",
        min = as.Date('1995-01-01', '%Y-%m-%d'),
        max = as.Date(Sys.Date(), '%Y-%m-%d'),
        value = c(
          as.Date('2007-01-01', '%Y-%m-%d'),
          as.Date(Sys.Date(), '%Y-%m-%d')
        ),
        animate = F,
        timeFormat = '%Y-%m-%d'
      ),
      numericInput(
        'i_vltnMvngAvg',
        'Set Moving Average: ',
        value = 2,
        min = 2,
        max = 15,
        step = 1
      ),
      checkboxInput('i_vltnLg', 'Log Scale', FALSE)
      
    ),
    
    mainPanel(width = 9, verticalLayout(div(class = "ncontainer", div(
      class = "n-content",
      column(
        width = 12,
       # style = "display: flex; flex-direction: column; align-items: center;",
        plotlyOutput('vltn', width = "95%", height = "1500px")
      )
    ))))
  )
  )
  )
), 

#### tab capital allocation ----
tabPanel(
  title = "Capital Allocation",
  id = "Capital Allocation",
  fluidRow(column(width = body_width, offset = (12 - body_width)/2,
  sidebarLayout(
    sidebarPanel(
      selectInput(
        'cptlAllctnMtrics',
        "Display Activity: ",
        choices = c(
          "capitalExpenditure",
          "dividendsPaid",
          "netDebtRepayment",
          "netAcquisitions",
          "netInvestments",
          "netRepurchases"
          
        ),
        multiple = T, selected = c("capitalExpenditure",
                                   "dividendsPaid",
                                   "netAcquisitions",
                                   "netRepurchases")
      ), sliderInput(
        'cptlAllctnSldr',
        "Window: ",
        min = 1995,
        max = as.integer(format(Sys.Date(), '%Y')),
        value = c(2007, as.integer(format(Sys.Date(
          
        ), '%Y'))),
        sep = '',
        animate = F,
        step = 1
      ), ), mainPanel(verticalLayout(div(class = "ncontainer", div(
        class = "n-content",
        column(
          width = 12,
          style = "display: flex; flex-direction: column; align-items: center;",
          plotlyOutput('cptlAllctn', width = "95%", height = "650px")
          
        )
      ))))
  )
  )
  )
  ), 

#### transcripts ----
tabPanel(
  title = "Transcripts",
  id = "Transcripts",
  fluidRow(column(width = body_width, offset = (12 - body_width)/2,
  sidebarLayout(
    
    sidebarPanel(
      numericInput(
        "trnscrptYr",
        "Select Year: ",
        value = as.integer(format(Sys.Date(), "%Y")),
        min = 1990,
        max = as.integer(format(Sys.Date(), "%Y")),
        step = 1
      ),
      numericInput(
        "trnscrptQrtr",
        "Select Quarter: ",
        value = 1,
        min = 1,
        max = 4,
        step = 1
      ),
      # fluidRow(column(
      #   width = 12,
      #   offset = 0,
      #   style = "width: auto;",

           
      div(class = "ncontainer", div(
        class = "n-content",
        textAreaInput(
          "i_oai",
          "Ask the AI assistant: ",
          rows = 3,
          placeholder = "Summarise the quarter's material investor information.", 
        ),
        actionButton('oaiBtn', 'Submit')
      )),
      hr(),
      
      
      div(class = "ncontainer", div(
        class = "n-content", h6('Chat History'), uiOutput("chtHst")
      ))
      
      # )
      # )
  ),
  mainPanel(verticalLayout(div(
    class = "ncontainer", div(class = "n-content", uiOutput('trnscrpt'), )
  ),  
  # div(class = "main-content",  # Add class for main content
  #      lapply(1:100, function(i) {
  #        p(paste("This is paragraph number", i))
  #      }))
  # 
  )
  
  
  ))
  )
  )
  ), 
  #### tab other ----
  tabPanel(title = "Other", 
           id = "Other",
  
  fluidRow(column(width = 10, offset = 1,         
  fluidRow(
    column(
      width = 6,
      offset = 0,
      
      plotlyOutput('stkRtChng')
    ),
    column(
      width = 6,
      offset = 0,
      plotlyOutput('sp5Prfmnc')
    )
  ))
  )
  ), #### login ----
  
  tabPanel("Login",
           fluidPage(
             shinyauthr::loginUI(
               "login",
               additional_ui = tags$div(
                 style = "margin-top: 20px;",
                 actionButton("register", "Register", class = "btn btn-danger")
               )
               
             ),
             
             shinyauthr::logoutUI(class = "pull-right", id = "logout")
             
           )
           
          )
  )
)
  


#### shiny server ----
server <- function(input, output, session) {
  
  con <- dbConnect(RSQLite::SQLite(), "datadb.db")
  
  i_mstrSmbl <- reactiveVal(NULL)
  
  ## data price
  stkPrc <- reactiveVal(NULL)
  ## data profile
  stkPrfl <- reactiveVal(NULL)
  ## data fundamentals
  stkInc <- reactiveVal(NULL)
  stkBs <- reactiveVal(NULL)
  stkCf <- reactiveVal(NULL)
  stkFDta <- reactiveVal(NULL)
  stkFdmntlsLng <- reactiveVal(NULL)
  stkInc <- reactiveVal(NULL)
  stkBal <- reactiveVal(NULL)
  stkCF <- reactiveVal(NULL)
  
  # market history
  # United States
  # Dow Jones Industrial Average: ^DJI
  # S&P 500: ^GSPC
  # NASDAQ Composite: ^IXIC
  # Russell 2000: ^RUT
  # Wilshire 5000: ^W5000
  # Canada
  # S&P/TSX Composite Index: ^GSPTSE
  # Europe
  # FTSE 100 (UK): ^FTSE
  # DAX (Germany): ^GDAXI
  # CAC 40 (France): ^FCHI
  # EURO STOXX 50: ^STOXX50E
  # Asia
  # Nikkei 225 (Japan): ^N225
  # Hang Seng Index (Hong Kong): ^HSI
  # SSE Composite Index (China): ^SSE
  # Australia
  # S&P/ASX 200: ^AXJO
  # Latin America
  # IBOVESPA (Brazil): ^BVSP
  # Merval (Argentina): ^MERV
  
  sp5 <- reactive({
    fmpc_price_history(symbols = "^GSPC", startDate = input$i_stkMdlSldr[1]) %>% select(symbol, date, close)
  })
  
  indxs <- reactive({
    fmpc_price_history(
      symbols = c("^AXJO", "^GSPC", "^HSI", "^STOXX50E"),
      startDate = Sys.Date() - 90
    ) %>% select(symbol, date, close)
  })
  
  
  
  ## auto-suggest
  updateSelectizeInput(
    session,
    'mstrSmbl',
    selected = 'META',
    choices = autosuggest_,
    server = TRUE,
    options = list(maxOptions = 10)
  )
  
  #### intro ----
  
  #### econ ----
  
    # GDP,
    # realGDP,
    # nominalPotentialGDP,
    # realGDPPerCapita,
    # federalFunds,
    # CPI,
    # inflationRate,
    # inflation,
    # retailSales,
    # consumerSentiment,
    # durableGoods,
    # unemploymentRate,
    # totalNonfarmPayroll,
    # initialClaims,
    # industrialProductionTotalIndex,
    # newPrivatelyOwnedHousingUnitsStartedTotalUnits,
    # totalVehicleSales,
    # retailMoneyFunds,
    # smoothedUSRecessionProbabilities,
    # 3MonthOr90DayRatesAndYieldsCertificatesOfDeposit,
    # commercialBankInterestRateOnCreditCardPlansAllAccounts,
    # 30YearFixedRateMortgageAverage,
    # 15YearFixedRateMortgageAverage
  
  output$ecn <- renderPlotly({
    start <- Sys.time()
    result <- dbGetQuery(con, "SELECT * FROM economic_indicators WHERE Indicator in ('realGDP',
         'realGDPPerCapita',
         'federalFunds',
         'CPI',
         'inflation',
         'consumerSentiment',
         'unemploymentRate',
         'commercialBankInterestRateOnCreditCardPlansAllAccounts',
         '30YearFixedRateMortgageAverage',
         'retailSales')"
                         ) %>% mutate(date = as.Date(date), date_added = as.Date(date_added))
    last_run_date <- as.Date(result %>% summarise(last_date_added = max(date_added)) %>% pull(last_date_added))
    
    if (Sys.Date() - as.Date(last_run_date) > 30){
      source(createdb.R)
    }

    p <- result %>% ggplot() +
      # geom_area(aes(x = date, y = close, ymin = 10000), alpha = 0.15) +
      # geom_ribbon(aes(x = date, ymin = mn_ * .97, ymax = close), alpha = 0.15) + #, fill = '#56CC9D'
      geom_line(aes(x = date, y = value)) + #, colour = '#FF7851'
      scale_x_date(date_labels = "%Y") +
      facet_wrap(vars(IndicatorName), ncol = 1, scales = 'free') +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) + #scales::label_number(scale = 1e-3)) +
      labs(x = '', y = '') +
      theme_minimal() +
      theme(
        # panel.spacing.x = unit(-1, "lines"),
        panel.spacing.y = unit(-0.5, "lines"),
        axis.text = element_text(face = "bold", size = 10),
        # plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(t = 10, b = 10)),
        strip.text.x = element_text(
          face = "bold",
          size = 12,
          margin = margin(
            t = 10,
            r = 0,
            b = 10,
            l = 0
          )
        )
      )
    
    print(paste("Econ: ", as.character(Sys.time() - start)))
    
    return(ggplotly(p) %>% 
             config(
               modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
               displaylogo = FALSE  # This removes the Plotly logo, which is often desired
             ) %>% layout(
               dragmode = FALSE
             ))
      
  })
  

  
  #### markets ----
  
  output$mrkts <- renderPlotly({
    
    start <- Sys.time()
    
    d_ <- req(indxs())
    mins_ <- d_ %>% group_by(symbol) %>% summarise(mn_ = min(close))
    d_ <- d_ %>% left_join(mins_, by = "symbol")
    
    d_ <- d_ %>%
      mutate(
        Index = case_when(
          symbol == '^STOXX50E' ~ 'EURO STOXX 50',
          symbol == '^N225' ~ 'Nikkei 225',
          symbol == "^AXJO" ~ "AUS 200",
          symbol == '^GSPC' ~ 'S&P 500',
          symbol == '^HSI' ~ 'Hang Seng Index',
          TRUE ~ symbol  # Keep original if no match
        )
      )
    
    print(paste("Markets: ", as.character(Sys.time() - start)))
    
    ggplotly(
      d_ %>% ggplot() +
        # geom_area(aes(x = date, y = close, ymin = 10000), alpha = 0.15) +
        geom_ribbon(aes(
          x = date, ymin = mn_ * .99, ymax = close
        ), alpha = 0.15) + #, fill = '#56CC9D'
        geom_line(aes(x = date, y = close)) + #, colour = '#FF7851'
        scale_x_date(date_labels = "%m-%d") +
        facet_wrap(vars(Index), ncol = 1, scales = 'free') +
        scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K")) +
        labs(x = '', y = '') +
        theme_minimal() +
        theme(
          # panel.spacing.x = unit(-1, "lines"),
          # panel.spacing.y = unit(-0.5, "lines"),
          axis.text = element_text(face = "bold", size = 10),
          # plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(t = 10, b = 10)),
          strip.text.x = element_text(
            face = "bold",
            size = 12,
            margin = margin(
              t = 10,
              r = 0,
              b = 10,
              l = 0
            )
          )
        )
    ) %>% style(hoverinfo = "none", traces = 1) %>% 
      config(
        modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
        displaylogo = FALSE  # This removes the Plotly logo, which is often desired
      ) %>% layout(
        dragmode = FALSE
      )
  })
  
  
  #### commodities ----
  
  output$cmdts <- renderPlotly({
    
    start = Sys.time()
    
    result <- dbGetQuery(con, "SELECT * FROM commodities") %>% mutate(date = as.Date(date), date_added = as.Date(date_added))
    last_run_date <- as.Date(result %>% summarise(last_date_added = max(date_added)) %>% pull(last_date_added))
    
    if (Sys.Date() - as.Date(last_run_date) > 30){
      source(createdb.R)
    }
    
    
    print(paste("Commodities: ", as.character(Sys.time() - start)))
    
    
    ggplotly(
      result %>% ggplot() +
        # # geom_area(aes(x = date, y = close, ymin = 10000), alpha = 0.15) +
        # geom_ribbon(aes(x = date, ymin = mn_ * .99, ymax = close), alpha = 0.15) + #, fill = '#56CC9D'
        geom_line(aes(x = date, y = close)) + #, colour = '#FF7851'
        scale_x_date(date_labels = "%Y") +
        facet_wrap(vars(CommodityName), ncol = 1, scales = 'free') +
        scale_y_continuous(labels = scales::label_number()) +
        labs(x = '', y = '') +
        theme_minimal() +
        theme(
          # panel.spacing.x = unit(-1, "lines"),
          # panel.spacing.y = unit(-0.5, "lines"),
          axis.text = element_text(face = "bold", size = 10),
          # plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(t = 10, b = 10)),
          strip.text.x = element_text(
            face = "bold",
            size = 12,
            margin = margin(
              t = 10,
              r = 0,
              b = 10,
              l = 0
            )
          )
        )
    ) %>% 
      config(
        modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
        displaylogo = FALSE  # This removes the Plotly logo, which is often desired
      ) %>% layout(
        dragmode = FALSE
      )#%>% style(hoverinfo = "none", traces = 1)
    
    
    
  })
  
  
  #### press  ----
  
  output$prssRls <- renderUI({
    d_ <- general.APIcall(endpoint = "Press-Release")
    
    lapply(1:(min(30, nrow(d_))), function(i) {
      create_news_container(
        type = "Press-Release",
        symbol = d_$symbol[i],
        date = d_$date[i],
        title = d_$title[i],
        text = d_$text[i]
      )
    })
  })

  #### news ----
  output$nws <- renderUI({
    d_ <- general.APIcall(endpoint = "News") %>% filter(site != "ndtv.com")
    
    lapply(1:(min(30, nrow(d_))), function(i) {
      create_news_container(
        type = "News",
        publishedDate = d_$publishedDate[i],
        title = d_$title[i],
        image = d_$image[i],
        site = d_$site[i],
        text = d_$text[i],
        url = d_$url[i]
      )
    })
  })
  
  
  #### login ----
  
  
  conn <- dbConnect(RSQLite::SQLite(), dbname = "users.sqlite")
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = dbGetQuery(conn, "SELECT * FROM users"),
    user_col = "email",
    pwd_col = "password",
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(id = "logout", active = reactive(credentials()$user_auth))
  
  
  
  observeEvent(input$register, {
    
    # Logic to handle registration, such as showing a modal or navigating to a registration page
    showModal(modalDialog(
      title = "Register",
      textInput("new_user", "Email:"),
      passwordInput("new_password", "Password:", placeholder = ""),
      textOutput('register_message'),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("cancel_button", "Cancel"),
        actionButton("submit_registration", "Register")
      )
    ))
  })
  
  observeEvent(input$cancel_button, {
    # Clear the input fields
    updateTextInput(session, "new_user", value = "")
    updateTextInput(session, "new_password", value = "")
    
    # Clear the output message
    output$register_message <- renderText("")
    
    # Close the modal
    removeModal()
  })
  
  observeEvent(input$submit_registration, {
    new_user <- input$new_user
    new_password <- input$new_password
    
    existing_users <- dbGetQuery(conn, "SELECT email from users")
    if (new_user %in% existing_users$email) {
      output$register_message <- renderText("Username already exists. Please choose another.")
    }
    
    else if (!grepl("@", new_user) | !grepl("\\.", new_user)) {
      output$register_message <- renderText("Please enter a valid e-mail address.")
    }
    
    
    else if (nchar(new_password) <= 5 |
             !grepl("\\d", new_password) |
             !grepl("[a-z]", new_password) |
             !grepl("[A-Z]", new_password) |
             !grepl("\\W", new_password)) {
      output$register_message <- renderText(
        "Password must be longer than five characters and have at least one digit, one upper-case letter, one lower-case letter and one special character."
      )
    }
    
    else {
      hashed_pass <- password_store(new_password)
      # Insert new user into the database
      dbExecute(
        conn,
        "INSERT INTO users (email, password) VALUES (?, ?)",
        params = list(new_user, hashed_pass)
      )
      output$register_message <- renderText("Registration successful! You can now log in.")
      
      updateTextInput(session, "new_user", value = "")
      updateTextInput(session, "new_password", value = "")
      
    }
  })
  
  
  observeEvent(input$mstrSmblBtn, {
    
    # print(credentials()$info)
    
    if (input$navbar == "Intro") {
      updateTabsetPanel(session, "navbar", selected = "General")
    }
    
    #### data ----
    
    i_mstrSmbl(input$mstrSmbl)
    
    # currencies
    fxs <- lapply(paste(c("BRL", "CAD", "CNY", "EUR", "HKD", "INR", "JPY", "KRW", "MXN", "MYR", "NOK", "SEK", "SGD", "TRY", "TWD", "ZAR"), 'USD', sep = ''), fx.APIcall)
    pair <- unname(unlist(lapply(fxs, '[', 'symbol')))
    rate <- unname(unlist(lapply(fxs, '[', 'price')))
    fxs <- data.frame(symbol = pair, price = rate)
    
    ## load data
    dt_0 <- fmpc_security_profile(input$mstrSmbl)
    dt_1 <- fmpc_price_history(symbols = input$mstrSmbl, startDate = "1995-01-01", endDate = Sys.Date()) %>%
      select(symbol, date, close) %>% mutate(close = round(close , 2))
    dt_2 <- fmpc_financial_bs_is_cf(symbols = input$mstrSmbl, statement = 'income', quarterly = FALSE, limit = 25)
    dt_3 <- fmpc_financial_bs_is_cf(symbols = input$mstrSmbl, statement = 'balance', quarterly = FALSE, limit = 25)
    dt_4 <- fmpc_financial_bs_is_cf(symbols = input$mstrSmbl, statement = 'cashflow', quarterly = FALSE, limit = 25)
    f_dt <- dt_2 %>% inner_join(dt_3, by = c('symbol', 'calendarYear')) %>% inner_join(
      dt_4 %>% select(-netIncome, -inventory, -depreciationAndAmortization),
      by = c('symbol', 'calendarYear')
    ) %>% select(!contains(c('.x', '.y', 'cik', 'link', 'dDate'))) %>%
      select(symbol, calendarYear, date, fillingDate, reportedCurrency, everything()) %>%
      group_by(symbol) %>% arrange(symbol, desc(calendarYear)) %>% ungroup() %>%
      mutate(fillingDate = as.Date(fillingDate, format = "%Y-%m-%d")) %>%
      mutate(calendarYear = as.integer(calendarYear)) %>%
      mutate(across(!contains(c('calendarYear', 'Ratio', 'ratio', 'weightedAverageShsOutDil')) & where(is.numeric),
                                                                  ~ case_when(
                                                                    reportedCurrency == 'CNY' ~ .x * fxs[fxs['symbol'] == 'CNYUSD', 'price'],
                                                                    reportedCurrency == 'BRL' ~ .x * fxs[fxs['symbol'] == 'BRLUSD', 'price'],
                                                                    reportedCurrency == 'SEK' ~ .x * fxs[fxs['symbol'] == 'SEKUSD', 'price'],
                                                                    reportedCurrency == 'EUR' ~ .x * fxs[fxs['symbol'] == 'EURUSD', 'price'],
                                                                    reportedCurrency == 'CAD' ~ .x * fxs[fxs['symbol'] == 'CADUSD', 'price'],
                                                                    reportedCurrency == 'TRY' ~ .x * fxs[fxs['symbol'] == 'TRYUSD', 'price'],
                                                                    reportedCurrency == 'MXN' ~ .x * fxs[fxs['symbol'] == 'MXNUSD', 'price'],
                                                                    reportedCurrency == 'TWD' ~ .x * fxs[fxs['symbol'] == 'TWDUSD', 'price'],
                                                                    reportedCurrency == 'ZAR' ~ .x * fxs[fxs['symbol'] == 'ZARUSD', 'price'],
                                                                    reportedCurrency == 'HKD' ~ .x * fxs[fxs['symbol'] == 'HKDUSD', 'price'],
                                                                    reportedCurrency == 'SGD' ~ .x * fxs[fxs['symbol'] == 'SGDUSD', 'price'],
                                                                    reportedCurrency == 'MYR' ~ .x * fxs[fxs['symbol'] == 'MYRUSD', 'price'],
                                                                    reportedCurrency == 'JPY' ~ .x * fxs[fxs['symbol'] == 'JPYUSD', 'price'],
                                                                    reportedCurrency == 'INR' ~ .x * fxs[fxs['symbol'] == 'INRUSD', 'price'],
                                                                    reportedCurrency == 'KRW' ~ .x * fxs[fxs['symbol'] == 'KRWUSD', 'price'],
                                                                    TRUE ~ .x
                                                                  )
      )) %>%
      mutate(
        netInvestments = abs(purchasesOfInvestments) - abs(salesMaturitiesOfInvestments),
        netRepurchases = abs(commonStockRepurchased) - abs(commonStockIssued),
        netDebtRepayment = abs(debtRepayment) - abs(otherFinancingActivites),
        bookValue = totalStockholdersEquity - goodwill - intangibleAssets
      ) %>%
      mutate(across(
        c(
          'acquisitionsNet',
          'capitalExpenditure',
          'commonStockRepurchased',
          'dividendsPaid',
          'interestExpense'
        ),
        ~ abs(.x)
      )) %>%
      mutate(
        fcfps = case_when(
          weightedAverageShsOutDil == 0 ~ 0,
          TRUE ~ freeCashFlow / weightedAverageShsOutDil
        ),
        
        divps = case_when(
          weightedAverageShsOutDil == 0 ~ 0,
          TRUE ~ dividendsPaid / weightedAverageShsOutDil
        ),
        
        bookps = case_when(
          weightedAverageShsOutDil == 0 ~ 0,
          TRUE ~ (totalStockholdersEquity - goodwill) / weightedAverageShsOutDil
        ),
        
        netdebtps = case_when(
          weightedAverageShsOutDil == 0 ~ 0,
          TRUE ~ (totalDebt - cashAndCashEquivalents) / weightedAverageShsOutDil
        ),
        
        operatingps = case_when(
          weightedAverageShsOutDil == 0 ~ 0,
          TRUE ~ operatingIncome / weightedAverageShsOutDil
        ),
        
        revenueps = case_when(
          weightedAverageShsOutDil == 0 ~ 0,
          TRUE ~ revenue / weightedAverageShsOutDil
        ),
        
        debt = case_when(totalAssets == 0 ~ 0, TRUE ~ totalLiabilities / totalAssets),
        
        RD = case_when(revenue == 0 ~ 0, TRUE ~ researchAndDevelopmentExpenses / revenue),
        
        SA = case_when(
          revenue == 0 ~ 0,
          TRUE ~ sellingGeneralAndAdministrativeExpenses / revenue
        ),
        
        operatingMargin = case_when(revenue == 0 ~ 0, TRUE ~ operatingIncome / revenue),
        
        ROE = case_when(
          (totalStockholdersEquity - goodwill) == 0 ~ 0,
          TRUE ~ operatingIncome / (totalStockholdersEquity - goodwill)
        ),
        
        ROIC = case_when(
          (totalAssets - totalCurrentLiabilities - cashAndCashEquivalents) == 0 ~ 0,
          TRUE ~ operatingIncome / (totalAssets - totalCurrentLiabilities - cashAndCashEquivalents)
        ),
        
        CAPEX = case_when(revenue == 0 ~ 0, TRUE ~ capitalExpenditure / revenue),
        
        netRepurchasesRevenue = case_when(
          revenue == 0 ~ 0,
          TRUE ~ (commonStockRepurchased - commonStockIssued) / revenue
        ),
        
        divtoOpInc = case_when(operatingIncome == 0 ~ 0, TRUE ~ dividendsPaid / operatingIncome),
        
        netInterestExptoOpInc = case_when(
          operatingIncome == 0 ~ 0,
          TRUE ~ (interestIncome - interestExpense) / operatingIncome
        )
      )
    
    dt_5 <- f_dt %>%
      pivot_longer(
        cols = where(is.numeric) &
          !contains(c('symbol', 'calendarYear', 'fillingDate')),
        names_to = 'Legend',
        values_to = 'Value'
      )
    
    
    stkPrfl(dt_0)
    stkPrc(dt_1)
    stkFDta(f_dt)
    stkFdmntlsLng(dt_5)
    stkInc(dt_2)
    stkBal(dt_3)
    stkCF(dt_4)
    
    
    stkMdlRctv <- reactive({
      prc <- req(stkPrc()) %>%
        filter(between(date, input$i_stkMdlSldr[1], input$i_stkMdlSldr[2])) %>%
        mutate(calendarYear = as.integer(substr(date, 1, 4))) %>%
        mutate(mn = min(close, na.rm = T),
               mx = max(close, na.rm = T))
      
      fndmt <- req(stkFDta()) %>% select(symbol, calendarYear, fillingDate, !!(input$i_mdlMtrc)) %>% rename('profit' = !!(input$i_mdlMtrc))
      
      # write.csv(prc, 'prc_test2.csv')
      # write.csv(fndmt, 'fndm2.csv')
      
      d_e <- prc  %>%
        left_join(fndmt, by = c('symbol', 'calendarYear')) %>% mutate(non.neg.ratio = close / profit) %>%
        mutate(non.neg.ratio = replace(non.neg.ratio, non.neg.ratio <= 0, NA)) %>%
        mutate(multiple = ifelse(
          input$i_mdlMltpl == 0,
          psych::harmonic.mean(non.neg.ratio, na.rm = T),
          input$i_mdlMltpl
        )) %>%
        mutate(Estimate = round(profit * multiple, 2))
    })
    
    
    
    
    
    
    
    
    
    
    
    #### stock news ----
    output$stkNws <- renderUI({
      d_ <- general.APIcall(endpoint = "News-Stock", symbol = i_mstrSmbl()) %>% filter(site != "fool.com")
      
      lapply(1:(min(6, nrow(d_))), function(i) {
        create_news_container(
          type = "News",
          publishedDate = d_$publishedDate[i],
          title = d_$title[i],
          image = d_$image[i],
          site = d_$site[i],
          text = d_$text[i],
          url = d_$url[i]
        )
      })
      
    })
    
    #### stock press ----
    output$stkPrs <- renderUI({
      d_ <- general.APIcall(endpoint = "Press-Release-Stock", symbol = i_mstrSmbl()) #%>% filter(site != "fool.com")
      
      
      lapply(1:(min(6, nrow(d_))), function(i) {
        create_news_container(
          type = "Press-Release",
          symbol = d_$symbol[i],
          date = d_$date[i],
          title = d_$title[i],
          text = d_$text[i]
        )
      })
    })
    
    #### stock summary ----
    
    output$fnnclSmmry <- renderText({
      mkap <- fmpc_security_mrktcap(i_mstrSmbl(), limit = 1)
      avg.over <- 3
      
      dt <- req(stkFDta()) %>% mutate(rn = row_number()) %>% filter(rn <= avg.over) %>%
        group_by(symbol) %>% summarise(across(
          c(
            'freeCashFlow',
            'operatingIncome',
            'netIncome',
            'bookValue',
            'epsdiluted',
            'researchAndDevelopmentExpenses',
            'totalDebt'
          ),
          ~ mean(.x)
        )) %>%
        left_join(mkap, by = "symbol") %>% mutate(
          `P-FCF` = marketCap / freeCashFlow,
          PE = marketCap / netIncome,
          `P-Book` = marketCap / bookValue,
          EV = marketCap + totalDebt,
          `EV-FCF` = EV / freeCashFlow
        ) %>% mutate(across(
          where(is.numeric),
          ~ scales::label_number(
            scale_cut = scales::cut_short_scale(),
            accuracy = 0.01
          )(.x)
        ))
      
      dt %>% select(-symbol, -date) %>% t(.) %>% kable() %>%
        kable_styling(
          bootstrap_options = c("striped", "hover", "condensed", "responsive"),
          full_width = TRUE,
          position = "center"
        ) %>%
        column_spec(1, bold = TRUE) %>%
        row_spec(0, bold = TRUE, color = "white") %>%
        add_header_above(c("Financials" = 1, "3y Avg." = 1))
      
    })
    
    #### stock trivia table ----
    
    output$prfl <- renderText({
      req(stkPrfl()) %>% select(-mktCap,
                                -description,
                                -image,
                                -defaultImage,
                                -dcf,
                                -dcfDiff,
                                -zip,
      ) %>%
        t(.) %>% kable() %>%
        kable_styling(
          bootstrap_options = c("striped", "hover", "condensed", "responsive"),
          full_width = TRUE,
          position = "center"
        ) %>%
        column_spec(1, bold = TRUE) %>%
        row_spec(0, bold = TRUE, color = "white") %>% # background = "#4E79A7") %>%
        add_header_above(c("General" = 1, " " = 1))
    })
    
    #### stock description ----
    
    output$dscrpt <- renderUI({
      tagList(tags$h2("Description"), tags$p(req(stkPrfl()) %>% pull(description)))
    })
    
    
    #### stock price ----
    
    output$stkP <- renderPlotly({
      updatemenus <- list(
        list(
          active = 1,
          buttons = list(list(
            label = 'Log Scale',
            method = 'update',
            args = list(list(visible = c(TRUE, TRUE)), list(
              # title = 'Log scale',
              yaxis = list(
                type = 'log',
                # tickprefix = "$",
                nticks = 10,
                tickformat = "$,.0f",
                tickfont = list(size = 10),
                title = ""
              )
            ))
          ), list(label = 'Linear Scale',
                  method = 'update',
                  args = list(list(visible = c(TRUE, TRUE)), list(
                    # title = 'Linear scale',
                    yaxis = list(
                      type = 'linear',
                      # tickprefix = "$",
                      nticks = 10,
                      tickformat = "$,.0f",
                      tickfont = list(size = 10),
                      title = ""
                    )
                  )))
          ), direction = "right", pad = list(r = 10, t = 10), showactive = TRUE, x = 0.01, xanchor = "left", y = -0.05, yanchor = "top", type = "buttons", font = list(size = 10), buttonwidth = 80
        )
      )
      
      # prm_user <- credentials()$info$premium
      # print(prm_user)
      #
      d_ <- req(stkPrc()) %>%
        mutate(calendarYear = as.Date(paste0(format(date, "%Y"), "-12-31")))
      
      i_tr <- as.data.frame(general.APIcall(endpoint = "Insider-Trans", symbol = i_mstrSmbl()))
      
      
      if (nrow(i_tr) != 0) {
        i_tr <- i_tr %>%
          group_by(year) %>%
          summarise(
            purchases = sum(purchases),
            sales = sum(sales),
            quarters = dplyr::n_distinct(quarter),
            totalBought = sum(totalBought),
            totalSold = sum(totalSold),
            total_pPurchases = sum(pPurchases),
            total_sSales = sum(sSales)
          ) %>% mutate(calendarYear = as.Date(paste0(year, "-12-31")),
                       buySellRatio = totalBought / totalSold)
      } else {
        i_tr <- data.frame(
          purchases = 0,
          sales = 0,
          quarters = 0,
          totalBought = 0,
          totalSold = 0,
          total_pPurchases = 0,
          total_sSales = 0,
          year = "2024"
        ) %>% mutate(calendarYear = as.Date(paste0(year, "-12-31")),
                     buySellRatio = totalBought / totalSold)
      }
      
      
      d_ <- d_ %>% left_join(i_tr, by = "calendarYear") %>% group_by(calendarYear) %>% mutate(y_ = last(close)) %>% ungroup()

      ggplotly(
        d_ %>% ggplot() +
          geom_area(
            aes(x = date, y = close),
            fill = '#56CC9D',
            alpha = 0.05
          ) +
          geom_line(aes(x = date, y = close), colour = '#56CC9D') +
          geom_point(
            aes(
              x = calendarYear,
              y = y_,
              text = paste(
                "Insider Trading",
                "\nbuySellRatio: ",
                as.character(round(buySellRatio, 2)),
                "\npurchases: ",
                as.character(scales::comma(purchases)),
                "\nsales",
                as.character(scales::comma(sales)),
                "\nquarters: ",
                as.character(quarters),
                '\ntotal bought: ',
                as.character(scales::comma(totalBought)),
                '\nTotal Sold: ',
                as.character(scales::comma(totalSold)),
                '\nTotal pPurchases: ',
                as.character(scales::comma(total_pPurchases)),
                '\nTotal sSales: ',
                as.character(scales::comma(total_sSales))
              )
            ),
            show.legend = FALSE,
            colour = '#FF7851'
          ) +
          scale_y_continuous(labels = scales::label_number_auto()) +
          labs(x = '', y = '') +
          theme_minimal()
        # tooltip = c('text')
      ) %>% layout(updatemenus = updatemenus
      #              title = list(
      #   text = "Price History",
      #   y = 0.95,  # y position (0 to 1)
      #   x = 0.01,   # x position (0 to 1)
      #   xanchor = "top",
      #   yanchor = "top",
      #   font = list(size = 25)
      # )
      ) %>% style(hoverinfo = "none", traces = 1)


    })
    

    #### fundamentals chart ----
    
    output$stkFndmt <- renderPlotly({
      ggplotly(
        req(stkFdmntlsLng()) %>%
          filter(
            between(calendarYear, input$fndmtlsSldr[1], input$fndmtlsSldr[2])
          ) %>%
          filter(Legend %in% input$fndmtlsMtrics) %>%
          ggplot() +
          geom_line(aes(
            x = fillingDate, y = Value, col = Legend
          ), linewidth = 1) +
          geom_point(
            aes(x = fillingDate, y = Value, col = Legend),
            size = 1,
            show.legend = FALSE
          ) +
          scale_y_continuous(
            n.breaks = 10,
            trans = ifelse(input$fndmtlsLg, 'log', 'identity'),
            labels = scales::label_number(scale_cut = scales::cut_short_scale())
          ) +
          labs(x = '', y = '', title = 'Fundamentals Chart') +
          theme_minimal()
      ) %>% layout(legend = list(orientation = 'h')) %>% 
        config(
          modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
          displaylogo = FALSE  # This removes the Plotly logo, which is often desired
        ) %>% layout(
          dragmode = FALSE
        )
      
    })
    

    
    #### fundamentals sector chart
    
    output$stkFndmntlsRltv <- renderPlot({
      app_dir <- shiny::getShinyOption("appDir")
      path.root <- paste0(app_dir, '/src_tables/') #file.path()
      # print(path.root)
      kw = input$sctr
      keyword <- unlist(strsplit(kw, ' '))
      len.kw <- length(keyword)
      
      all.files <- list.files(path.root)
      
      if (len.kw == 1) {
        file <- all.files[grepl(tolower(keyword)[1], all.files)]
        read.path <- paste(path.root, file, sep = '')
      }
      else if (len.kw == 2) {
        p1 <- grepl(tolower(keyword)[1], all.files)
        p2 <- grepl(tolower(keyword)[2], all.files)
        pf <- ifelse(p1 + p2 == 2, TRUE, FALSE)
        
        file <- all.files[pf]
        read.path <- paste(path.root, file, sep = '')
      }
      
      cohort.summary <- read.csv(read.path)  %>% mutate(
        calendarYear = as.Date(calendarYear),
        min = as.numeric(min),
        med = as.numeric(med),
        max = as.numeric(max),
        Legend = metric
      ) %>%
        filter(between(
          as.integer(format(calendarYear, "%Y")),
          input$fndmtlsSldr[1],
          input$fndmtlsSldr[2]
        ))
      
      smry <- req(stkFdmntlsLng()) %>% filter(
        Legend %in% c(
          'fillingDate',
          'debt',
          'ROIC',
          'netInterestExptoOpInc',
          'RD',
          'SA',
          'operatingMargin',
          'CAPEX',
          'ROE',
          'netRepurchasesRevenue'
        )
      ) %>%
        filter(between(calendarYear, input$fndmtlsSldr[1], input$fndmtlsSldr[2])) #%>% mutate(Value = pmin(pmax(Value, 0), 1))
      
      
      
      # ggplotly(
      ggplot() +
        geom_line(data = cohort.summary,
                  aes(x = calendarYear, y = med),
                  linewidth = 1) +
        geom_ribbon(data = cohort.summary,
                    aes(
                      ymin = min,
                      ymax = max ,
                      x = calendarYear,
                      y = med
                    ),
                    alpha = 0.2) +
        geom_line(
          data = smry,
          aes(x = fillingDate, y = Value),
          linewidth = 1,
          colour = '#FF7851'
        ) +
        geom_point(data = smry,
                   aes(x = fillingDate, y = Value),
                   colour = '#FF7851') +
        
        scale_x_date(date_breaks = "2 years", date_labels = "%y") +
        scale_y_continuous(
          n.breaks = 7,
          limits = c(0, NA),
          oob = scales::squish,
          labels = scales::percent_format()
        ) +
        labs(title = 'Industry Distribution', y = '', x = '') +
        facet_wrap(vars(Legend), scales = 'free_y') +
        theme_minimal() +
        theme(
          axis.text = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold", size = 15),
          strip.text = element_text(size = 10)
        )
      # )
      
    })
    
    
    #### fundamentals table ----
    
    # output$stkFndmntlsTbl <- renderDT({
    #   datatable(req(stkFDta()) %>% filter(calendarYear >= 2018), #%>%
    #             #mutate(across(!contains(c('ratio', 'calendarYear', 'Ratio', 'average')) & where(is.numeric), ~scales::comma()(.x))),  #~scales::label_number(scale_cut = scales::cut_short_scale())(.x))),
    #             filter = "top",
    #             options = list(
    #               pageLength = 10,
    #               autoWidth = TRUE,
    #               scrollX = TRUE,
    #               dom = 'Bfrtip',  # Adds buttons for copy, csv, excel, etc.
    #               buttons = c('copy', 'csv', 'excel')
    #             ),
    #             extensions = 'Buttons',
    #             class = "cell-border stripe hover"  # Adds styling to the table
    #   )
    # })
    
    #### fundamentals tbl tabsetPanel ----
    
    output$stkInc_ <- renderDT({
      datatable(
        req(stkInc()) %>% filter(calendarYear >= 2018) %>%
          mutate(across(
            where(is.numeric), ~ custom_number_format(.x, decimals = 2)
          )),
        filter = "top",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        class = 'cell-border stripe hover'
      )
    })
    
    output$stkBal_ <- renderDT({
      datatable(
        req(stkBal()) %>% filter(calendarYear >= 2018) %>%
          mutate(across(
            where(is.numeric), ~ custom_number_format(.x, decimals = 2)
          )),
        filter = "top",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        class = 'cell-border stripe hover'
      )
    })
    
    output$stkCF_ <- renderDT({
      datatable(
        req(stkCF()) %>% filter(calendarYear >= 2018) %>%
          mutate(across(
            where(is.numeric), ~ custom_number_format(.x, decimals = 2)
          )),
        filter = "top",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        class = 'cell-border stripe hover'
      )
    })
    
    #### model ----
    
    output$stkMdl <- renderPlotly({
      
      d_e <- stkMdlRctv()
      
      
      p <- d_e %>%
        ggplot() +
        geom_area(aes(x = date, y = close), alpha = 0.15) +
        geom_line(aes(x = date, y = close)) +
        geom_point(aes(x = fillingDate, y = Estimate),
                   colour = '#FF7851',
                   shape = 3) +
        geom_smooth(
          aes(x = fillingDate, y = Estimate),
          colour = '#56CC9D',
          method = 'lm',
          formula = y ~ splines::ns(x, 2),
          se = F,
          fullrange = TRUE,
          linetype = 'dashed'
        ) +
        geom_smooth(
          aes(x = fillingDate, y = Estimate),
          colour = '#56CC9D',
          method = 'lm',
          se = F,
          fullrange = TRUE
        ) +
        scale_x_date(date_breaks = '1 year',
                     date_labels = "%y",
                     name = '') +
        scale_y_continuous(
          n.breaks = 11,
          trans = ifelse(input$i_mdlLg == TRUE, 'log', 'identity'),
          labels = scales::dollar_format(),
          name = ''
        ) +
        coord_cartesian(ylim = c(max(d_e$mn) * 0.95, max(d_e$mx)) * 1.15, expand = T) +
        labs(title = paste(
          i_mstrSmbl(),
          ifelse(
            input$i_mdlMtrc == 'epsdiluted',
            'Price vs. EPS x average multiple',
            ifelse(
              input$i_mdlMtrc == 'fcfps',
              'Price vs. FCF per share x average multiple',
              ifelse(
                input$i_mdlMtrc == 'operatingps',
                'Price vs. Operating Earnings per share x average multiple',
                'Price vs. Revenue per share x average multiple'
              )
            )
          ),
          '—',
          "Annual"
        )) +
        theme_minimal()
      # b_ <- ggplot_build(p)
      # b_lm_ns_ <- b_[[4]]
      # b_lm <- b_[[5]]
      # write.csv(b_lm_ns_, 'ggplot_obj.csv')
      
      ggplotly(
        p
      ) %>% style(hoverinfo = "text") %>% style(hoverinfo = "none", traces = c(1, 4,5)) %>% 
        config(
          modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
          displaylogo = FALSE  # This removes the Plotly logo, which is often desired
        ) %>% layout(
          dragmode = FALSE
        )
    })
    
    
    #### model table ----
    
    output$stkMdlTbl <- renderText({
      
      tbl <- stkMdlRctv() %>% 
        dplyr::distinct(calendarYear, fillingDate, profit, multiple, Estimate) %>% 
        arrange(desc(calendarYear)) 
      
      # write.csv(stkMdlRctv(), 'test.csv')
      
      tbl %>% 
        kable() %>%
        kable_styling(
          bootstrap_options = c("striped", "hover", "condensed", "responsive"),
          full_width = TRUE
        ) %>%
        column_spec(1, bold = TRUE) %>%
        row_spec(0, bold = TRUE, color = "white") %>%
        row_spec(0:nrow(tbl), align = "c") %>%
        add_header_above(c("calendarYear" = 1, "fillingDate" = 1, "profit" = 1, "multiple" = 1, "estimate" = 1))
    })
    
    
    
    #### valuation ----
    
    output$vltn <- renderPlotly({
      
      d_fd <- req(stkFDta()) %>% filter(symbol == i_mstrSmbl()) %>%
        select(symbol, calendarYear, fillingDate, epsdiluted, fcfps, divps, bookps, netdebtps, operatingps, revenueps) %>%
        arrange(desc(fillingDate)) %>%
        mutate(across(!contains(c('date', 'symbol', 'calendarYear') ), ~moving.average(.x, span = input$i_vltnMvngAvg, order = "up"), .names = "{col}_ma" ) )
      
      
      mx_cy <- max(d_fd$calendarYear, na.rm = T)
      # print(mx_cy)
      cy_y <- as.integer(format(Sys.Date(), "%Y"))
      yrs <- d_fd$calendarYear
      
      # print(cy_y)
      # print(yrs)
      # 
      # print(!(cy_y %in% yrs))
      
      if (!(cy_y %in% yrs)) {
        d_fd2 <- bind_rows(d_fd, 
                       d_fd %>% 
                         filter(calendarYear == mx_cy) %>% 
                         mutate(calendarYear = mx_cy + 1))
      }
      else {
        d_fd2 <- d_fd
      }
      
      prc <- req(stkPrc()) %>% 
        filter(date >= input$i_stkVltnSldr[1] & date <= input$i_stkVltnSldr[2]) %>%
        mutate(calendarYear = as.integer(format(date, "%Y")))
      
      vltn_d <- prc %>%
        # left_join(
        #   d_fd %>% select(symbol, fillingDate, calendarYear),
        #   by = c("calendarYear")
        # ) %>%
        left_join(
          d_fd2,
          by = c("calendarYear" = "calendarYear")
        ) %>% mutate(PE_Value = close/epsdiluted,
                     `P-FCF_Value` = close/fcfps,
                     `P-DIV_Value` = close/divps,
                     `P-BOOK_Value` = close/bookps,
                     `EV-FCF_Value` = (close + netdebtps) / fcfps,
                     `P-OP_Value` = close/operatingps,
                     `P-REVENUE_Value` = close / revenueps,
                     `P-DEBT_Value` = close / netdebtps,
                     
                     PE_Trend = close/epsdiluted_ma,
                     `P-FCF_Trend` = close/fcfps_ma,
                     `P-DIV_Trend` = close/divps_ma,
                     `P-BOOK_Trend` = close/bookps_ma,
                     `EV-FCF_Trend` = (close + netdebtps_ma) / fcfps_ma,
                     `P-OP_Trend` = close/operatingps_ma,
                     `P-REVENUE_Trend` = close / revenueps_ma,
                     `P-DEBT_Trend` = close / netdebtps_ma
                     ) %>%
        pivot_longer(cols = c(c(PE_Value, `P-FCF_Value`, `P-DIV_Value`, `P-BOOK_Value`, `EV-FCF_Value`, `P-OP_Value`, `P-REVENUE_Value`, `P-DEBT_Value`), 
                              c(PE_Trend, `P-FCF_Trend`, `P-DIV_Trend`, `P-BOOK_Trend`, `EV-FCF_Trend`, `P-OP_Trend`, `P-REVENUE_Trend`, `P-DEBT_Trend`)),
                     names_to = c('Legend', ".value"),
                     # values_to = 'Value'
                     names_sep = "_"
                     ) %>%
        group_by(Legend) %>%
        mutate(Value = round(Value, 2),
               Trend = round(Trend, 2),
               # Smooth = round(moving.average(Value, span = 120, order = 'down'), 2)
               ) %>%
        mutate(Legend = factor(Legend, ordered = TRUE, levels = c("PE", "P-FCF", "EV-FCF", "P-OP", "P-REVENUE", "P-BOOK", "P-DEBT", "P-DIV")))
      # 
      # write.csv(vltn_d, 'valuation_test3.csv')
      
      # vltn_d <- prc %>% 
      #   left_join(
      #     d_fd2,
      #     by = c("calendarYear" = "calendarYear")
      #   ) %>% pivot_longer(
      #     cols = c(epsdiluted, fcfps, divps, bookps, netdebtps, operatingps, revenueps, ev_spec),
      #     names_to = 'Legend',
      #     values_to = 'scores'
      #   ) %>% group_by(Legend) %>% mutate(Value = ifelse(Legend == "ev_spec", (close + debtps_) / fcfps_, close / scores),
      #                                     debtps_ma = moving.average(debtps_, span = input$i_vltnMvngAvg, order = "down"),
      #                                     fcfps_ma = moving.average(fcfps_, span = input$i_vltnMvngAvg, order = "down"),
      #                                     
      #                                     Trend = ifelse(Legend == "ev_spec", (close + debtps_ma) / fcfps_ma,  
      #                                                    close / moving.average(scores, span = input$i_vltnMvngAvg, order = "down"))) %>% ungroup() %>% mutate(
      #                  Legend = case_when(Legend == "epsdiluted" ~ "PE",
      #                                     Legend == "fcfps" ~ "P-FCF",
      #                                     Legend == "divps" ~ "P-DIV",
      #                                     Legend == "bookps" ~ "P-BOOK",
      #                                     Legend == "netdebtps" ~ "P-DEBT",
      #                                     Legend == "operatingps" ~ "P-OP",
      #                                     Legend == "revenueps" ~ "P-REVENUE",
      #                                     Legend == "ev_spec" ~ "EV-FCF") 
      #                ) %>% mutate(Value = round(Value, 2), 
      #                             Trend = round(Trend, 2),
      #                             Legend = factor(Legend, ordered = TRUE, levels = c("PE", "P-FCF", "EV-FCF", "P-OP", "P-REVENUE", "P-BOOK", "P-DEBT", "P-DIV")))
      # 
      # write.csv(vltn_d, 'valuation_test2.csv')
        
      
      ggplotly(
      vltn_d %>% filter(Legend %in% input$i_vltnSldr) %>% ggplot() +
        geom_line(aes(x = date, y = Value), colour = '#56CC9D', linetype = 'dashed', alpha = 0.5) +
        geom_line(aes(x = date, y = Trend), colour = '#FF7851') +
        # geom_line(aes(x = date, y = Smooth), alpha = 0.5) +
        scale_x_date(date_breaks = '2 years',
                     date_labels = '%y',
                     name = '') +
        scale_y_continuous(
          # n.breaks = 10,
          trans = ifelse(input$i_vltnLg == 1, 'log', 'identity'),
          labels = scales::number_format(),
          name = ''
        ) +
        facet_wrap(vars(Legend), ncol = 1, scales = 'free_y') +
        # facet_grid(rows = vars(Legend), cols = vars(2), scales = 'free_y') +
        labs(title = paste0('Valuation Chart — ', as.character(input$i_vltnMvngAvg), '-year Moving Avg.'),
             # subtitle = paste("Moving Average —", as.character(input$i_vltnMvngAvg))
             ) +
        theme_minimal() +
        theme(
          axis.text = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold", size = 15),
          strip.text = element_text(face = "bold", size = 10),
          panel.spacing.x = unit(-0.65, "cm")
          # panel.spacing.y = unit(0, "lines")
        )
      ) %>% 
        config(
          modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
          displaylogo = FALSE  # This removes the Plotly logo, which is often desired
        ) %>% layout(
          dragmode = FALSE
        )
      
    })
    
    #### capital allocation ----
    
    output$cptlAllctn <- renderPlotly({
      d <- req(stkFdmntlsLng())  %>%
        filter(between(calendarYear, input$cptlAllctnSldr[1], input$cptlAllctnSldr[2])) %>%
        filter(Legend %in% input$cptlAllctnMtrics)
      
      ggplotly(
        d %>%
          ggplot() +
          geom_bar(
            aes(
              x = fillingDate,
              y = Value,
              fill = Legend,
              text = paste("calendarYear:", as.character(calendarYear))
            ),
            stat = 'identity',
            position = 'stack',
            just = 1
          ) +
          scale_x_date(
            name = '',
            date_breaks = "1 year",
            date_labels = "%y"
          ) +
          scale_y_continuous(
            name = '',
            n.breaks = 7,
            labels = scales::label_number(scale_cut = scales::cut_short_scale())
          ) +
          labs(title = "Capital Allocation Chart") +
          theme_minimal()
      ) %>% layout(legend = list(orientation = 'h')) %>% style(hoverinfo = "text") %>% 
        config(
          modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
          displaylogo = FALSE  # This removes the Plotly logo, which is often desired
        ) %>% layout(
          dragmode = FALSE
        )
      
    })
    
    
    
    trnscrpt_ <- reactive({
      fmpc_earning_call_transcript(i_mstrSmbl(), quarter = input$trnscrptQrtr, year = input$trnscrptYr)
      
    })
    
    # chat_history <- reactiveVal(
    # 
    #   list(
      # list(role = "system",
      #      content = paste0("You are an expert financial analyst specializing in interpreting investor transcripts. Analyze the following excerpt from",
      #                      stkPrfl() %>% pull(companyName),
      #                      " Q",
      #                      as.character(input$trnscrptQrtr),
      #                      " ",
      #                      as.character(input$trnscrptYr),
      #                      " earnings call transcript, focusing on key financial metrics, management's outlook, and any significant strategic changes. Provide a structured response with an executive summary, main points using bullet points, and suggest follow-up questions for investors. Consider current market conditions and Company X's position in the tech industry when providing your analysis.",
      #                      " Format response in HTML for R Shiny renderUI! (for example, for bold, use a div with appropriate style parameter). This is the transcript: "#,
      #                      #substr(trnscrpt_() %>% pull(content), 1, 5000)
      #      )
      # )
    #                                  
    # ))
    
    # typeof(fmpc_security_profile('AAPL') %>% pull(companyName))
    # typeof(fmpc_earning_call_transcript() %>% pull(content))
    # 
    #### transcript ----
    
    output$trnscrpt <- renderUI({
      tx_d <- trnscrpt_()
      if (is.null(tx_d)) {
        return(tags$p("Transcript not available for this quarter"))
      }
      
      output <- ""
      for (line in strsplit(tx_d %>% pull(content), "\n")[[1]]) {
        if (grepl(": ", line)) {
          output <- paste(output, "<br>", "<h4> > </h4>", line, "<br>")
        } else {
          output <- paste(output, line)
        }
      }
      output <- trimws(output) # remove leading/trailing whitespace
      
      return(HTML(output))
    })
    
    
    observeEvent(input$oaiBtn, {
      
      if (nchar(input$i_oai) > 500) {
        return(HTML("The question is too long."))
      }
      q_ <- list(list(role = "user", content = input$i_oai))
      
      prompt <- list(list(
        role = "system",
        content = paste0(
          "You are an expert financial analyst specializing in interpreting investor transcripts. Analyze the following excerpt from",
          stkPrfl() %>% pull(companyName),
          " Q",
          as.character(input$trnscrptQrtr),
          " ",
          as.character(input$trnscrptYr),
          " earnings call transcript, focusing on key financial metrics, management's outlook, and any significant strategic changes. Provide a structured response with an executive summary, main points using bullet points, and suggest follow-up questions for investors. Consider current market conditions and Company X's position in the tech industry when providing your analysis.",
          " Format response in HTML for R Shiny renderUI! don't reflect this in the response (for example, for bold, use a div with appropriate style parameter). This is the transcript: ",
          # substr(
            trnscrpt_() %>% pull(content)
          # , 1, 5000)
        )
      ))
      
      msgs <- c(prompt, q_)
      
      completion <- create_chat_completion(
        model = "gpt-4o-mini",
        messages = msgs,
        max_tokens = 500,
        temperature = 0.25
      )
      
      resp_ <- list(list(role = "assistant", content = completion$choices$message.content))
      updateTextAreaInput(session, "i_oai", value = "")
      
      msgs <- c(msgs, resp_)
      output$chtHst <- renderUI({
        # print(msgs)
        return(HTML(paste(sapply(msgs[-1], function(msg) {
          paste0(toupper(msg$role), ": ", msg$content, "<br> <br>")
        }), collapse = "")))
      })
    })
    
    #### gradient ----
    
    output$stkRtChng <- renderPlotly({
      ggplotly(
        req(stkFdmntlsLng()) %>%
          filter(
            between(calendarYear, input$fndmtlsSldr[1], input$fndmtlsSldr[2])
          ) %>%
          filter(Legend %in% input$fndmtlsMtrics) %>%
          group_by(Legend) %>% mutate(Value = Value - dplyr::lag(
            Value, n = 1, order_by = fillingDate
          )) %>%
          ggplot() +
          geom_line(aes(
            x = fillingDate, y = Value, col = Legend
          ), linewidth = 1) +
          geom_point(
            aes(x = fillingDate, y = Value, col = Legend),
            size = 1,
            show.legend = FALSE
          ) +
          scale_y_continuous(
            n.breaks = 10,
            trans = ifelse(input$fndmtlsLg, 'log', 'identity'),
            labels = scales::label_number(scale_cut = scales::cut_short_scale())
          ) +
          labs(x = '', y = '', title = "Fundamentals YoY Gradient") +
          theme_minimal()
      ) %>% layout(legend = list(orientation = 'h'))
      
    })
    
    
    #### performance ----
    
    output$sp5Prfmnc <- renderPlotly({
      # sp500 index
      sp5 <- sp5()
      # add to price
      pr_1 <- req(stkPrc()) %>% filter(date >= input$i_stkMdlSldr[1]) %>% left_join(sp5 %>% mutate(close_mkt = close) %>% select(date, close_mkt),
                                                                                    by = c('date'))
      # scale performance
      pr_2 <- pr_1 %>% group_by(symbol) %>% arrange(date) %>% mutate(
        close_fst = close / first(close, order_by = date),
        close_mkt_fst = close_mkt / first(close_mkt, order_by = date),
        returnVsMarket = round(close_fst / close_mkt_fst, 2)
      )
      
      ggplotly(
        pr_2 %>% ggplot() + geom_line(aes(x = date, y = returnVsMarket), col = '#FF7851') + geom_hline(
          yintercept = 1,
          col = '#56CC9D',
          alpha = 0.5,
          linetype = 'dashed'
        ) + labs(title = "Performance vs. SP500") + theme_minimal()
      ) %>% layout(showlegend = FALSE)
    })
    
  })
  
}

shinyApp(ui, server)
