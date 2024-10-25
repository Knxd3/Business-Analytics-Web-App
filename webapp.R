library(shiny)
library(dplyr)
library(tidyr)
library(DT)
# library(kableExtra)
library(ggplot2)
library(plotly)
library(fmpcloudr)
# library(openai)
# library(lubridate)
library(Cairo)
library(bslib)
# library(shinyauthr)
# library(RSQLite)
# library(sodium)
library(thematic)
# library(shinybrowser)
# library(shinyjs)

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
# bootswatch_themes()
# load functions
source('webfunctions.R')

# autosuggest_ <- fmpc_symbols_available() %>% filter(type == "stock" ) %>% filter(exchangeShortName %in% c("NYSE", "NASDAQ", "JPX", "LSE", "HKSE", "ASX", "SHH", "SHZ", "XETRA", "EURONEXT", "PNK", "OTC", "BSE")) %>% select(symbol, name)
# #
# # autosuggest_ <- autosuggest_ %>% group_by(exchangeShortName, exchange) %>% summarise(n = n())
# 
# autosuggest_['name'][is.na(autosuggest_['name'])] <- ""
# 
# write.csv(autosuggest_ %>% mutate(name_f = paste0(name, " (", symbol, ")")), 'availab_symbols.csv', row.names = F)
# 


body_width <- 8

#### shiny UI ----
ui <- fluidPage(

  tags$head(tags$style(
    HTML(
      "
      @media (max-width: 1340px) {
      .navbar-nav {
      margin-left: 10px;
      }
      }
      @media (min-width: 1340px) {
        .navbar-nav {
          width: 100%;
        }
        
        .navbar-nav > li:nth-child(10) {
          margin-left: auto;
          margin-right: 10px;
          
        }
      }
      .navbar-nav > li:nth-child(10) > a {
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
      .custom-container { width: 100%; background-color: #fcfcfc; /* Light gray background */ padding: 20px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); /* Bottom shade */ margin-bottom: 20px; margin-right: 0px; }
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
    
  
    id = "navbar", 
    theme = bs_theme(bootswatch = "minty",), 
    collapsible = TRUE, 
    
    # title = "Value Quant Investment Platform - v1.0",
    
    title = tags$img(src = "logo2-removebg-preview.png", width = "300px;", style = "margin-right: 5px;"),
    
    header = div(
      class = "custom-container",
      fluidRow(
        column(
          width = 12,
          style = "display: flex; justify-content: center; align-items: center; margin-top: 10px;",
          
          div(
            style = "display: flex; align-items: center; width: 100%; max-width: 600px; margin: 0 auto;",
            
            div(
              style = "flex: 1; margin-right: 10px; min-width: 0;",
              selectizeInput(
                inputId = "mstrSmbl",
                label = NULL,
                choices = NULL,
                multiple = FALSE,
                selected = NULL,
                options = list(
                  create = FALSE,
                  placeholder = "Search company name or ticker...",
                  maxItems = 1,
                  onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                  onType = I("function (str) {if (str === '') {this.close();}}"),
                  onItemAdd = I("function() {this.close();}"),
                  onInitialize = I('function() { this.setValue(""); }')
                ),
                width = '100%'
              )
            ),
            
            div(
              style = "flex-shrink: 0; margin-top: -10px;",
              actionButton(
                inputId = 'mstrSmblBtn',
                label = NULL,
                icon = icon("search"),
                style = "
        padding: 10px 20px;
        font-size: 16px;
        font-weight: bold;
        border: none;
        border-radius: 5px;
        cursor: pointer;
        transition: background-color 0.3s, box-shadow 0.3s;
        height: 38px;
        line-height: 1;
        display: flex;
        align-items: center;
        justify-content: center;
      "
              )
            )
          )
        )
      )
    ), 
  
  
    footer = fluidRow(column(width = 10, offset = 1, tagList(
      div(
        class = "footer",
        "© 2024 Value Quant Investment.",
        tags$a(href = "#href", "Visit the substack.")
      )
    ))),
    
    #### tab Welcome ----
    
    tabPanel(
      title = "Welcome",
      id = "Welcome",
      # News/Press Release
      div(
        class = "container-fluid",
        style = "max-width: 1200px; margin: auto;",
        div(
          class = "card shadow-sm", 
          div(
          class = "card-body", 
          div(
            class = "col-12", 
            tabsetPanel(
            type = "tabs",
            tabPanel("News", 
                     uiOutput("nws")),
            tabPanel("Press Release", 
                     uiOutput('prssRls'))
          ))
        ))
      )
      
    ), 
    #### tab Macro ----
    tabPanel(
      title = "Macro",
      id = "Macro",
      div(
        class = "container-fluid",
        style = "max-width: 1200px; margin: auto;",
        
        # First row with two plots side by side
        div(
          class = "row mb-4",
          # Commodities
          div(
            class = "col-md-6",
            div(
              class = "card shadow-sm",
              div(
                class = "card-body",
                h5(class = "card-title d-flex justify-content-between align-items-center", 
                   'Commodities',
                   # actionButton("info_cmmdts", "ℹ️", class = "btn btn-sm btn-outline-secondary")
                ),
                plotlyOutput('cmdts', height = '700px') %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
              )
            )
          ),
          
          # Global Market Index
          div(
            class = "col-md-6",
            div(
              class = "card shadow-sm",
              div(
                class = "card-body",
                h5(class = "card-title d-flex justify-content-between align-items-center", 
                   'Global Market Index',
                   # actionButton("info_mrkts", "ℹ️", class = "btn btn-sm btn-outline-secondary")
                ),
                plotlyOutput('mrkts', height = '700px') %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
              )
            )
          )
        ),
        
        # Second row with one plot underneath
        div(
          class = "row",
          # Economic Indicators
          div(
            class = "col-12",
            div(
              class = "card shadow-sm",
              div(
                class = "card-body",
                h5(class = "card-title d-flex justify-content-between align-items-center", 
                   'Economic Indicators',
                   # actionButton("info_ecn", "ℹ️", class = "btn btn-sm btn-outline-secondary")
                ),
                plotlyOutput('ecn', height = '2200px') %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
              )
            )
          )
        ),
        
       
      )
    ), 
  #### tab general ----
  tabPanel(
    title = "General",
    id = "General",
    div(
      class = "container-fluid",
      style = "max-width: 1200px; margin: auto;",
      
      div(
        class = "row",
        
        # Left column (8 units wide)
        div(
          class = "col-md-8",
          div(
            class = "card shadow-sm mb-4",
            div(
              class = "card-body",
              h5(class = "card-title", "Stock Price"),
              plotlyOutput('stkP') %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
            )
          ),
          div(
            class = "card shadow-sm mb-4",
            div(
              class = "card-body",
              h5(class = "card-title", "Financial Summary"),
              div(style = "overflow-x: auto;", tableOutput("fnnclSmmry")) #%>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
            )
          ),
          div(
            class = "card shadow-sm mb-4",
            div(
              class = "card-body",
              h5(class = "card-title", "Description"),
              uiOutput('dscrpt') %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
            )
          ),
          div(
            class = "card shadow-sm",
            div(
              class = "card-body",
              h5(class = "card-title", "Profile"), 
              tableOutput("prfl") %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
            )
          ),
          div(
            class = "card shadow-sm",
            div(
              class = "card-body",
              h5(class = "card-title", "Analyst Guidance"),
              plotlyOutput("anlstEstmts")
            )
          )
        ),
        
        # Right column (4 units wide)
        div(
          class = "col-md-4",
          div(
            class = "card shadow-sm",
            div(
              class = "card-body",
              tabsetPanel(
                type = "tabs",
                tabPanel("News", uiOutput('stkNws')), #%>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5),
                tabPanel("Press Release", uiOutput('stkPrs')) #%>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
              )
            )
          )
        )
      )
    ),
    # tags$head(
    #   tags$script(HTML("
    #   var tabRendered = {};
    #   Shiny.addCustomMessageHandler('tabRendered', function(message) {
    #   if (message.reset) {
    #   tabRendered = {};
    #   console.log('All tabs reset');
    #   }
    #    else if (!tabRendered[message.tabName]) {
    #       console.log(message.tabName + ' has finished rendering');
    #       tabRendered[message.tabName] = true;
    #       Shiny.setInputValue('tabReady', message.tabName);
    #     }
    #   });
    # "))
    # )
  ),
  #### tab fundamentals ----
  tabPanel(
    title = 'Fundamentals',
    id = "Fundamentals",
    div(
      class = "container-fluid",
      style = "max-width: 1400px; margin: auto;",
      
      div(
        class = "row",
        
        # Sidebar
        div(
          class = "col-md-3",
          div(
            class = "card shadow-sm mb-4",
            div(
              class = "card-body",
              h5(class = "card-title", "Controls"),
              selectInput(
                'fndmtlsMtrics',
                "Display Fundamentals:",
                choices = c(
                  'revenue', 'operatingIncome', 'netIncome', 'freeCashFlow', 'inventory',
                  'dividendsPaid', 'weightedAverageShsOutDil', 'propertyPlantEquipmentNet',
                  'cashAndCashEquivalents', 'totalAssets', 'interestExpense', 'totalLiabilities',
                  'retainedEarnings', 'debtRepayment', 'totalDebt', 'longTermDebt',
                  'capitalExpenditure', 'netRepurchases', 'netInvestments',
                  'stockBasedCompensation', 'netAcquisitions', 'researchAndDevelopmentExpenses'
                ),
                multiple = TRUE,
                selected = c('freeCashFlow', 'netIncome', 'OperatingIncome', 'dividendsPaid', 'stockBasedCompensation')
              ),
              sliderInput(
                'fndmtlsSldr',
                "Window:",
                min = 1995,
                max = as.integer(format(Sys.Date(), '%Y')),
                value = c(2007, as.integer(format(Sys.Date(), '%Y'))),
                sep = '',
                animate = FALSE,
                step = 1
              ),
              checkboxInput('fndmtlsLg', 'Log Scale', FALSE),
              selectInput(
                'sctr',
                'Comparison Cohort:',
                choices = c(
                  'All Sectors', "Healthcare", "Basic Materials", "Industrials",
                  "Consumer Cyclical", "Technology", "Real Estate", "Consumer Defensive",
                  "Communication Services", "Energy", "Financial"
                ),
                selected = 'All Sectors'
              )
            )
          )
        ),
        
        # Main content
        div(
          class = "col-md-9",
          div(
            class = "card shadow-sm mb-4",
            div(
              class = "card-body",
              h5(class = "card-title", "Stock Fundamentals"),
              plotlyOutput('stkFndmt') %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
            )
          ),
          div(
            class = "card shadow-sm mb-4",
            div(
              class = "card-body",
              h5(class = "card-title", "Relative Fundamentals"),
              plotOutput('stkFndmntlsRltv', height = "600px")
            )
          ),
          div(
            class = "card shadow-sm",
            div(
              class = "card-body",
              h5(class = "card-title", "Financial Statements"),
              tabsetPanel(
                type = "tabs",
                tabPanel("Income", DTOutput('stkInc_', height = "600px")),
                tabPanel("Balance", DTOutput('stkBal_', height = "600px")),
                tabPanel("Cash Flow", DTOutput('stkCF_', height = "600px"))
              )
            )
          )
        )
      )
    )
  ),
  
#### tab model ----
tabPanel(
  title = "Model",
  id = "Model",
  div(
    class = "container-fluid",
    style = "max-width: 1400px; margin: auto;",
    
    div(
      class = "row",
      
      # Sidebar
      div(
        class = "col-md-3",
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-body",
            h5(class = "card-title", "Model Controls"),
            sliderInput(
              "i_stkMdlSldr",
              "Window:",
              min = as.Date('1995-01-01', '%Y-%m-%d'),
              max = as.Date(Sys.Date(), '%Y-%m-%d'),
              value = c(
                as.Date('2007-01-01', '%Y-%m-%d'),
                as.Date(Sys.Date(), '%Y-%m-%d')
              ),
              animate = FALSE,
              timeFormat = '%Y-%m-%d',
              step = 31
            ),
            radioButtons(
              'i_mdlMtrc',
              "Profit Metric (per Share):",
              choices = c('Net Income' = 'epsdiluted', 
                          'Free Cash Flow' = 'fcfps', 
                          'Operating Earnings' = 'operatingps', 
                          'Revenue' = 'revenueps'),
              selected = 'epsdiluted',
              inline = TRUE
            ),
            numericInput(
              'i_mdlMltpl',
              'Overwrite Multiple:',
              value = 0,
              min = 0,
              max = 1000,
              step = 1
            ),
            numericInput(
              'i_mdlSplns',
              'Set Smoothness:',
              min = 1,
              max = 7,
              step = 1,
              value = 2
            ),
            dateInput(
              'i_trgtFrct',
              'Set Forecast End:',
              min = as.Date(Sys.Date() + 180),
              max = as.Date(Sys.Date() + 7200),
              value = as.Date(Sys.Date() + 360 * 3),
              format = "yyyy-mm",
              weekstart = 1,
              startview = "year"
            ),
            checkboxInput(
              'i_mdlLg',
              'Log Scale',
              FALSE
            )
          )
        )
      ),
      
      # Main content
      div(
        class = "col-md-9",
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-body",
            h5(class = "card-title", "Stock Model"),
            plotlyOutput('stkMdl', height = "650px") %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
          )
        ),
        div(
          class = "card shadow-sm",
          div(
            class = "card-body",
            h5(class = "card-title", "Model Data"),
            DTOutput('stkMdlTbl')
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
  div(
    class = "container-fluid",
    style = "max-width: 1400px; margin: auto;",
    
    div(
      class = "row",
      
      # Sidebar
      div(
        class = "col-md-3",
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-body",
            h5(class = "card-title", "Valuation Controls"),
            selectInput(
              "i_vltnSldr",
              "Select Facets:",
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
              selectize = TRUE
            ),
            sliderInput(
              "i_stkVltnSldr",
              "Window:",
              min = as.Date('1995-01-01', '%Y-%m-%d'),
              max = as.Date(Sys.Date(), '%Y-%m-%d'),
              value = c(
                as.Date('2007-01-01', '%Y-%m-%d'),
                as.Date(Sys.Date(), '%Y-%m-%d')
              ),
              animate = FALSE,
              timeFormat = '%Y-%m-%d'
            ),
            numericInput(
              'i_vltnMvngAvg',
              'Set Moving Average:',
              value = 2,
              min = 2,
              max = 15,
              step = 1
            ),
            checkboxInput('i_vltnLg', 'Log Scale', FALSE),
            # checkboxInput('i_vltnInv', 'Yield', FALSE)
          )
        )
      ),
      
      # Main content
      div(
        class = "col-md-9",
        div(
          class = "card shadow-sm",
          div(
            class = "card-body",
            h5(class = "card-title", "Valuation Chart"),
            plotlyOutput('vltn', height = "1500px") %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
          )
        )
      )
    ),
    
    #### dcf gauge ----
    
    div(
      class = "row",
      
      # Second Sidebar
      div(
        class = "col-md-3",
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-body",
            h5(class = "card-title", "Fair Value Controls"),
            numericInput(
              inputId = "i_fvHrzn",
              label = "Projection Period (years):",
              value = 5,
              min = 1,
              max = 20,
              step = 1
            ),
            numericInput(
              inputId = "i_grwthRt",
              label = "Growth Rate (%):",
              value = 5,
              min = -10.0,
              max = 50.0,
              step = 0.1
            ),
            numericInput(
              inputId = "i_dscntRt",
              label = "Discount Rate (%)",
              value = 5,
              min = 0.0,
              max = 20.0,
              step = 0.1
            ),
            numericInput(
              inputId = "i_extMltpl",
              label = "Exit Multiple",
              value = 10,
              min = 1,
              max = 50,
              step = 1
            ),
            numericInput(
              inputId = "i_strtFv",
              label = "Start Value (M)",
              value = 1,
              min = 0.0,
              max = 10^9,
              step = 0.1
            )
          )
        )
      ),
      
      # Second Main Content
      div(
        class = "col-md-9",
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-body",
            h5(class = "card-title", "Discounted Fair Value"),
            plotlyOutput('mdlGauge'),
            wellPanel(
              h5('Control Helpers:'),
              textOutput(outputId = "dcf_controls1"), # smooth model end value implied/ linear model implied rate / 
              textOutput(outputId = 'dcf_controls2')#latest stock price
            ),
            wellPanel(
              h5('DCF Results:'),
              textOutput(outputId = "dcf_results1"),
              textOutput(outputId = "dcf_results2"),
              textOutput(outputId = "dcf_results3"),
              textOutput(outputId = "dcf_results4")
            )
          )
        )
      )
    )
  )
),

#### tab capital allocation ----
tabPanel(
  title = "Capital Allocation",
  id = "Capital Allocation",
  div(
    class = "container-fluid",
    style = "max-width: 1400px; margin: auto;",
    
    div(
      class = "row",
      
      # Sidebar
      div(
        class = "col-md-3",
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-body",
            h5(class = "card-title", "Capital Allocation Controls"),
            selectInput(
              'cptlAllctnMtrics',
              "Display Activity:",
              choices = c(
                "Capital Expenditure" = "capitalExpenditure",
                "Dividends Paid" = "dividendsPaid",
                "Net Debt Repayment" = "netDebtRepayment",
                "Net Acquisitions" = "netAcquisitions",
                "Net Investments" = "netInvestments",
                "Net Repurchases" = "netRepurchases"
              ),
              multiple = TRUE,
              selected = c("capitalExpenditure", "dividendsPaid", "netAcquisitions", "netRepurchases")
            ),
            sliderInput(
              'cptlAllctnSldr',
              "Window:",
              min = 1995,
              max = as.integer(format(Sys.Date(), '%Y')),
              value = c(2007, as.integer(format(Sys.Date(), '%Y'))),
              sep = '',
              animate = FALSE,
              step = 1
            )
          )
        )
      ),
      
      # Main content
      div(
        class = "col-md-9",
        div(
          class = "card shadow-sm",
          div(
            class = "card-body",
            h5(class = "card-title", "Capital Allocation Chart"),
            plotlyOutput('cptlAllctn', height = "650px") %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
          )
        )
      )
    )
  )
),

#### Transcripts ----
tabPanel(
  title = "Financials",
  id = "Financials",
  
  div(
    class = "container-fluid",
    style = "max-width: 1400px; margin: auto;",
    
    navset_tab(
      nav_panel("Transcripts", 
                div(
                  class = "row",
                  
                  # Sidebar
                  div(
                    class = "col-md-3",
                    div(
                      class = "card shadow-sm mb-4",
                      div(
                        class = "card-body",
                        h5(class = "card-title", "Transcript Controls"),
                        numericInput(
                          "trnscrptYr",
                          "Select Year:",
                          value = as.integer(format(Sys.Date(), "%Y")),
                          min = 1990,
                          max = as.integer(format(Sys.Date(), "%Y")) + 2,
                          step = 1
                        ),
                        numericInput(
                          "trnscrptQrtr",
                          "Select Quarter:",
                          value = 1,
                          min = 1,
                          max = 4,
                          step = 1
                        ),
                        hr(),
                        h6("AI Assistant"),
                        textAreaInput(
                          "i_oai",
                          "Ask the AI assistant:",
                          rows = 3,
                          value = "Summarise material information.",
                          placeholder = "Summarise the quarter's material investor information."
                        ),
                        actionButton('oaiBtn', 'Submit', class = "btn btn-primary mt-2", disabled = TRUE),
                        hr(),
                        h6("Chat History"),
                        uiOutput("chtHst") %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
                      )
                    )
                  ),
                  
                  # Main content
                  div(
                    class = "col-md-9",
                    div(
                      class = "card shadow-sm",
                      div(
                        class = "card-body",
                        h5(class = "card-title", "Transcript"),
                        uiOutput('trnscrpt') %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
                      )
                    )
                  )
                )
      ),
      
      #### Form 10-K ----
      
      nav_panel("SEC Filings", 
                div(
                  class = "row",
                  
                  # Sidebar
                  div(
                    class = "col-md-3",
                    div(
                      class = "card shadow-sm mb-4",
                      div(
                        class = "card-body",
                        h5(class = "card-title", "SEC Form Controls"),
                        
                        selectInput(
                          "filingType",
                          "Select Filing Type:",
                          choices = c("10-Q" = "10-q", "10-K" = "10-k", "20-F" = "20-f"),
                          selected = "10-k"
                        ),
                        selectInput(
                          "filingDate",
                          "Select Filing Date:",
                          choices = NULL
                        ),
                      hr(),
                      # h6("AI Assistant"),
                      # textAreaInput(
                      #   "i_oai10k",
                      #   "Ask the AI assistant:",
                      #   rows = 3,
                      #   value = "Summarise material information.",
                      #   placeholder = "Summarise the notes to the financial statements."
                      # ),
                      actionButton('edgarButton', 'Summarise Notes', class = "btn btn-primary mt-2", disabled = FALSE)#,
                      # hr(),
                      # h6("Chat History"),
                      # uiOutput("chtHst10k") %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
                      )
                      )
                  ), 
                  
                  # Main content (updated)
                  div(
                    class = "col-md-9",
                    div(
                      class = "card shadow-sm",
                      div(
                        class = "card-body",
                        h5(class = "card-title", "Form 10-K/10-Q/20-F"),
                        tabsetPanel(
                          id = "mainTabset",
                          tabPanel(
                            "Filing Content",
                            div(
                              style = "max-height: 800px; overflow-y: auto; padding: 15px;",
                              uiOutput('trnscrpt10k') %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
                            )
                          ),
                          tabPanel(
                            "Notes to Financial Statements",
                            div(
                              style = "max-height: 800px; overflow-y: auto; padding: 15px;",
                              uiOutput('edgarNotes') %>% shinycssloaders::withSpinner(type = 5, color = "#0dc5c1", size = 0.5)
                            )
                          )
                        )
                      )
                    )
                  )
                
       )
      ),
      #### Sales Breakdown ----
      
      nav_panel("Sales Breakdown",
                div(
                  class = "row",
                # Main content
                div(
                  class = "col-md-6",
                  div(
                    class = "card shadow-sm",
                    div(
                      class = "card-body",
                      h5(class = "card-title", "Revenue Breakdown - Product"),
                      plotlyOutput('revenue-product', height = "650px")
                    )
                  )
                ),
                div(
                  class = "col-md-6",
                  div(
                    class = "card shadow-sm",
                    div(
                      class = "card-body",
                      h5(class = "card-title", "Revenue Breakdown - Geography"),
                      plotlyOutput('revenue-geo', height = "650px")
                    )
                  )
                )
                )
            )
  )
)
),

# #### tab screener -----
# tabPanel(
#   title = 'Screener',
#   id = 'screener',
#   div(
#     class = "container-fluid",
#     style = "max-width: 1400px; margin: auto;",
#     div(
#       class = "row",
#       
#       # Sidebar
#       div(
#         class = "col-md-3",
#         div(
#           class = "card-body",
#           h5(class = "card-title", "Screener Controls"),
#           dateInput(
#             'i_startScreener',
#             'Set End of Screener Window:',
#             min = as.Date("2007-01-01"),
#             max = as.Date("2024-08-01"),
#             value = as.Date("2008-08-01"),
#             format = "yyyy-mm",
#             weekstart = 1,
#             startview = "year"
#           ),
#           dateInput(
#             'i_endScreener',
#             'Set End of Screener Window:',
#             min = as.Date("2007-01-01"),
#             max = as.Date("2024-08-01"),
#             value = as.Date("2024-08-01"),
#             format = "yyyy-mm",
#             weekstart = 1,
#             startview = "year"
#           ),
#           numericInput(
#             'i_screenerToPlot',
#             'Companies to plot:',
#             value = 5,
#             min = 1,
#             max = 15,
#             step = 1
#           )
#         )
#       ),
#       
#       
#       # Main Panel
#       
#       div(
#         class = "col-md-9",
#         div(
#           class = "card shadow-sm",
#           div(
#             class = "card-body",
#             h5(class = "card-title", "Performance vs. SP500"),
#             plotlyOutput('screenerPlot')
#           )
#         )
#       )
#       
#       
#     )
#   )
# ),


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







server <- function(input, output, session) {
  

#### DATA LOADING ----
  
# faster query via db
con <- RSQLite::dbConnect(RSQLite::SQLite(), "datadb.db")

# load search suggestions
autosuggest_ <- RSQLite::dbGetQuery(con, "SELECT * FROM availab_symbols")
autosuggest_ <- setNames(autosuggest_$symbol, autosuggest_$name_f)





# update auto-suggest
updateSelectizeInput(
  session,
  'mstrSmbl',
  selected = "Meta Platforms, Inc. (META)",
  choices = autosuggest_,
  server = TRUE,
  options = list(maxOptions = 15)
)






{
  ## market history
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
}

{
## econ
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
}





# load gspc price
sp5 <- reactive({
  print('sp5')
  fmpc_price_history(symbols = "^GSPC", startDate = input$i_stkMdlSldr[1]) %>% select(symbol, date, close)
})

# load other markets price
indxs <- reactive({
  print('indxs')
  fmpc_price_history(
    symbols = c("^AXJO", "^GSPC", "^HSI", "^STOXX50E"),
    startDate = Sys.Date() - 60
  ) %>% select(symbol, date, close)
})







# load stock price
stkPrc <- eventReactive(input$mstrSmblBtn, {
  # faster Time difference of 0.365654 secs
  # a large data set, enforce parsimony from endpoint directly
  start <- Sys.time()
  dt_1 <- as.data.frame(general.APIcall(endpoint = "Price", symbol = input$mstrSmbl, start="2005-01-01"))
  names(dt_1) <- c('symbol', 'date', 'close')
  dt_1 <- dt_1 %>% mutate(close = as.numeric(close), date = lubridate::ymd(date)) %>% arrange(date)
  print(paste('Price:',Sys.time() - start))
  return(dt_1)
})

# load stock trivia
stkPrfl <- eventReactive(input$mstrSmblBtn, {
  dt_0 <- fmpc_security_profile(input$mstrSmbl)
  return(dt_0)
})  



# load INC, BAL, FCF
stkInc <- eventReactive(input$mstrSmblBtn, {
  dt_2 <- fmpc_financial_bs_is_cf(symbols = input$mstrSmbl, statement = 'income', quarterly = FALSE, limit = 25) #%>%
  #select(symbol, calendarYear, fillingDate, date, reportedCurrency, revenue, operatingIncome, researchAndDevelopmentExpenses, sellingGeneralAndAdministrativeExpenses, interestExpense, interestIncome, netIncome, weightedAverageShsOutDil, epsdiluted)
  return(dt_2)
})
stkBal <- eventReactive(input$mstrSmblBtn, {
  dt_3 <- fmpc_financial_bs_is_cf(symbols = input$mstrSmbl, statement = 'balance', quarterly = FALSE, limit = 25) #%>%
  #select(symbol, calendarYear, fillingDate, date, reportedCurrency, totalStockholdersEquity, goodwill, intangibleAssets, totalAssets, totalLiabilities, totalCurrentLiabilities, cashAndCashEquivalents, totalDebt)
  return(dt_3)
})
stkCF <- eventReactive(input$mstrSmblBtn, {
  dt_4 <- fmpc_financial_bs_is_cf(symbols = input$mstrSmbl, statement = 'cashflow', quarterly = FALSE, limit = 25) #%>%
  #select(symbol, calendarYear, fillingDate, date, reportedCurrency, freeCashFlow, dividendsPaid, capitalExpenditure, purchasesOfInvestments, salesMaturitiesOfInvestments, commonStockRepurchased, commonStockIssued, debtRepayment, otherFinancingActivites, acquisitionsNet,     netIncome, inventory, depreciationAndAmortization)
  
  
})

# parse company data
stkFDta <- eventReactive(input$mstrSmblBtn, {
  
  start <- Sys.time()
  dt_2 <- req(stkInc())
  dt_3 <- req(stkBal())
  dt_4 <- req(stkCF())
  print(paste('BS IS CF:', Sys.time() - start))
  
  start <- Sys.time()
  # handle conversion for foreign stocks
  if (req(stkPrfl())$currency[1] != "USD") {
    
    # faster Time difference of 1.252749 secs
    start <- Sys.time()
    # Create currency pairs
    currency_pairs <- paste0(c("BRL", "CAD", "CNY", "EUR", "HKD", "AUD", "JPY", "SGD", "TWD"), "USD")
    # Fetch FX data
    fxs <- lapply(currency_pairs, fx.APIcall)
    # Extract symbols and rates in one operation
    fxs_data <- vapply(fxs, function(x) c(x$symbol, x$price), character(2))
    # Create data frame
    fxs <- data.frame(symbol = fxs_data[1,], price = as.numeric(fxs_data[2,]), stringsAsFactors = FALSE)
    
    convert_multiple <- function(dt_list, fxs) {
      lapply(dt_list, function(dt) convert.c(dt, fxs))
    }
    converted_dts <- convert_multiple(list(dt_2, dt_3, dt_4), fxs)
    dt_2 <- converted_dts[[1]]
    dt_3 <- converted_dts[[2]]
    dt_4 <- converted_dts[[3]]
    
    # print(Sys.time() - start)
  }
  print(paste('FX:', Sys.time() - start))
  
  # faster
  start <- Sys.time()
  f_dt <- dt_2 %>% 
    inner_join(dt_3, by = c('symbol', 'calendarYear')) %>% 
    inner_join(select(dt_4, -netIncome, -inventory, -depreciationAndAmortization),
               by = c('symbol', 'calendarYear')) %>% 
    select(-matches('\\.x|\\.y|cik|link|dDate')) %>%
    select(symbol, calendarYear, date, fillingDate, reportedCurrency, everything()) %>%
    arrange(symbol, desc(calendarYear)) %>%
    mutate(fillingDate = as.Date(fillingDate),
           calendarYear = as.integer(calendarYear))
  
  print(paste('Joining:', Sys.time() - start))
  
  start <- Sys.time()
  
  
  # faster
  start <- Sys.time()
  f_dt <- f_dt %>%
    mutate(
      netInvestments = abs(purchasesOfInvestments) - abs(salesMaturitiesOfInvestments),
      netRepurchases = abs(commonStockRepurchased) - abs(commonStockIssued),
      netDebtRepayment = abs(debtRepayment) - abs(otherFinancingActivites),
      bookValue = totalStockholdersEquity - goodwill - intangibleAssets,
      
      across(c('acquisitionsNet', 'capitalExpenditure', 'commonStockRepurchased', 'dividendsPaid', 'interestExpense'), abs),
      
      inv_weightedAverageShsOutDil = ifelse(weightedAverageShsOutDil == 0, 0, 1 / weightedAverageShsOutDil),
      inv_revenue = ifelse(revenue == 0, 0, 1 / revenue),
      inv_operatingIncome = ifelse(operatingIncome == 0, 0, 1 / operatingIncome),
      
      fcfps = freeCashFlow * inv_weightedAverageShsOutDil,
      divps = dividendsPaid * inv_weightedAverageShsOutDil,
      bookps = (totalStockholdersEquity - goodwill) * inv_weightedAverageShsOutDil,
      netdebtps = (totalDebt - cashAndCashEquivalents) * inv_weightedAverageShsOutDil,
      operatingps = operatingIncome * inv_weightedAverageShsOutDil,
      revenueps = revenue * inv_weightedAverageShsOutDil,
      
      debt = ifelse(totalAssets == 0, 0, totalLiabilities / totalAssets),
      RD = researchAndDevelopmentExpenses * inv_revenue,
      SA = sellingGeneralAndAdministrativeExpenses * inv_revenue,
      operatingMargin = operatingIncome * inv_revenue,
      ROE = ifelse(totalStockholdersEquity - goodwill == 0, 0, operatingIncome / (totalStockholdersEquity - goodwill)),
      ROIC = ifelse(totalAssets - totalCurrentLiabilities - cashAndCashEquivalents == 0, 0,
                    operatingIncome / (totalAssets - totalCurrentLiabilities - cashAndCashEquivalents)),
      CAPEX = capitalExpenditure * inv_revenue,
      netRepurchasesRevenue = (commonStockRepurchased - commonStockIssued) * inv_revenue,
      divtoOpInc = dividendsPaid * inv_operatingIncome,
      netInterestExptoOpInc = (interestIncome - interestExpense) * inv_operatingIncome
    ) %>%
    select(all_of(c(
      'symbol', 'calendarYear', 'date', 'fillingDate', 'reportedCurrency',
      'revenue', 'operatingIncome', 'netIncome', 'freeCashFlow', 'inventory',
      'dividendsPaid', 'weightedAverageShsOutDil', 'propertyPlantEquipmentNet',
      'cashAndCashEquivalents', 'totalAssets', 'interestExpense', 'totalLiabilities',
      'retainedEarnings', 'debtRepayment', 'totalDebt', 'longTermDebt', 'sellingGeneralAndAdministrativeExpenses',
      'capitalExpenditure', 'stockBasedCompensation', 'researchAndDevelopmentExpenses',
      'fcfps', 'divps', 'bookps', 'operatingps', 'revenueps', 'debt', 'RD', 'SA', 'operatingMargin',
      'ROE', 'ROIC', 'CAPEX', 'netRepurchasesRevenue', 'divtoOpInc', 'netInterestExptoOpInc',
      'netInvestments', 'netRepurchases', 'netDebtRepayment', 'bookValue',
      'epsdiluted', 'netdebtps'
    )))
  
  print(paste('Transforms:', Sys.time() - start))
  
  return(f_dt)
})





# for fundamentals
stkFdmntlsLng <- reactive({
  print('Long data executed')
  req(stkFDta()) %>%pivot_longer(
    cols = where(is.numeric) &
      !contains(c('symbol', 'calendarYear', 'fillingDate')),
    names_to = 'Legend',
    values_to = 'Value'
  )
})


# for model
stkMdlRctv <- reactive({
  
  tryCatch({
    print('Model re-executed')
    prc_ <- isolate(stkPrc())
    prc <- prc_ %>%
      filter(between(date, input$i_stkMdlSldr[1], input$i_stkMdlSldr[2])) %>%
      mutate(calendarYear = as.integer(substr(date, 1, 4))) %>%
      mutate(mn = min(close, na.rm = T),
             mx = max(close, na.rm = T),
             mn_x = min(date, na.rm = T))
    
    fndmt_ <- isolate(stkFDta())
    fndmt <- fndmt_ %>% select(symbol, calendarYear, fillingDate, !!(input$i_mdlMtrc)) %>% rename('profit' = !!(input$i_mdlMtrc))
    
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
  }, error = function(e) {
    print(e)
  }
  )
})





i_mstrSmbl <- reactiveVal(NULL)

# update various UI when button clicked
observeEvent(input$mstrSmblBtn, {
  
  # point user to stock page
  if (input$navbar %in% c("Welcome", "Macro")) {
    updateTabsetPanel(session, "navbar", selected = "General")
  }
  
  # enable buttons
  updateActionButton(session, 'oaiBtn', disabled = FALSE)
  updateActionButton(session, 'oaiBtn10k', disabled = FALSE)
  
  # fill with blank to stop loading sign
  output$chtHst <- renderUI({
    return(HTML("<p></p>"))
  })
  
  output$chtHst10k <- renderUI({
    return(HTML("<p></p>"))
  })
  output$edgarNotes <- renderUI({
    return(HTML("<p></p>"))
  })
  
  
  # update dcf calculator starting point
  last_fcf <- as.numeric(req(stkCF())[which.max(req(stkCF())$calendarYear), 'freeCashFlow'])
  updateNumericInput(session, "i_strtFv", value = round(last_fcf / value_fcf(last_fcf), 2), label = label_fcf(last_fcf))
  
  output$edgarNotes <- renderUI({
    return(HTML("<p></p>"))
  })
  
  
  i_mstrSmbl(input$mstrSmbl)
  
})







# load transcripts 
trnscrpt_ <- reactive({
  fmpc_earning_call_transcript(i_mstrSmbl(), quarter = input$trnscrptQrtr, year = input$trnscrptYr)
  
})


# get the final link for filing
filing_info <- reactive({
  # print(i_filingType())
  # print(isolate(input$mstrSmbl))
  f <- general.APIcall(endpoint = "10k-sec", symbol = i_mstrSmbl(), form = input$filingType) 
  
  if (length(f) != 0) {
    f <- f %>% filter(grepl('.htm', finalLink)) %>% mutate(fillingDate = substr(fillingDate, 1, 10))
  }
  # print(f)
  return(f)
})























#### NEWS ----
output$nws <- renderUI({
  d_ <- general.APIcall(endpoint = "News") %>% filter(site != "ndtv.com")
  
  # start = Sys.time()
  
  n <- min(20, nrow(d_))
  d_subset <- d_[1:n, ]
  
  result <- vector("list", n)
  result <- Map(function(publishedDate, title, image, site, text, url) {
    create_news_container(
      type = "News",
      publishedDate = publishedDate,
      title = title,
      image = image,
      site = site,
      text = text,
      url = url
    )
  }, d_subset$publishedDate, d_subset$title, d_subset$image, d_subset$site, d_subset$text, d_subset$url)
  
  # # print(Sys.time() - start)
  
  return(result)
  
  
})



















#### PRESS RELEASE  ----

output$prssRls <- renderUI({
  d_ <- general.APIcall(endpoint = "Press-Release")
  
  # faster
  # start = Sys.time()
  n <- min(20, nrow(d_))
  d_subset <- d_[1:n, ]
  
  
  result <- vector("list", n)
  result <- Map(function(symbol, date, title, text) {
    create_news_container(
      type = "Press-Release",
      symbol = symbol,
      date = date,
      title = title,
      text = text
    )
  }, d_subset$symbol, d_subset$date, d_subset$title, d_subset$text)
  # # print(Sys.time() - start)
  
  return(result)
})























#### ECON ----

output$ecn <- renderPlotly({
  # start <- Sys.time()
  result <- RSQLite::dbGetQuery(con, "SELECT * FROM economic_indicators WHERE Indicator in ('totalVehicleSales',
         'federalFunds',
         'inflation',
         '15YearFixedRateMortgageAverage',
         '3MonthOr90DayRatesAndYieldsCertificatesOfDeposit',
         'consumerSentiment',
         'unemploymentRate',
         'commercialBankInterestRateOnCreditCardPlansAllAccounts',
         '30YearFixedRateMortgageAverage') " # AND date > '1995-01-01' ,
  ) %>% mutate(date = lubridate::ymd(date), date_added = lubridate::ymd(date_added))
  
  # # faster Time difference of 0.002198935 secs
  # start <- Sys.time()
  result <- result %>%
    group_by(IndicatorName) %>%
    mutate(mn_ = min(value) * .95) %>%
    ungroup()
  # # print(Sys.time() - start)
  
  last_run_date <- lubridate::ymd(max(result$date_added))
  
  if (Sys.Date() - lubridate::ymd(last_run_date) > 30){
    source("createdb.R")
  }
  
  p <- result %>% ggplot() +
    # geom_area(aes(x = date, y = value), alpha = 0.05, fill = '#56CC9D') +
    geom_ribbon(aes(
      x = date, ymin = mn_, ymax = value
    ), alpha = 0.05, fill = '#56CC9D') +
    geom_line(aes(x = date, y = value), colour = '#56CC9D') + #, colour = '#FF7851'
    scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
    facet_wrap(vars(IndicatorName), ncol = 1, scales = 'free') +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) + #scales::label_number(scale = 1e-3)) +
    labs(x = '', y = '') +
    theme_minimal() +
    theme(
      # panel.spacing.x = unit(-1, "lines"),
      panel.spacing.y = unit(-0.5, "lines"),
      axis.text = element_text(face = "bold", size = 10),
      strip.placement = "inside",
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
  
  # # print(paste("Econ: ", as.character(Sys.time() - start)))
  
  return(ggplotly(p) %>% style(hoverinfo = "none", traces = seq(1, 9, 1) )%>%
           config(
             modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
             displaylogo = FALSE  # This removes the Plotly logo, which is often desired
           ) %>% layout(
             dragmode = FALSE
           ))
  
})
















#### MARKETS ----

output$mrkts <- renderPlotly({
  
  start <- Sys.time()
  
  d_ <- req(indxs())
  d_ <- d_ %>%
    group_by(symbol) %>%
    mutate(mn_ = min(close) * .95) %>%
    ungroup()
  
  
  
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
  
  # # print(paste("Markets: ", as.character(Sys.time() - start)))
  
  ggplotly(
    d_ %>% ggplot() +
      # geom_area(aes(x = date, y = close, ymin = 10000), alpha = 0.15) +
      geom_ribbon(aes(
        x = date, ymin = mn_, ymax = close
      ), alpha = 0.05, fill = '#56CC9D') + #, fill = '#56CC9D'
      geom_line(aes(x = date, y = close), colour = '#56CC9D') + #, colour = '#FF7851'
      scale_x_date(date_breaks = "10 days", date_labels = "%b-%d") +
      facet_wrap(vars(Index), ncol = 1, scales = 'free') +
      scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K")) +
      labs(x = '', y = '') +
      theme_minimal() +
      theme(
        # panel.spacing.x = unit(-1, "lines"),
        # panel.spacing.y = unit(-0.5, "lines"),
        axis.text = element_text(face = "bold", size = 10),
        strip.placement = "inside",
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
  ) %>% style(hoverinfo = "none", traces = seq(1,4,1)) %>%
    config(
      modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
      displaylogo = FALSE  # This removes the Plotly logo, which is often desired
    ) %>% layout(
      dragmode = FALSE
    )
})























#### COMMODITIES ----

output$cmdts <- renderPlotly({
  
  start = Sys.time()
  
  result <- RSQLite::dbGetQuery(con, "SELECT * FROM commodities") %>% mutate(date = lubridate::ymd(date), date_added = lubridate::ymd(date_added))
  last_run_date <- lubridate::ymd(max(result$date_added))
  
  result <- result %>%
    group_by(CommodityName) %>%
    mutate(mn_ = min(close) * .95) %>%
    ungroup()
  
  if (Sys.Date() - lubridate::ymd(last_run_date) > 30){
    source(createdb.R)
  }
  
  
  # # print(paste("Commodities: ", as.character(Sys.time() - start)))
  ggplotly(
    result %>% ggplot() +
      # geom_area(aes(x = date, y = close), alpha = 0.05, fill = '#56CC9D') +
      geom_ribbon(aes(
        x = date, ymin = mn_ * .95, ymax = close
      ), alpha = 0.05, fill = '#56CC9D') +
      geom_line(aes(x = date, y = close), colour = '#56CC9D') + #, colour = '#FF7851'
      scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
      facet_wrap(vars(CommodityName), ncol = 1, scales = 'free') +
      scale_y_continuous(labels = scales::label_number()) +
      labs(x = '', y = '') +
      theme_minimal() +
      theme(
        # panel.spacing.x = unit(-1, "lines"),
        # panel.spacing.y = unit(-0.5, "lines"),
        axis.text = element_text(face = "bold", size = 10),
        strip.placement = "inside",
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
  ) %>% style(hoverinfo = "none", traces = c(1,2,3, 4) ) %>%
    config(
      modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
      displaylogo = FALSE  # This removes the Plotly logo, which is often desired
    ) %>% layout(
      dragmode = FALSE
    )#%>% style(hoverinfo = "none", traces = 1)
  
  
  
})


































#### STOCK PRICE CHART ----

output$stkP <- renderPlotly({
  start <- Sys.time()
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
  start <- Sys.time()
  
  d_ <- req(stkPrc()) %>%
    mutate(calendarYear = lubridate::ymd(paste0(format(date, "%Y"), "-12-31")))
  
  i_tr <- as.data.frame(general.APIcall(endpoint = "Insider-Trans", symbol = isolate(input$mstrSmbl)))
  
  
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
      ) %>% mutate(calendarYear = lubridate::ymd(paste0(year, "-12-31")),
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
    ) %>% mutate(calendarYear = lubridate::ymd(paste0(year, "-12-31")),
                 buySellRatio = totalBought / totalSold)
  }
  
  
  d_ <- d_ %>% left_join(i_tr, by = "calendarYear") %>% group_by(calendarYear) %>% mutate(y_ = last(close)) %>% ungroup()
  min_ = min(d_$close) * .95
  
  # print(paste("Insider: ", as.character(Sys.time() - start)))
  
  start <- Sys.time()
  
  p <- ggplotly(
    d_ %>% ggplot() +
      geom_ribbon(
        aes(
          x = date,
          ymin = min_,
          ymax = close
        ),
        alpha = 0.05,
        fill = '#56CC9D'
      ) +
      geom_line(aes(x = date, y = close), colour = '#56CC9D') +
      geom_point(
        aes(
          x = calendarYear,
          y = y_,
          text = paste(
            'Date:', date,
            "\nClose:", close,
            "\nInsider Trading",
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
          ),
          size = buySellRatio
        ),
        show.legend = FALSE,
        colour = '#FF7851'
      ) +
      #scale_y_continuous(labels = scales::label_number_auto()) +
      labs(x = '', y = '') +
      theme_minimal()
  ) %>% layout(updatemenus = updatemenus)  %>% 
    style(hoverinfo = "none", traces = 1) %>% config(
      modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut')
      # displaylogo = FALSE
    ) %>% layout(
      dragmode = FALSE
    )
  #              title = list(
  #   text = "Price History",
  #   y = 0.95,  # y position (0 to 1)
  #   x = 0.01,   # x position (0 to 1)
  #   xanchor = "top",
  #   yanchor = "top",
  #   font = list(size = 25)
  # )) %>% style(hoverinfo = "none", traces = 1) %>% config(modeBarButtonsToRemove = c('zoom', 'select')) %>% layout(dragmode = FALSE)
  
  # print(paste("Plot: ", as.character(Sys.time() - start)))
  
  return(p)
})



















#### GUIDANCE ----

output$anlstEstmts <- renderPlotly({
  
  estmts <- general.APIcall(endpoint = "Analysts", symbol = isolate(input$mstrSmbl)) %>% 
    mutate(date = lubridate::ymd(date)) %>%
    rename(
      "Buy Ratings" = analystRatingsbuy,
      "Hold Ratings" = analystRatingsHold,
      "Sell Ratings" = analystRatingsSell,
      "Strong Sell Ratings" = analystRatingsStrongSell,
      "Strong Buy Ratings" = analystRatingsStrongBuy
    )
  
  to_pivot <- c("Buy Ratings",
                "Hold Ratings",
                "Sell Ratings",
                "Strong Sell Ratings",
                "Strong Buy Ratings")
  
  estmts <- tidyr::pivot_longer(estmts, cols = all_of(to_pivot), names_to = 'Guidance', values_to = 'Votes')
  
  colors_n <- length(unique(estmts$Guidance))
  custom_palette <- colorRampPalette(c('#56CC9D', '#FF7851'))
  
  p <- estmts %>%
    ggplot() +
    geom_bar(aes(x = date, y = Votes, fill = Guidance, text = paste('Date:', date, '\n', Guidance, ':', Votes)), position = 'stack', stat = 'identity') +
    scale_y_continuous(labels = scales::number_format()) +
    scale_fill_manual(values = custom_palette(colors_n)) + 
    labs(y = "Ratings", x = "", fill = "Guidance") +
    theme_minimal()
  
  return(
    ggplotly(p, tooltip = 'text') %>%
      layout(legend = list(orientation = "h", 
                           y = -0.2,
                           x = 0.5,
                           xanchor = "center")) %>%
      config(
        modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
        displaylogo = FALSE  # This removes the Plotly logo, which is often desired
      ) %>% layout(
        dragmode = FALSE)
  )
  
  
})































#### STOCK NEWS ----
output$stkNws <- renderUI({
  d_ <- general.APIcall(endpoint = "News-Stock", symbol = isolate(input$mstrSmbl)) %>% filter(site != "fool.com")
  
  # lapply(1:(min(6, nrow(d_))), function(i) {
  #   create_news_container(
  #     type = "News",
  #     publishedDate = d_$publishedDate[i],
  #     title = d_$title[i],
  #     image = d_$image[i],
  #     site = d_$site[i],
  #     text = d_$text[i],
  #     url = d_$url[i]
  #   )
  # })
  
  # start = Sys.time()
  n <- min(6, nrow(d_))
  d_subset <- d_[1:n, ]
  
  
  result <- vector("list", n)
  result <- Map(function(publishedDate, title, image, site, text, url) {
    create_news_container(
      type = "News",
      publishedDate = publishedDate,
      title = title,
      image = image,
      site = site,
      text = text,
      url = url
    )
  }, d_subset$publishedDate, d_subset$title, d_subset$image, d_subset$site, d_subset$text, d_subset$url)
  
  # # print(Sys.time() - start)
  
  return(result)
  
})


























#### STOCK PRESS ----
output$stkPrs <- renderUI({
  d_ <- general.APIcall(endpoint = "Press-Release-Stock", symbol = isolate(input$mstrSmbl)) #%>% filter(site != "fool.com")
  
  
  # lapply(1:(min(6, nrow(d_))), function(i) {
  #   create_news_container(
  #     type = "Press-Release",
  #     symbol = d_$symbol[i],
  #     date = d_$date[i],
  #     title = d_$title[i],
  #     text = d_$text[i]
  #   )
  # })
  
  
  # start = Sys.time()
  n <- min(6, nrow(d_))
  d_subset <- d_[1:n, ]
  
  
  result <- vector("list", n)
  result <- Map(function(symbol, date, title, text) {
    create_news_container(
      type = "Press-Release",
      symbol = symbol,
      date = date,
      title = title,
      text = text
    )
  }, d_subset$symbol, d_subset$date, d_subset$title, d_subset$text)
  # # print(Sys.time() - start)
  
  return(result)
})




















#### SUMMARY AVERAGES ----

output$fnnclSmmry <- renderText({
  
  start <- Sys.time()

  mkap <- fmpc_security_mrktcap(isolate(input$mstrSmbl), limit = 1)
  
  periods <- c(1, 3, 5, 7, 10)
  data <- req(stkFDta())

  
  dt_combined <- data %>%
    group_by(symbol) %>%
    mutate(row_number = row_number()) %>%
    ungroup() %>%
    crossing(period = periods) %>%
    filter(row_number <= period) %>%
    group_by(symbol, period) %>%
    summarise(across(
      c('freeCashFlow', 'operatingIncome', 'netIncome', 'bookValue', 'epsdiluted',
        'researchAndDevelopmentExpenses', 'totalDebt', 'sellingGeneralAndAdministrativeExpenses'),
      ~ mean(.x, na.rm = TRUE)
    ), .groups = "drop") %>%
    # left_join(select(mkap, symbol, marketCap), by = "symbol") %>%
    mutate(
      `P-FCF` = mkap$marketCap[1] / freeCashFlow,
      PE = mkap$marketCap[1] / netIncome,
      `P-Book` = mkap$marketCap[1] / bookValue,
      EV = mkap$marketCap[1] + totalDebt,
      `EV-FCF` = EV / freeCashFlow
    ) %>%
    rename(
      "Free Cash Flow" = freeCashFlow,
      "Operating Income" = operatingIncome,
      "Net Income" = netIncome,
      "Book Value" = bookValue,
      "EPS (Diluted)" = epsdiluted,
      "R&D Expenses" = researchAndDevelopmentExpenses,
      "SG&A Expenses" = sellingGeneralAndAdministrativeExpenses,
      "Total Debt" = totalDebt
    ) %>%
    select(-symbol) %>%
    mutate(across(where(is.numeric) & !contains('CalendarYear') & !matches("period"),
                  ~ custom_number_format(.x, decimals = 2))) %>%
    mutate(Period = paste0(period, "y Avg.")) %>%
    select(-period)
  
  
  
  # Create the table
  t <- dt_combined %>%
    select(Period, everything()) %>%
    pivot_longer(cols = -Period, names_to = "Metric", values_to = "Value") %>%
    pivot_wider(names_from = Period, values_from = Value) %>%
    add_row(Metric = "Market Cap", `1y Avg.` = custom_number_format(mkap$marketCap[1], decimals = 2)) %>%
    kableExtra::kable() %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = TRUE,
      position = "center"
    ) %>%
    kableExtra::column_spec(1, bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::add_header_above(c("Financials" = 1, "Averages" = length(periods)))
  
  
  # alternatively return data table but it's not faster
  # t <- dt_combined %>%
  #     select(Period, everything()) %>%
  #     pivot_longer(cols = -Period, names_to = "Metric", values_to = "Value") %>%
  #     pivot_wider(names_from = Period, values_from = Value)
  
  print(paste("Summary table: ", as.character(Sys.time() - start)))
  
  
  
  return(t)
  
})


























#### STOCK TRIVIA ----

output$prfl <- renderText({
  start <- Sys.time()
  t <- req(stkPrfl()) %>% select(-mktCap,
                                 -description,
                                 -image,
                                 -defaultImage,
                                 -dcf,
                                 -dcfDiff,
                                 -zip,
  ) %>%
    t(.) %>% kableExtra::kable() %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = TRUE,
      position = "center"
    ) %>%
    kableExtra::column_spec(1, bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE, color = "white") %>% # background = "#4E79A7") %>%
    kableExtra::add_header_above(c("General" = 1, " " = 1))
  
  # print(paste("trivia: ", as.character(Sys.time() - start)))
  
  return(t)
})






















#### STOCK DESCRIPTION ----

output$dscrpt <- renderUI({
  start <- Sys.time()
  t <- tagList(#tags$h2("Description"), 
    tags$p(req(stkPrfl()) %>% pull(description)))
  
  # print(paste("Desc: ", as.character(Sys.time() - start)))
  return(t)
})





























#### FUNDAMENTALS CHART ----

output$stkFndmt <- renderPlotly({
  
  d_ <- req(stkFdmntlsLng())
  
  colors_n <- length(unique(input$fndmtlsMtrics))
  custom_palette <- colorRampPalette(c('#56CC9D', '#FF7851'))
  
  p <- d_ %>%
    filter(
      between(calendarYear, input$fndmtlsSldr[1], input$fndmtlsSldr[2])
    ) %>%
    filter(Legend %in% input$fndmtlsMtrics) %>%
    {
      if(input$fndmtlsLg) {
        filter(., Value > 0)
      } else {
        .
      }
    } %>%
    ggplot() +
    geom_line(aes(
      x = fillingDate, y = Value, col = Legend
    ), linewidth = 1) +
    geom_point(
      aes(x = fillingDate, y = Value, col = Legend, 
          text = paste('FillingDate:', fillingDate, '\n', Legend, ": ", scales::comma(round(Value/1e6, 2)), "M")),
      size = 1,
      show.legend = FALSE
    ) +
    scale_x_date(date_breaks = "2 years", 
                 date_labels = "%y") +
    scale_y_continuous(
      #n.breaks = 7,
      trans = if(input$fndmtlsLg) scales::pseudo_log_trans(base = exp(1)) else "identity",
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    scale_color_manual(values = custom_palette(colors_n)) +
    labs(x = '', y = '', title = 'Fundamentals Chart') +
    theme_minimal()
  
  
  ggplotly(p, tooltip = c("text")) %>% 
    layout(legend = list(orientation = 'h')) %>% 
    config(
      modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
      displaylogo = FALSE
    ) %>% 
    layout(
      dragmode = FALSE
    ) %>% 
    style(hoverinfo = "text", traces = 1) # %>%
  # style(hoverinfo = "none", traces = 1)
  
})




























#### INDUSTRY FUNDAMENTALS ----

output$stkFndmntlsRltv <- renderPlot({
  app_dir <- shiny::getShinyOption("appDir")
  path.root <- paste0(app_dir, '/src_tables/') #file.path()
  # # print(path.root)
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
    calendarYear = lubridate::ymd(calendarYear),
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
  
  smry <- isolate(stkFdmntlsLng()) %>% filter(
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
      colour = '#56CC9D'
    ) +
    geom_point(data = smry,
               aes(x = fillingDate, y = Value),
               colour = '#56CC9D') +
    
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


























#### FUNDAMENTAL TABLES -----

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








































#### MODEL CHART ----

model_projection <- reactiveVal(NULL)

output$stkMdl <- renderPlotly({
  
  d_e <- stkMdlRctv() %>% mutate(min_ = min(close) * .95)
  # 
  # # print(input$i_trgtFrct)
  # end <- as.integer(format(input$i_trgtFrct, "%Y")) - 2024
  
  xx <- data.frame(date = lubridate::ymd(input$i_trgtFrct))
  
  # print(input$i_mdlLg)
  # print(input$i_trgtFrct)
  # print(input$i_mdlSplns)
  # print(d_e)
  # 
  p <- d_e %>%
    ggplot() +
    geom_ribbon(aes(x = date, ymin = min_, ymax = close), alpha = 0.15) +
    # geom_area(aes(x = date, y = close), alpha = 0.15) +
    geom_line(aes(x = date, y = close)) +
    geom_point(aes(x = fillingDate, y = Estimate),
               colour = '#FF7851',
               shape = 3) +
    
    geom_smooth(
      aes(x = fillingDate, y = Estimate, text = ''),
      colour = '#56CC9D',
      method = 'lm',
      formula = y ~ splines::ns(x, input$i_mdlSplns),
      se = F,
      fullrange = TRUE,
      linetype = 'dashed'
    ) +
    geom_smooth(
      aes(x = fillingDate, y = Estimate, text = ''),
      colour = '#56CC9D',
      alpha = 0.5,
      method = 'lm',
      se = F,
      fullrange = TRUE
    ) +
    geom_text(
      data = xx,
      aes(x = date, y = 500, label = ""),
      hjust = 0,
      vjust = 0.5,
      # label.padding = unit(0.5, "lines"),
      # label.size = 0.35,
      # color = "black",
      # fill = "white"
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
    coord_cartesian(ylim = c(max(d_e$mn) * 0.95, max(d_e$mx) * 1.15),
                    xlim = c(max(d_e$mn_x), input$i_trgtFrct),
                    expand = T) +
    
    labs(title = paste(
      isolate(input$mstrSmbl),
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
  # b_lm_ns_ <- data.frame(b_$data[[4]]) %>% select(x,y)
  # b_lm <- b_$data[[5]]
  # # 
  # model_projection(list(b_lm_ns_, b_lm))
  # 
  # write.csv(b_lm_ns_, 'ggplot_obj1.csv')
  # write.csv(b_lm, 'ggplot_obj2.csv')
  
  #   return(p)
  #   
  # })
  
  pp <- ggplotly(
    p
  ) %>% style(hoverinfo = "text") %>% style(hoverinfo = "none", traces = c(1, 4, 5, 6, 7)) %>% style(hovertemplate = "Estimate: %{y:.2f}", traces = c(4,5)) %>%
    config(
      modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
      displaylogo = FALSE  # This removes the Plotly logo
    ) %>% layout(
      dragmode = FALSE
    )
  
  return(pp)
})


multpl_ <- reactiveVal(NULL)

#### MODEL TABLE ----

output$stkMdlTbl <- renderDT({
  
  tbl <- stkMdlRctv() %>%
    dplyr::distinct(calendarYear, fillingDate, profit, multiple, Estimate) %>%
    arrange(desc(calendarYear)) %>% na.omit(.)
  
  multpl_(unique(tbl %>% pull(multiple))[1])
  
  datatable(
    tbl %>%
      mutate(across(
        where(is.numeric) & !contains('CalendarYear'), ~ custom_number_format(.x, decimals = 2)
      )),
    filter = "top",
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      scrollX = TRUE,
      dom = 'frtip',
      searching = FALSE
    ),
    class = 'cell-border stripe',
    selection = 'none'
  )
  
  # write.csv(stkMdlRctv(), 'test.csv')
  
  # tbl %>%
  #   kable() %>%
  #   kable_styling(
  #     bootstrap_options = c("striped", "hover", "condensed", "responsive"),
  #     full_width = TRUE
  #   ) %>%
  #   column_spec(1, bold = TRUE) %>%
  #   row_spec(0, bold = TRUE, color = "white") %>%
  #   row_spec(0:nrow(tbl), align = "c") %>%
  #   add_header_above(c("calendarYear" = 1, "fillingDate" = 1, "profit" = 1, "multiple" = 1, "estimate" = 1))
  
  # x <- read.csv('ggplot_obj1.csv')
  
  # x %>% mutate(format(as.Date(x), "%y"))
  #
  # g_tbl <- model_projection() %>% mutate(yr = format(as.Date(x), "%Y"), date = format(as.Date(x), "%Y-%m-%d")) %>% select(x, y, yr, date)
  # e_tbl <- stkMdlRctv() %>%
  #     dplyr::distinct(calendarYear, fillingDate, profit, multiple, Estimate) %>%
  #     arrange(desc(calendarYear))
  # # x %>% kable() %>% kable_styling(bootstrap_options = c("striped"))
  #
  #
  # mltpl <- stkMdlRctv() %>% distinct(multiple) %>% pull(multiple)
  # ltst_yr <- max(e_tbl %>% na.omit(.) %>% pull(calendarYear))
  
  
  
  
})





































#### VALUATION ----

output$vltn <- renderPlotly({
  
  d_fd <- req(stkFDta()) %>% filter(symbol == isolate(input$mstrSmbl)) %>%
    select(symbol, calendarYear, fillingDate, epsdiluted, fcfps, divps, bookps, netdebtps, operatingps, revenueps) %>%
    arrange(desc(fillingDate)) %>%
    mutate(across(!contains(c('date', 'symbol', 'calendarYear') ), ~moving.average(.x, span = input$i_vltnMvngAvg, order = "up"), .names = "{col}_ma" ) )
  
  
  mx_cy <- max(d_fd$calendarYear, na.rm = T)
  # # print(mx_cy)
  cy_y <- as.integer(format(Sys.Date(), "%Y"))
  yrs <- d_fd$calendarYear
  
  # # print(cy_y)
  # # print(yrs)
  # 
  # # print(!(cy_y %in% yrs))
  
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






























fv_ <- reactiveVal(NULL)
cfs_lv <- reactiveVal(NULL)
cfs_ <- reactiveVal(NULL)
cfs_pv <- reactiveVal(NULL)
cfs_tv <- reactiveVal(NULL)
#### DCF gauge ----

output$mdlGauge <- renderPlotly({
  df <- stkMdlRctv()
  latest_date <- which.max(df$date)
  price <- as.numeric(df[latest_date, "close"])
  
  inc <- stkInc()
  fcf <- stkCF()
  shares_outst <- as.numeric(inc[which.max(inc$calendarYear), 'weightedAverageShsOutDil'])
  last_fcf <- as.numeric(fcf[which.max(fcf$calendarYear), 'freeCashFlow'])
  # 
  # updateNumericInput(session, "i_strtFv", label = label_fcf(input$i_strtFv))
  
  tbl <- stkMdlRctv() %>%
    dplyr::distinct(calendarYear, fillingDate, profit, multiple, Estimate) %>%
    arrange(desc(calendarYear)) %>% na.omit(.)
  
  profit <- as.numeric(tbl[which.max(tbl$calendarYear), "profit"])
  
  rescaling_factor = value_fcf(last_fcf)
  
  # # print(rescaling_factor)
  
  cash_flows <- (input$i_strtFv * rescaling_factor) * ((1+input$i_grwthRt/100) ^ seq(1, input$i_fvHrzn))
  pv <- sum(cash_flows / (1 + input$i_dscntRt/100)^(1:input$i_fvHrzn))
  tv <- (cash_flows[length(cash_flows)] * input$i_extMltpl) / (1 + input$i_dscntRt/100)^input$i_fvHrzn
  # 
  # # print(cash_flows)
  # # print(pv)
  # # print(tv)
  
  fv <- pv + tv
  fvpshr <- fv / shares_outst
  
  # store value in reactive component
  cfs_lv(cash_flows[length(cash_flows)])
  fv_(fvpshr)
  cfs_(sum(cash_flows))
  cfs_pv(pv)
  cfs_tv(tv)
  
  
  top_range <- max(fvpshr * 1.3)
  min_range <- min(fvpshr * 0.7)
  
  steps <- seq(min_range, top_range, length.out = 6)
  # # print(steps)
  
  
  # Define colors and labels
  colors <- c('#56CC9D', '#7BE495', '#FFD166', '#FF9B71', '#FF7851')
  labels <- c('Overvalued', 'Light Green', 'Fair Value', 'Amber', 'Undervalued')
  
  # Create the gauge chart
  plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = round(fvpshr, 2),
    gauge = list(
      axis = list(range = list(min_range, top_range), tickwidth = 1, tickcolor = "transparent"),
      bar = list(color = "transparent"),
      bgcolor = "white",
      borderwidth = 2,
      bordercolor = "gray",
      steps = list(
        list(range = c(steps[1], steps[2]), color = colors[1]),
        list(range = c(steps[2], steps[3]), color = colors[2]),
        list(range = c(steps[3], steps[4]), color = colors[3]),
        list(range = c(steps[4], steps[5]), color = colors[4]),
        list(range = c(steps[5], steps[6]), color = colors[5])
      ),
      threshold = list(
        line = list(color = "red", width = 4),
        thickness = 0.75,
        value = price
      )
    )
  ) %>%
    layout(
      margin = list(l=20, r=30),
      annotations = list(
        list(
          x = 0.5 + 0.45 * cos(pi * 0.05),
          y = 0.5 + 0.45 * sin(pi * 0.1),
          text = labels[1],
          showarrow = FALSE,
          xanchor = 'center',
          yanchor = 'middle'
        ),
        list(
          x = 0.5 + 0.45 * cos(pi * 0.5),
          y = 0.1 + 0.45 * sin(pi * 0.35),
          text = labels[3],
          showarrow = TRUE,
          xanchor = 'center',
          yanchor = 'middle'
        ),
        list(
          x = 0.5 + 0.45 * cos(pi * 0.95),
          y = 0.5 + 0.45 * sin(pi * 0.9),
          text = labels[5],
          showarrow = FALSE,
          xanchor = 'center',
          yanchor = 'middle'
        )
      )
    ) %>%
    config(displayModeBar = FALSE) %>%
    layout(autosize = TRUE, margin = list(t = 40, b = 40, l = 40, r = 40))
})







































output$dcf_controls1 <- renderText({
  
  # controls helper
  
  # prefer another approach
  
  # # spline model implied CAGR
  # projection <- model_projection()[[1]] %>% mutate(x = lubridate::as_date(x))
  # # print(projection)
  # 
  # min_yr <-  lubridate::year(min(projection %>% pull(x)))
  # # print(min_yr)
  # end_projection <- as.numeric(projection[which.max(projection$x), "y"])
  # start_projection <- as.numeric(projection[which.min(projection$x), "y"])
  # # print(start_projection)
  # # print(end_projection)
  # cagr <- round(((end_projection / start_projection)^( 1 / (lubridate::year(input$i_trgtFrct) - min_yr)) - 1) * 100, 2)
  
  
  # # print('----')
  # # print(input$i_stkVltnSldr[1])
  
  period_start <- as.integer(format(as.Date(input$i_stkVltnSldr[1]), "%Y"))
  period_end <- as.integer(format(as.Date(input$i_stkVltnSldr[2]), "%Y"))
  inc_ <- stkInc() %>% mutate(calendarYear = as.integer(calendarYear), operatingIncome = as.numeric(operatingIncome)) %>% filter(between(calendarYear, period_start, period_end))
  
  
  
  # x <- fmpc_financial_bs_is_cf('TSN', statement = 'income', quarterly = FALSE, limit = 20) %>% mutate(operatingIncome = as.numeric(operatingIncome), calendarYear = as.integer(calendarYear))
  
  # Calculate min and max years
  min_year <- min(inc_$calendarYear)
  max_year <- max(inc_$calendarYear)
  
  # Fit linear model
  model <- lm(operatingIncome ~ calendarYear, data = inc_)
  
  # Predict values for start and end years
  start_pred <- predict(model, newdata = data.frame(calendarYear = min_year))
  end_pred <- predict(model, newdata = data.frame(calendarYear = max_year))
  
  # Calculate growth rate
  years_diff <- max_year - min_year
  growth_rate <- ((end_pred / start_pred) ^ (1 / years_diff) - 1) * 100
  
  # Round growth rate to 2 decimal places
  cagr <- round(growth_rate, 2)
  
  # # print(min_year)
  # # print(max_year)
  # # print(start_pred)
  # # print(end_pred)
  
  #return html
  return(paste("Earnings Trend Implied CAGR (%):", cagr))
  
  
})





output$dcf_controls2 <- renderText({
  # latest stock price
  df <- stkMdlRctv()
  latest_date <- which.max(df$date)
  price <- as.numeric(df[latest_date, "close"])
  
  #return html
  paste('Latest Stock Price:', price)
})



output$dcf_results1 <- renderText({
  
  
  cfs_lv <- cfs_lv()
  multpl_ <- multpl_()
  # cfs_ <- cfs_()
  # cfs_pv <- cfs_pv()
  # cfs_tv <- cfs_tv()
  
  statement <- paste("End Period Profit x Historical Multiple:",
                     scales::label_number(scale_cut = scales::cut_short_scale())(cfs_lv * multpl_)
                     # '\n',
                     # 'Nominal Future Cash Flows: ',
                     # round(cfs_, 2),
                     # '\n',
                     # 'NPV Future Cash Flows:',
                     # round(cfs_pv, 2),
                     # '\n',
                     # 'Terminal Value:',
                     # round(cfs_tv, 2)
  )
  
  return(statement)
  
})


scales::label_number(scale_cut = scales::cut_short_scale())(100000)

output$dcf_results2 <- renderText({
  cfs_ <- cfs_()
  
  statement <- paste("Nominal Future Profit:",
                     scales::label_number(scale_cut = scales::cut_short_scale())(cfs_) )
  return(statement)
})

output$dcf_results3 <- renderText({
  cfs_pv <- cfs_pv()
  
  statement <- paste("Net Present Value:",
                     scales::label_number(scale_cut = scales::cut_short_scale())(cfs_pv) )
  return(statement)
})

output$dcf_results4 <- renderText({
  cfs_tv <- cfs_tv()
  
  statement <- paste("Terminal Value:",
                     scales::label_number(scale_cut = scales::cut_short_scale())(cfs_tv) )
  return(statement)
})






























#### CAPITAL ALLOCATION ----

output$cptlAllctn <- renderPlotly({
  d <- isolate(stkFdmntlsLng())  %>%
    filter(between(calendarYear, input$cptlAllctnSldr[1], input$cptlAllctnSldr[2])) %>%
    filter(Legend %in% input$cptlAllctnMtrics)
  
  
  colors_n <- length(unique(d$Legend))
  custom_palette <- colorRampPalette(c('#56CC9D', '#FF7851'))
  
  ggplotly(
    d %>%
      ggplot() +
      geom_bar(
        aes(
          x = fillingDate,
          y = Value,
          fill = Legend,
          text = paste(
            'FillingDate:',
            fillingDate,
            '\n',
            "Legend:", Legend,
            "<br>Value:", scales::comma(round(Value/1e6, 2)), "M",
            "<br>Calendar Year:", as.character(calendarYear)
          )
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
      scale_fill_manual(values = custom_palette(colors_n)) +
      labs(title = "Capital Allocation Chart") +
      theme_minimal(),
    tooltip = c("x", "text")
  ) %>% 
    layout(legend = list(orientation = 'h')) %>% 
    style(hoverinfo = "text") %>% 
    config(
      modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
      displaylogo = FALSE
    ) %>% 
    layout(
      dragmode = FALSE
    )
  
})





























#### QUARTERLY CALLS ----

output$trnscrpt <- renderUI({
  
  tx_d <- trnscrpt_()
  print(nchar(tx_d))
  
  if (is.null(tx_d)) {
    return(tags$p("Transcript not available for this quarter."))
  }
  
  output <- ""
  for (line in strsplit(tx_d %>% pull(content), "\n")[[1]]) {
    if (grepl(": ", line)) {
      output <- paste(output, "<br>", "<span>  </span>", line, "<br>")
    } else {
      output <- paste(output, line)
    }
  }
  output <- trimws(output) # remove leading/trailing whitespace
  
  
  return(HTML(output))
})


# i_filingType<- reactiveVal(input$filingType)

#### SEC FILINGS ----



# on new symbol or filing type, update available dates
observe({
  filing_info <- filing_info()
  
  if (length(filing_info) != 0) {
    
    updateSelectInput(session,
                      "filingDate",
                      choices = filing_info$fillingDate,
                      selected = filing_info$fillingDate[1])
  }
  else {
    
  }
})

# request the final link from edgar
sec_content <- reactive({
  filing_info <- filing_info()
  
  # catch if not found and display message
  if (length(filing_info) != 0) {
    user_agent <- "Alex D draghicialex96@gmail.com"
    tryCatch({
      url_ <- filing_info[filing_info$fillingDate == input$filingDate, 'finalLink']
      # print(url_)
      sec_f <- httr::GET(url = url_, 
                         config = httr::user_agent(user_agent))
      httr::content(sec_f, "text")
    }, error = function(e) {
      NULL
    }
    )
    
  }
})


# if output is true, return sec filing
output$trnscrpt10k <- renderUI({
  content_ <- sec_content()
  if (!is.null(content_)) {
    HTML(sec_content())
  }
  else {
    HTML("<p> Form not available </p>")
  }
  
  
  
  
})

#### NOTES TO STATEMENTS ----
# parse the notes data

notes_content <- reactive({
  
  # print(html_content_)
  
  # FIDN SPANS BETWEEN HEADER & ITEM 9
  
  tryCatch({
    content_ <- sec_content()
    html_content_ <- rvest::read_html(content_)
    elements <- html_content_ %>%
      rvest::html_nodes("span, b")
    
    print(elements)
    
    text <- elements %>%
      rvest::html_text2()
    
    margin_ <- ifelse(input$filingType %in% c("10-k", "20-f"), 1000, 1000)
    start_pattern_ <- ifelse(input$filingType %in% c("10-k", "20-f"), "^notes to (fin|cons)", "^notes to cond")
    end_pattern_ <- ifelse(input$filingType %in% c("10-k", "20-f"), "^item 9.", "^item 2.")
    # sum(nchar(text))
    # index of the span that matches the start pattern
    start_index <- which(stringr::str_detect(
      text,
      stringr::regex(start_pattern_, ignore_case = TRUE)
    ))
    start_index <- start_index[start_index > margin_]
    print(start_index)
    # text in the financial notes
    text_after <- text[start_index[1]:length(text)]
    
    # remove whitespace and empty spans
    text_after_tidy <- text_after[stringr::str_squish(text_after) > 0]
    
    # Find the index of the span that matches the end pattern
    end_index <- which(stringr::str_detect(
      text_after_tidy,
      stringr::regex(end_pattern_, ignore_case = TRUE)
    ))
    # end_index
    
    # text between patterns
    between <- text_after_tidy[1:end_index[1]]
    
    # tidy small spans
    between_tidy <- stringr::str_squish(paste(between, collapse = " "))
    return(between_tidy)
  }, error= function(e) {
    return(NULL)
  }
  )
  
  
})



#### Notes summary btn ----
observeEvent(input$edgarButton, {
  
  # to display loading after displayed time out
  output$edgarNotes <- renderUI({
    return(div())
  })
  
  notes_to_f <- isolate(notes_content())
  
  prompt_in <- paste(
    "You are the investment intelligence unit of an investment firm. You have perfect legal, accounting and business knowledge at your disposal",
    "Your goal is to discover material information important to investors.",
    "You are given notes to the financial statements from 10-k, 10-q and 20-f filings.",
    "You must give a brief summary to each note.",
    "Then you must distill the key insights in a separate section.",
    "A key insight is material information, especially if hidden in the notes and not apparent without careful scrutiny.",
    "Format response in HTML for R Shiny renderUI (for example, for bold, use a div with appropriate style parameter), don't reflect this fact in the response. Avoid wrapping in ``` quotes. Always end with a finished sentence. ",
    "This is the filing: ",
    notes_to_f
    
    # , 1, 5000)
  )
  if (is.null(notes_to_f)) {
    output$edgarNotes <- renderUI({
      HTML("<p> Notes not found. </p>")
    })
  } else{
    generate_AI_output(input, output, id_out = 'edgarNotes', prompt_in = prompt_in, user_input = 0, id_in = NULL)
  }
})


































#### Sales Breakdown - Product ----

output$`revenue-product` <- renderPlotly({
  
  product <- as.data.frame(general.APIcall(endpoint = "revenue-product", symbol = isolate(input$mstrSmbl)))
  result <- data.frame(Date = character(),
                       Value = numeric(),
                       Segment = character(),
                       stringsAsFactors = FALSE)
  
  cols <- colnames(product)
  for (date in cols){
    # print(product[[date]] %>% na.omit(.))
    row <- product[[date]] %>% na.omit(.)
    
    if (length(row) > 0) {
      temp_df <- data.frame(
        Date = rep(date, length(row)),
        Value = as.numeric(row),
        Segment = names(row),
        stringsAsFactors = FALSE
      )
      result <- rbind(result, temp_df)
    }
    
  }
  
  result <- result %>% mutate(Date = lubridate::ymd(Date))
  
  colors_n <- length(unique(result$Segment))
  custom_palette <- colorRampPalette(c('#56CC9D', '#FF7851'))
  
  # print(result)
  
  
  p <- result %>%
    ggplot() +
    geom_bar(aes(x = Date, y = Value, fill = Segment, text = paste(Segment, ": ", scales::comma(round(Value/1e6, 2)), "M")), position = 'stack', stat = 'identity') +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_fill_manual(values = custom_palette(colors_n)) +
    labs(y = "$", x = "", fill = "Categories") +
    theme_minimal()
  
  return(
    ggplotly(p, tooltip = c("x", "text")) %>%
      layout(legend = list(orientation = "h", 
                           y = -0.2,
                           x = 0.5,
                           xanchor = "center")) %>%
      config(
        modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
        displaylogo = FALSE
      ) %>% layout(
        dragmode = FALSE)
  )
  
})

# Sales Breakdown - Geography ----

output$`revenue-geo` <- renderPlotly({
  
  product <- as.data.frame(general.APIcall(endpoint = "revenue-geo", symbol = isolate(input$mstrSmbl)))
  result <- data.frame(Date = character(),
                       Value = numeric(),
                       Segment = character(),
                       stringsAsFactors = FALSE)
  
  cols <- colnames(product)
  for (date in cols){
    # print(product[[date]] %>% na.omit(.))
    row <- product[[date]] %>% na.omit(.)
    
    if (length(row) > 0) {
      temp_df <- data.frame(
        Date = rep(date, length(row)),
        Value = as.numeric(row),
        Segment = names(row),
        stringsAsFactors = FALSE
      )
      result <- rbind(result, temp_df)
    }
    
  }
  
  result <- result %>% mutate(Date = lubridate::ymd(Date))
  
  colors_n <- length(unique(result$Segment))
  custom_palette <- colorRampPalette(c('#56CC9D', '#FF7851'))
  
  # print(result)
  
  
  p <- result %>%
    ggplot() +
    geom_bar(aes(x = Date, y = Value, fill = Segment, text = paste(Segment, ": ", scales::comma(round(Value/1e6, 2)), "M")), position = 'stack', stat = 'identity') +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_fill_manual(values = custom_palette(colors_n)) +
    labs(y = "$", x = "", fill = "Categories") +
    theme_minimal()
  
  return(
    ggplotly(p, tooltip = c("x", "text")) %>%
      layout(legend = list(orientation = "h", 
                           y = -0.2,
                           x = 0.5,
                           xanchor = "center")) %>%
      config(
        modeBarButtonsToRemove = c('zoom', 'pan', 'select', 'lasso2d', 'zoomIn', 'zoomOut'),
        displaylogo = FALSE 
      ) %>% layout(
        dragmode = FALSE)
  )
  
})


































#### gradient ----

output$stkRtChng <- renderPlotly({
  ggplotly(
    isolate(stkFdmntlsLng()) %>%
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



















#### AI summary ----
generate_AI_output <- function(input, output, id_out, prompt_in, user_input, id_in = NULL) {
  # output[[id_out]] <- renderUI({
  
  print(prompt_in)
  print(nchar(prompt_in))
  # if user question then add question to prompt chain
  if (user_input) {
    # limit question size
    if (nchar(input[[id_in]]) > 500) {
      updateTextAreaInput(session, "i_oai", value = "The question is too long.")
      return()
    }
    
    q_ <- list(list(role = "user", content = input[[id_in]]))
    prompt <- list(list(
      role = "system",
      content = prompt_in
    ))
    
    msgs <- c(prompt, q_) 
  } else {
    # use the prompt only
    msgs <- list(list(
      role = "system",
      content = prompt_in
    ))
  }
  
  tryCatch({
    
    httr::with_config(
      httr::config(connecttimeout_ms = 600000),
      completion <- openai::create_chat_completion(
        
        model = "gpt-4o-mini",
        messages = msgs,
        max_tokens = 1200,
        temperature = 0.25
      )
    )
    
    resp_ <- list(list(role = "assistant", content = completion$choices$message.content)) 
    updateTextAreaInput(session, "i_oai", value = "")
    
    msgs <- c(msgs, resp_)
    output[[id_out]] <- renderUI({
      # # print(msgs)
      return(HTML(paste(sapply(msgs[-1], function(msg) {
        paste0(toupper(msg$role), ": ", msg$content, "<br> <br>")
      }), collapse = "")))
    }) 
  }, error = function(e){
    print(e)
    output[[id_out]] <- renderUI({
      return(
        HTML("<p> Server timed out. Please try again. </p>")
      )
    }
    )
  }
  )
  
}  






#### Transcript query btn ----

observeEvent(input$oaiBtn, {
  
  output$chtHst <- renderUI({
    return(div())
  })
  
  # the prompt to answer use question
  prompt_in <- paste0(
    "You are an expert financial analyst specializing in interpreting investor transcripts and addressing the user's questions. Analyze the following excerpt from",
    stkPrfl() %>% pull(companyName),
    " Q",
    as.character(input$trnscrptQrtr),
    " ",
    as.character(input$trnscrptYr),
    " earnings call transcript, focusing on key financial metrics, management's outlook, and any significant strategic changes. Respond to the query by providing a structured response such as an executive summary, main points using bullet points, justified by key figures, and historical parallels, if relevant. Consider current market conditions and the company's position in its industry when providing your analysis.",
    " Format response in HTML for R Shiny renderUI (for example, for bold, use a div with appropriate style parameter), don't reflect this fact in the response. Avoid wrapping in ``` quotes. Always end with a finished sentence. This is the transcript: ",
    # substr(
    trnscrpt_() %>% pull(content)
    # , 1, 5000)
  )
  
  # update the UI output
  generate_AI_output(input, output, id_in = 'i_oai', id_out = 'chtHst', prompt_in = prompt_in, user_input = 1)
  
})











}


shinyApp(ui = ui, server = server)