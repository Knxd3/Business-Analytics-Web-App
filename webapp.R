library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(kableExtra)
library(ggplot2)
library(bizdays)
library(plotly)
library(fmpcloudr)
library(openai)
library(lubridate)
library(Cairo)
library(bslib)

# load API keys
fmpc_set_token(Sys.getenv("API_FMPC"))
Sys.setenv(OPENAI_API_KEY = Sys.getenv("API_OAI"))

# shiny Cairo
options(
  shiny.usecairo = T,
  cairo_antialias = 'subpixel',
  dpi = 300,
  scipen = 999
)
thematic_shiny(font = "auto")

# load functions
source('webfunctions.R')

autosuggest_ <- fmpc_symbols_available() %>% filter(type == "stock" &
                                                      exchangeShortName %in% c("NYSE", "NASDAQ")) %>% pull(symbol)

#### shiny UI ----
ui <- navbarPage(
  theme = bs_theme(bootswatch = "minty"),
  title = "Value Quant Investment Platform - v1.0",
  header = fixedRow(column(
    width = 4,
    offset = 0,
    style = "margin-left: 5px;",
    selectizeInput(
      inputId = "mstrSmbl",
      label = NULL,
      choices = c('AAPL', 'MSFT', 'GOOG'),
      multiple = FALSE,
      selected = 'AAPL',
      options = list(placeholder = "Type..."),
      width = '100%'
    )
  ),
  column(width = 2, offset = 0, style = "margin-left: 5px;",
         actionButton('mstrSmblBtn', 'Submit'))
  ),


  #### tab general ----
  tabPanel(title = 'General',
           fluidRow(column(width = 6, style = "padding: 25px; padding-top: 5px;", 
                           verticalLayout(
                             tableOutput("fnnclSmmry"),
                             tableOutput("prfl")
                             )),
                    column(width = 6, style = "padding: 25px; padding-top: 5px;",
                           verticalLayout(
                             plotlyOutput('stkP'),
                             uiOutput('dscrpt')
                             )
                    ),
           )
           ),

  #### tab fundamentals ----
  tabPanel(title = 'Fundamentals',
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
             mainPanel(
               verticalLayout(
                 
                 plotlyOutput('stkFndmt'),

               )
             )
           ),
           fluidRow(column(width = 12, offset = 0, style = "align: center;",
                           plotOutput('stkFndmntlsRltv',
                                      width = "95vw",
                                      height = "700px"))),
           fluidRow(column(width = 12, offset = 0, style = "align: center;",
                           DTOutput('stkFndmntlsTbl',
                                      width = "95vw",
                                      height = "700px")))
           ),
  
  #### tab model ----
tabPanel(
  title = "Model",
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
        "Profit Metric: ",
        choices = c('epsdiluted', 'fcfps', 'operatingps', 'revenueps'),
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
      plotlyOutput('stkMdl'),
      tableOutput('stkMdlTbl')
      
      )
    )
  )
),

#### tab valuation----
tabPanel(
  title = "Valuation",
  sidebarLayout(
    sidebarPanel(
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
        value = 1,
        min = 1,
        max = 10,
        step = 1
      ),
      checkboxInput(
        'i_vltnLg',
        'Log Scale',
        FALSE
      )
      
    ),
    mainPanel(
      verticalLayout(
        column(width = 12,
               style = "display: flex; flex-direction: column; align-items: center;",
        plotlyOutput('vltn', 
                   width = "81%",
                   height = "2000px")
        )
      )
    )
  )
),

#### tab capital allocation ----
tabPanel(title = "Capital Allocation",
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
               multiple = T,
               selected = c(
                 "capitalExpenditure",
                 "dividendsPaid",
                 "netAcquisitions",
                 "netRepurchases"
               )
             ),
             sliderInput(
               'cptlAllctnSldr',
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
           ),
           mainPanel(
             verticalLayout(
               column(width = 12,
                      style = "display: flex; flex-direction: column; align-items: center;",
                      plotlyOutput('cptlAllctn', 
                                   width = "95%",
                                   height = "650px")
               
             )
           )
           
         )
         )
         ),
#### transcripts ----
tabPanel(title = "Transcripts",
         sidebarLayout(
           sidebarPanel(
             numericInput(
               "trnscrptYr",
               "Select Year: ",
               value = as.integer(format(Sys.Date(), "%Y")),
               min = 1990,
               max =as.integer(format(Sys.Date(), "%Y")),
               step = 1
             ),
             numericInput(
               "trnscrptQrtr",
               "Select Quarter: ",
               value = 1,
               min = 1,
               max = 4,
               step = 1
             )
           ),
           mainPanel(
             verticalLayout(
               uiOutput('trnscrpt'),
             )
           )
           
         )
         ),

#### tab other ----
tabPanel(title = "Other", fluidRow(
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

#### shiny server ----
server <- function(input, output, session) {
  
  current_selection <- reactiveVal(NULL)
  
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
  
  sp5 <- reactive({
    # market history
    fmpc_price_history(symbols = "^GSPC", startDate = input$i_stkMdlSldr[1])
  })
  
  ## auto-suggest
  updateSelectizeInput(
    session,
    'mstrSmbl',
    selected = 'AAPL',
    choices = autosuggest_,
    server = TRUE,
    options = list(maxOptions = 10)
  )

  observeEvent(input$mstrSmblBtn, {
    
    #### data ----
    
    current_selection(input$mstrSmbl)
    ## load data
    dt_0 <- fmpc_security_profile(input$mstrSmbl)
    dt_1 <- fmpc_price_history(symbols = input$mstrSmbl, startDate = -1, endDate = Sys.Date()) %>%
      select(symbol, date, close) %>% mutate(close = round(close , 2))
    dt_2 <- fmpc_financial_bs_is_cf(symbols = input$mstrSmbl, statement = 'income', quarterly = FALSE, limit = -1)
    dt_3 <- fmpc_financial_bs_is_cf(symbols = input$mstrSmbl, statement = 'balance', quarterly = FALSE, limit = -1)
    dt_4 <- fmpc_financial_bs_is_cf(symbols = input$mstrSmbl, statement = 'cashflow', quarterly = FALSE, limit = -1)
    f_dt <- dt_2 %>% inner_join(dt_3, by = c('symbol', 'calendarYear')) %>% inner_join(
      dt_4 %>% select(-netIncome, -inventory, -depreciationAndAmortization),
      by = c('symbol', 'calendarYear')
    ) %>% select(!contains(c('.x', '.y', 'cik', 'link', 'dDate'))) %>%
      select(symbol, calendarYear, date, fillingDate, reportedCurrency, everything()) %>%
      group_by(symbol) %>% arrange(symbol, desc(calendarYear)) %>% ungroup() %>%
      mutate(fillingDate = as.Date(fillingDate, format = "%Y-%m-%d")) %>%
      mutate(calendarYear = as.integer(calendarYear)) %>%
      mutate(netInvestments = abs(purchasesOfInvestments) - abs(salesMaturitiesOfInvestments),
             netRepurchases = abs(commonStockRepurchased) - abs(commonStockIssued),
             netDebtRepayment = abs(debtRepayment) - abs(otherFinancingActivites),
             bookValue = totalStockholdersEquity - goodwill - intangibleAssets) %>%
      mutate(across(c('acquisitionsNet', 'capitalExpenditure',
                      'commonStockRepurchased', 'dividendsPaid', 'interestExpense'), ~abs(.x))) %>%
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

        debt = case_when(
          totalAssets == 0 ~ 0,
          TRUE ~ totalLiabilities / totalAssets
        ),

        RD = case_when(
          revenue == 0 ~ 0,
          TRUE ~ researchAndDevelopmentExpenses / revenue
        ),

        SA = case_when(
          revenue == 0 ~ 0,
          TRUE ~ sellingGeneralAndAdministrativeExpenses / revenue
        ),

        operatingMargin = case_when(
          revenue == 0 ~ 0,
          TRUE ~ operatingIncome / revenue
        ),

        ROE = case_when(
          (totalStockholdersEquity - goodwill) == 0 ~ 0,
          TRUE ~ operatingIncome / (totalStockholdersEquity - goodwill)
        ),

        ROIC = case_when(
          (totalAssets - totalCurrentLiabilities - cashAndCashEquivalents) == 0 ~ 0,
          TRUE ~ operatingIncome / (totalAssets - totalCurrentLiabilities - cashAndCashEquivalents)
        ),

        CAPEX = case_when(
          revenue == 0 ~ 0,
          TRUE ~ capitalExpenditure / revenue
        ),

        netRepurchasesRevenue = case_when(
          revenue == 0 ~ 0,
          TRUE ~ (commonStockRepurchased - commonStockIssued) / revenue
        ),

        divtoOpInc = case_when(
          operatingIncome == 0 ~ 0,
          TRUE ~ dividendsPaid / operatingIncome
        ),

        netInterestExptoOpInc = case_when(
          operatingIncome == 0 ~ 0,
          TRUE ~ (interestIncome - interestExpense) / operatingIncome
        )
      )

    dt_5 <- f_dt %>%
      pivot_longer(cols = where(is.numeric) & !contains(c('symbol', 'calendarYear', 'fillingDate')),
                   names_to = 'Legend',
                   values_to = 'Value')


    stkPrfl(dt_0)
    stkPrc(dt_1)
    stkFDta(f_dt)
    stkFdmntlsLng(dt_5)
    
    
    stkMdlRctv <- reactive({
      prc <- req(stkPrc()) %>%
        filter(between(date, input$i_stkMdlSldr[1], input$i_stkMdlSldr[2])) %>% 
        mutate(calendarYear = as.integer(substr(date, 1, 4))) %>% 
        mutate(mn = min(close, na.rm = T), mx = max(close, na.rm = T))
      
      fndmt <- req(stkFDta()) %>% select(symbol, calendarYear, fillingDate, !!(input$i_mdlMtrc)) %>% rename('profit' = !!(input$i_mdlMtrc))
      
      # write.csv(prc, 'prc_test2.csv')
      # write.csv(fndmt, 'fndm2.csv')
      
      d_e <- prc  %>%
        left_join(
          fndmt,
          by = c('symbol', 'calendarYear')
        ) %>% mutate(non.neg.ratio = close / profit) %>%
        mutate(non.neg.ratio = replace(non.neg.ratio, non.neg.ratio <= 0, NA)) %>%
        mutate(multiple = ifelse(input$i_mdlMltpl == 0, psych::harmonic.mean(non.neg.ratio, na.rm = T), input$i_mdlMltpl)) %>%
        mutate(Estimate = round(profit * multiple, 2))
    })
    
    
    
    
    
    
    
    
    #### stock snapshot ----
    
    output$fnnclSmmry <- renderText({
      mkap <- fmpc_security_mrktcap(current_selection(), limit = 1)
      
      dt <- req(stkFDta()) %>% mutate(rn = row_number()) %>% filter(rn < 6) %>%
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
          ~ scales::comma(mean(.x))
        )) %>%
        left_join(mkap, by = "symbol") %>% mutate(marketCap = scales::comma(marketCap))
      
      dt %>% select(-symbol, -date) %>% t(.) %>% kable() %>%
        kable_styling(
          bootstrap_options = c("striped", "hover", "condensed", "responsive"),
          full_width = TRUE,
          position = "center"
        ) %>%
        column_spec(1, bold = TRUE) %>%
        row_spec(0, bold = TRUE, color = "white") %>%
        add_header_above(c("Financials" = 1, "5y Avg." = 1))
        
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
      tagList(
        tags$h2("Description"),
        tags$p(req(stkPrfl()) %>% pull(description))
      )
    })

    
    #### stock price ----

    output$stkP <- renderPlotly({

      updatemenus <- list(
        list(
          active = 1,
          buttons = list(
            list(
              label = 'Log Scale',
              method = 'update',
              args = list(
                list(visible = c(TRUE, TRUE)),
                list(# title = 'Log scale',
                     yaxis = list(type = 'log',
                                  # tickprefix = "$",
                                  nticks = 10,
                                  tickformat = "$,.0f",
                                  tickfont = list(size = 10),
                                  title = ""
                                  )
                     )
              )
            ),
            list(
              label = 'Linear Scale',
              method = 'update',
              args = list(
                list(visible = c(TRUE, TRUE)),
                list(# title = 'Linear scale',
                     yaxis = list(type = 'linear',
                                  # tickprefix = "$",
                                  nticks = 10,
                                  tickformat = "$,.0f",
                                  tickfont = list(size = 10),
                                  title = ""
                                  )
                     )
              )
            )
          ),
          direction = "right",
          pad = list(r = 10, t = 10),
          showactive = TRUE,
          x = 0.01,
          xanchor = "left",
          y = -0.05,
          yanchor = "top",
          type = "buttons",
          font = list(size = 10),
          buttonwidth = 80
        )
      )

      ggplotly(
        req(stkPrc()) %>% ggplot() +
          geom_area(aes(x = date, y = close), alpha = 0.15) +
          geom_line(aes(x = date, y = close)) +
          scale_y_continuous(labels = scales::dollar_format()) +
          labs(x = '', y = '') +
          theme_minimal()
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
          filter(between(calendarYear, input$fndmtlsSldr[1], input$fndmtlsSldr[2])) %>%
          filter(Legend %in% input$fndmtlsMtrics)%>%
          ggplot() +
          geom_line(aes(x = fillingDate, y = Value, col = Legend), linewidth = 1) +
          geom_point(aes(x = fillingDate, y = Value, col = Legend), size = 1, show.legend = FALSE) +
          scale_y_continuous(n.breaks = 10, trans = ifelse(input$fndmtlsLg, 'log', 'identity'), labels = scales::label_number(scale_cut = scales::cut_short_scale() )) +
          labs(x = '', y = '', title = 'Fundamentals Chart') +
          theme_minimal()
      ) %>% layout(legend = list(orientation = 'h'))

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

      cohort.summary <- read.csv(read.path)  %>% mutate(calendarYear = as.Date(calendarYear),
                                                       min = as.numeric(min),
                                                       med = as.numeric(med),
                                                       max = as.numeric(max),
                                                       Legend = metric
                                                       ) %>%
        filter(between(as.integer(format(calendarYear, "%Y")), input$fndmtlsSldr[1], input$fndmtlsSldr[2]))

      smry <- req(stkFdmntlsLng()) %>% filter(Legend %in% c('fillingDate', 'debt', 'ROIC', 'netInterestExptoOpInc', 'RD', 'SA', 'operatingMargin', 'CAPEX', 'ROE', 'netRepurchasesRevenue')) %>%
          filter(between(calendarYear, input$fndmtlsSldr[1], input$fndmtlsSldr[2])) #%>% mutate(Value = pmin(pmax(Value, 0), 1))


      # ggplotly(
          ggplot() +
          geom_line(data = cohort.summary, aes(x = calendarYear, y = med), linewidth = 1) +
          geom_ribbon(data = cohort.summary, aes(ymin = min, ymax = max , x = calendarYear, y = med), alpha = 0.2) +
          geom_line(data = smry, aes(x = fillingDate, y = Value), linewidth = 1,  colour = 'red') +
          geom_point(data = smry, aes(x = fillingDate, y = Value), colour = 'red') +

            scale_x_date(date_breaks = "2 years", date_labels = "%y") +
            scale_y_continuous(n.breaks = 7, limits = c(0, NA), oob = scales::squish, labels = scales::percent_format()  ) +
            labs(title = 'Industry Distribution', y = '', x = '') +
            facet_wrap(vars(Legend), scales = 'free_y') +
          theme_minimal() +
            theme(axis.text = element_text(face="bold", size = 10),
                  plot.title = element_text(face="bold", size = 15),
                  strip.text = element_text(size = 10))
      # )

    })
    
    
    #### fundamentals table ----
    
    output$stkFndmntlsTbl <- renderDT({
      datatable(req(stkFDta()) %>% filter(calendarYear >= 2015),
                filter = "top",
                options = list(
                  pageLength = 10,
                  autoWidth = TRUE,
                  scrollX = TRUE,
                  dom = 'Bfrtip',  # Adds buttons for copy, csv, excel, etc.
                  buttons = c('copy', 'csv', 'excel')
                ),
                extensions = 'Buttons',
                class = "cell-border stripe hover"  # Adds styling to the table
      )
    })
    
    
    #### model ----
    
    output$stkMdl <- renderPlotly({
      
      d_e <- stkMdlRctv()
      
      ggplotly(
        d_e %>%
          ggplot() +
          geom_area(aes(x = date, y = close), alpha = 0.15) +
          geom_line(aes(x = date, y = close)) +
          geom_point(aes(x = fillingDate, y = Estimate), colour = 'red', shape = 3) +
          geom_smooth(aes(x = fillingDate, y = Estimate), colour = 'red', linetype = 'dashed', method = 'lm', formula = y ~ splines::ns(x, 2), se = F, fullrange = TRUE) +
          geom_smooth(aes(x = fillingDate, y = Estimate), colour = 'red', method = 'lm', se = F, fullrange = TRUE) +
          scale_x_date(date_breaks = '1 year', date_labels = "%y", name = '') +
          scale_y_continuous(n.breaks = 11, trans = ifelse(input$i_mdlLg == TRUE, 'log', 'identity'), labels = scales::dollar_format(), name = '') +
          coord_cartesian(ylim = c(max(d_e$mn) * 0.95, max(d_e$mx)) * 1.15, expand = T) +
          labs(title = paste(current_selection(),
                             ifelse(input$i_mdlMtrc == 'epsdiluted',
                                    'Price vs. EPS x average multiple',
                                    ifelse(input$i_mdlMtrc == 'fcfps',
                                           'Price vs. FCF per share x average multiple',
                                           ifelse(input$i_mdlMtrc == 'operatingps',
                                                  'Price vs. Operating Earnings per share x average multiple',
                                                  'Price vs. Revenue per share x average multiple'))),
                             '—',
                             "Annual")) +
          theme_minimal()
      ) %>% style(hoverinfo = "none", traces = 1)
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
      
      d_fd <- req(stkFDta()) %>% filter(symbol == current_selection()) %>%
        select(symbol, calendarYear, fillingDate, epsdiluted, fcfps, divps, bookps, netdebtps, operatingps, revenueps) %>%
        arrange(desc(fillingDate)) %>%
        mutate(across(!contains(c('date', 'symbol', 'calendarYear') ), ~moving.average(.x, span = input$i_vltnMvngAvg)) )
      
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
        ) %>% mutate(PE = close/epsdiluted,
                     PFCF = close/fcfps,
                     PDIV = close/divps,
                     PBOOK = close/bookps,
                     EVFCF = (close + netdebtps) / fcfps,
                     PO = close/operatingps,
                     PR = close / revenueps
                     ) %>%
        pivot_longer(cols = c(PE, PFCF, PDIV, PBOOK, EVFCF, PO, PR),
                     names_to = 'Legend',
                     values_to = 'Values') %>%
        mutate(Values = round(Values, 2))
      # 
      # write.csv(d_fd2, 'valuation_test.csv')
      
      ggplotly(
      vltn_d %>% ggplot() +
        geom_line(aes(x = date, y = Values), colour = 'red') +
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
        labs(title = 'Valuation Chart',
             subtitle = paste("Moving Average —", as.character(input$i_vltnMvngAvg))) +
        theme_minimal() +
        theme(
          axis.text = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold", size = 15),
          strip.text = element_text(face = "bold", size = 10)
        )
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
          geom_bar(aes(x = fillingDate, y = Value, fill = Legend, text = paste("calendarYear:", as.character(calendarYear))),
                   stat = 'identity',
                   position = 'stack',
                   just = 1) +
          scale_x_date(name = '',
                       date_breaks = "1 year",
                       date_labels = "%y") +
          scale_y_continuous(name = '',
                             n.breaks = 7,
                             labels = scales::label_number(scale_cut = scales::cut_short_scale() )
                             ) +
          labs(title = "Capital Allocation Chart") +
          theme_minimal()
      ) %>% layout(legend = list(orientation = 'h')) %>% style(hoverinfo = "text")
      
    })
    
    #### transcript ----
    
    output$trnscrpt <- renderUI({
      tx_d <- fmpc_earning_call_transcript(current_selection(), quarter = input$trnscrptQrtr, year = input$trnscrptYr)
      # tagList(
      #   tags$h2("Description"),
      #   tags$p(tx_d)
      # )
      
      if (is.null(tx_d)) {
        return(tags$p("Transcript not available for this quarter"))
      }
      
      output <- ""
      for (line in strsplit(tx_d %>% pull(content), "\n")[[1]]) {
        if (grepl(": ", line)) {
          output <- paste(output, "<br>", "<h4> Paragraph: </h4>", line, "<br>")
        } else {
          output <- paste(output, line)
        }
      }
      output <- trimws(output) # remove leading/trailing whitespace
      return(HTML(output))
    })
    
    #### gradient ----
    
    output$stkRtChng <- renderPlotly({
      ggplotly(
        req(stkFdmntlsLng()) %>%
          filter(between(calendarYear, input$fndmtlsSldr[1], input$fndmtlsSldr[2])) %>%
          filter(Legend %in% input$fndmtlsMtrics)%>%
          group_by(Legend) %>% mutate(Value = Value - dplyr::lag(Value, n = 1, order_by = fillingDate)) %>%
          ggplot() +
          geom_line(aes(x = fillingDate, y = Value, col = Legend), linewidth = 1) +
          geom_point(aes(x = fillingDate, y = Value, col = Legend), size = 1, show.legend = FALSE) +
          scale_y_continuous(n.breaks = 10, trans = ifelse(input$fndmtlsLg, 'log', 'identity'), labels = scales::label_number(scale_cut = scales::cut_short_scale() )) +
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
      
      ggplotly(pr_2 %>% ggplot() + geom_line(aes(
        x = date, y = returnVsMarket, col = symbol
      )) + geom_hline(
        yintercept = 1,
        col = 'red',
        alpha = 0.5,
        linetype = 'dashed'
      ) + labs(title = "Performance vs. SP500") + theme_minimal()
      ) %>% layout(showlegend = FALSE)
    })
    
  })

}

shinyApp(ui, server)
