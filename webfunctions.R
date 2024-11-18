#### adjusted euclidean dist. ----
adj.euclidean.dist <- function(v1, v2) {
  # collate as df
  df <- data.frame(v1 = v1, v2 = v2)
  
  # number of rows
  original.length <- dim(df)[1]
  # remove NAs
  df <- na.omit(df)
  # rows remaining
  number.remaining <- dim(df)[1]
  
  if (number.remaining == 0) {
    return (0)
  }
  
  # euclidean distance formula
  euclidean <- sqrt(sum(df$v2-df$v1)**2) 
  
  # manhattan distance formula
  #mahattan <- 
  # adjust by number of missings
  adjusted.euclidean <- euclidean * (original.length/number.remaining)**0.5
  
  return(adjusted.euclidean)
}



#### calculate moving average ----
moving.average <- function(x, span = 1, order = 'down') {
  if (order == 'down') {
  new.x <- stats::filter(x, rep(1 / span, span), sides = 1)
  return(new.x)
  }
  else if (order == 'up') {
    new.x <- stats::filter(rev(x), rep(1 / span, span), sides = 1)
    return(rev(new.x))
  }
}


#### get FX rate - API call ----
fx.APIcall <- function(currency.pair) {
  res <- httr::GET(url = paste('https://financialmodelingprep.com/api/v3/quote/', currency.pair, '?apikey=', Sys.getenv("API_FMPC"), sep = ''))
  raw.content <- httr::content(res, as = 'raw')
  content <- base::rawToChar(raw.content)
  fx <- jsonlite::fromJSON(content)
  fx <- fx %>% select(symbol, price)
  return(fx)
}


#### general API call ----
general.APIcall <- function(endpoint, columns = NULL, symbol = NULL, start = NULL, quarter = NULL, form = NULL) {
  if (endpoint == "Press-Release") {
    url = paste0('https://financialmodelingprep.com/api/v3/press-releases?page=0&apikey=', Sys.getenv("API_FMPC"))
  } else if (endpoint == "News") {
    url = paste0("https://financialmodelingprep.com/api/v4/general_news?page=0&apikey=", Sys.getenv("API_FMPC"))
  } else if (endpoint == "News-Stock") {
    url = paste0("https://financialmodelingprep.com/api/v3/stock_news?tickers=", symbol, "&page=0&apikey=", Sys.getenv("API_FMPC"))
  } else if (endpoint == "Press-Release-Stock") {
    url = paste0("https://financialmodelingprep.com/api/v3/press-releases/", symbol, "?apikey=", Sys.getenv("API_FMPC"))
  } else if (endpoint == "Econ") {
    url = paste0("https://financialmodelingprep.com/api/v4/economic?name=", symbol, "&apikey=", Sys.getenv("API_FMPC"))
  } else if (endpoint == "Insider-Trans") {
    url = paste0("https://financialmodelingprep.com/api/v4/insider-roaster-statistic?symbol=", symbol, "&apikey=", Sys.getenv("API_FMPC"))
  } else if (endpoint %in% c("Crude-Oil", "Nat-Gas", "Beef", "Corn")) {
    url = paste0("https://financialmodelingprep.com/api/v3/historical-chart/1month/", symbol, "?from=2004-02-10&to=", as.character(Sys.Date()), "&apikey=", Sys.getenv("API_FMPC"))
  } else if (endpoint == "Price") {
    end <- as.character(Sys.Date())
    start <- as.character(start)
    url = paste0('https://financialmodelingprep.com/api/v3/historical-price-full/', symbol, '?from=', start, '&to=', end, '&serietype=line', "&apikey=", Sys.getenv("API_FMPC"))
  } else if (endpoint == "Analysts") {
    url = paste0('https://financialmodelingprep.com/api/v3/analyst-stock-recommendations/', symbol, '?apikey=', Sys.getenv("API_FMPC"))
  } else if (endpoint == "10k") {
    url = paste0('https://financialmodelingprep.com/api/v4/financial-reports-json?symbol=', symbol, '&year=', as.character(start), '&period=', quarter, '&apikey=', Sys.getenv("API_FMPC"))
  } else if (endpoint == "revenue-product") {
    url = paste0('https://financialmodelingprep.com/api/v4/revenue-product-segmentation?symbol=', symbol, '&structure=flat&period=annual&apikey=' , Sys.getenv("API_FMPC"))
  } else if (endpoint == "revenue-geo") {
    url = paste0('https://financialmodelingprep.com/api/v4/revenue-geographic-segmentation?symbol=', symbol, '&structure=flat&period=annual&apikey=' , Sys.getenv("API_FMPC"))
  } else if (endpoint == "10k-sec") {
    url = paste0('https://financialmodelingprep.com/api/v3/sec_filings/', symbol, '?type=', form, '&page=0&apikey=', Sys.getenv("API_FMPC"))
    }
  
  
  res <- httr::GET(url)
  raw.content <- httr::content(res, as = 'raw')
  content_json <- base::rawToChar(raw.content)
  content_df <- jsonlite::fromJSON(content_json)
  if (is.null(columns)) {
    
    content_out <- content_df
  }
  else {
  content_out <- content_df %>% select(all_of(columns))
  }
  return(content_out)
}

# 
# res <- 
# raw.content <- httr::content(res, as = 'raw')
# content_json <- base::rawToChar(raw.content)
# content_df <- jsonlite::fromJSON(content_json)
# 
# content_df

#### news/press-release div container ----
create_news_container <- function(symbol, type = "News", date = NA, title = NA, text = NA, publishedDate = NA, image = NA, site = NA, url = NA) {
 
  
   if (type == "News") {
     
     
     div(
      
       class = "news-container",
       style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 5px; display: flex;",

       div(
         class = "news-image",
         style = "flex: 0 0 100px; margin-right: 15px;",
         img(src = image, alt = title, style = "width: 100%; height: auto; border-radius: 5px;")
         
       ),

       div(
         class = "news-content",
         style = "flex: 1;",

         div(
           class = "news-header",
           style = "display: flex; justify-content: space-between; margin-bottom: 10px;",

           strong(site, style = "font-size: 16px; color: #1a5f7a;"),
           span(format(as.POSIXct(publishedDate), "%b %d, %y"), style = "color: #666;")
         ),

         h5(title, style = "margin-top: 0; margin-bottom: 10px; color: #333;"),

         p(substr(text, 1, 250), "...", style = "margin: 0 0 10px 0; color: #555;"),

         a(href = url, "Read more", target = "_blank",
           style = "color: #1a5f7a; text-decoration: none; font-weight: bold;")
       ),
       
       
       
     )
     
     
     
  }
 
  
   else if (type == "Press-Release") {
    
    
    
  div(
    class = "news-container",
    style = "border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
    
    div(
      class = "news-header",
      style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
      
      strong(symbol, style = "font-size: 18px; color: #1a5f7a;"),
      span(format(as.POSIXct(date), "%b %d, %y"), style = "color: #666;")
    ),
    
    h5(title, style = "margin-top: 0; margin-bottom: 10px; color: #333;"),
    
    p(substr(text, 1, 550), style = "margin: 0; color: #555;")
  )
  }
}


custom_number_format <- function(x, 
                                 threshold_k = 1e3, 
                                 threshold_m = 1e6, 
                                 threshold_b = 1e9,
                                 decimals = 1) {
  
  format_number <- function(value) {
    if (is.na(value)) return(NA_character_)
    
    abs_value <- abs(value)
    sign <- ifelse(value < 0, "-", "")
    
    if (abs_value >= threshold_b) {
      return(paste0(sign, format(round(abs_value / 1e9, decimals), nsmall = decimals), "B"))
    } else if (abs_value >= threshold_m) {
      return(paste0(sign, format(round(abs_value / 1e6, decimals), nsmall = decimals), "M"))
    } else if (abs_value >= threshold_k) {
      return(paste0(sign, format(round(abs_value / 1e3, decimals), nsmall = decimals), "K"))
    } else {
      return(paste0(sign, format(round(abs_value, decimals), nsmall = decimals)))
    }
  }
  
  sapply(x, format_number)
}


# Calculate the Compounded Annual Return
cagr <- function(start, end, periods) {
  return ((end - start)^(1/periods))
}

# Discounted Cash Flow w terminal growth
dcf_valuation <- function(cash_flows, growth_rate, discount_rate, terminal_growth_rate, periods = 5) {
  
  # Calculate present value of projected cash flows
  pv_cash_flows <- sapply(1:periods, function(t) {
    cf <- cash_flows * (1 + growth_rate)^t
    cf / (1 + discount_rate)^t
  })
  
  # Calculate terminal value
  terminal_value <- (cash_flows * (1 + growth_rate)^periods * (1 + terminal_growth_rate)) / 
    (discount_rate - terminal_growth_rate)
  
  # Discount terminal value
  pv_terminal_value <- terminal_value / (1 + discount_rate)^periods
  
  # Calculate enterprise value
  enterprise_value <- sum(pv_cash_flows) + pv_terminal_value
  
  return(enterprise_value)
}


# Convert Currency

# convert.c <- function(df, fxs) {
#   
#   df <- df %>%
#     mutate(across(!contains(c('calendarYear', 'Ratio', 'ratio', 'cik', 'weightedAverageShsOutDil')) & where(is.numeric),
#                   ~ case_when(
#                     reportedCurrency == 'CNY' ~ .x * fxs[fxs['symbol'] == 'CNYUSD', 'price'],
#                     reportedCurrency == 'BRL' ~ .x * fxs[fxs['symbol'] == 'BRLUSD', 'price'],
#                     # reportedCurrency == 'SEK' ~ .x * fxs[fxs['symbol'] == 'SEKUSD', 'price'],
#                     reportedCurrency == 'EUR' ~ .x * fxs[fxs['symbol'] == 'EURUSD', 'price'],
#                     reportedCurrency == 'CAD' ~ .x * fxs[fxs['symbol'] == 'CADUSD', 'price'],
#                     # reportedCurrency == 'TRY' ~ .x * fxs[fxs['symbol'] == 'TRYUSD', 'price'],
#                     # reportedCurrency == 'MXN' ~ .x * fxs[fxs['symbol'] == 'MXNUSD', 'price'],
#                     reportedCurrency == 'TWD' ~ .x * fxs[fxs['symbol'] == 'TWDUSD', 'price'],
#                     # reportedCurrency == 'ZAR' ~ .x * fxs[fxs['symbol'] == 'ZARUSD', 'price'],
#                     reportedCurrency == 'HKD' ~ .x * fxs[fxs['symbol'] == 'HKDUSD', 'price'],
#                     reportedCurrency == 'SGD' ~ .x * fxs[fxs['symbol'] == 'SGDUSD', 'price'],
#                     # reportedCurrency == 'MYR' ~ .x * fxs[fxs['symbol'] == 'MYRUSD', 'price'],
#                     reportedCurrency == 'JPY' ~ .x * fxs[fxs['symbol'] == 'JPYUSD', 'price'],
#                     # reportedCurrency == 'INR' ~ .x * fxs[fxs['symbol'] == 'INRUSD', 'price'],
#                     # reportedCurrency == 'KRW' ~ .x * fxs[fxs['symbol'] == 'KRWUSD', 'price'],
#                     reportedCurrency == "AUD" ~ .x * fxs[fxs['symbol'] == "AUDUSD", 'price'],
#                     TRUE ~ .x
#                   )
#     ))
#   
#   return(df)
# }

# convert.c <- function(df, fxs) {
#   # Create a named vector of exchange rates
#   fx_rates <- setNames(fxs$price, fxs$symbol)
#   
#   # Create a vector of currency pairs
#   currency_pairs <- paste0(df$reportedCurrency, "USD")
#   
#   # Create a vector of multipliers
#   multipliers <- fx_rates[currency_pairs]
#   multipliers[is.na(multipliers)] <- 1  # Set multiplier to 1 for USD and unmatched currencies
#   
#   # Define columns to exclude from conversion
#   exclude_cols <- c('calendarYear', 'Ratio', 'ratio', 'cik', 'weightedAverageShsOutDil')
#   
#   # Identify numeric columns to convert
#   cols_to_convert <- sapply(df, is.numeric) & 
#     !names(df) %in% exclude_cols &
#     !grepl("Ratio|ratio", names(df))
#   
#   # Perform vectorized multiplication
#   df[, cols_to_convert] <- sweep(df[, cols_to_convert, drop = FALSE], 
#                                  1, multipliers, `*`)
#   print(df)
#   return(df)
# }

convert.c <- function(df, fxs, reportedCurrency, currency_to) {
  # Create a named vector of exchange rates
  fx_rates <- setNames(fxs$price, fxs$symbol)
  
  # Create the currency pair
  currency_pair <- paste0(reportedCurrency, currency_to)
  
  # Get the multiplier
  multiplier <- fx_rates[currency_pair]
  if (is.na(multiplier)) multiplier <- 1  # Set multiplier to 1 for USD and unmatched currencies
  
  # Define columns to exclude from conversion
  exclude_cols <- c('calendarYear', 'Ratio', 'ratio', 'cik', 'weightedAverageShsOutDil')
  
  # Identify numeric columns to convert
  cols_to_convert <- sapply(df, is.numeric) & 
    !names(df) %in% exclude_cols &
    !grepl("Ratio|ratio", names(df))
  
  # Perform vectorized multiplication
  df[, cols_to_convert] <- df[, cols_to_convert, drop = FALSE] * multiplier
  
  # print(df)
  return(df)
}



label_fcf <- function(last_fcf) {
  num_digits <- nchar(as.character(abs(last_fcf)))
  
  # print(nchar(as.character(abs(last_fcf))))
  suffix <- if (num_digits > 9) {
    "(B)"  # Billions
  } else if (num_digits > 6) {
    "(M)"  # Millions
  } else if (num_digits > 3) {
    "(T)"  # Thousands
  } else {
    ""     # Hundreds or less
  }
  
  label <- paste0('Start Value ', suffix)
  return(label)
}


value_fcf <- function(last_fcf) {
  num_digits <- nchar(as.character(abs(last_fcf)))
  
  # print(nchar(as.character(abs(last_fcf))))
  multiple <- if (num_digits > 9) {
    10^9  # Billions
  } else if (num_digits > 6) {
    10^6  # Millions
  } else if (num_digits > 3) {
    10^3  # Thousands
  } else {
    1     # Hundreds or less
  }
  
  return(multiple)
}

# user_agent <-
# sec_f <- httr::GET(url = 'https://www.sec.gov/Archives/edgar/data/320193/000032019323000106/aapl-20230930.htm', config = httr::user_agent(user_agent))
# content_ <- httr::content(sec_f, 'text')
# content_



