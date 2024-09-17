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
general.APIcall <- function(endpoint, columns = NULL, symbol = NULL) {
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
           span(format(as.POSIXct(publishedDate), "%b %d, %y %H:%M"), style = "color: #666;")
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
      span(format(as.POSIXct(date), "%b %d, %y %H:%M"), style = "color: #666;")
    ),
    
    h5(title, style = "margin-top: 0; margin-bottom: 10px; color: #333;"),
    
    p(substr(text, 1, 350), style = "margin: 0; color: #555;")
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