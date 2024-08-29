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
moving.average <- function(x, span = 1) {
  x <- rev(x)
  new.x <- stats::filter(x, rep(1 / span, span), sides = 1)
  return(rev(new.x))
}






#### get FX rate - API call ----
fx.APIcall <- function(currency.pair) {
  res <- httr::GET(url = paste('https://financialmodelingprep.com/api/v3/quote/', currency.pair, '?apikey=975f47f0bca36cce0a7f3c6e9b5639a7', sep = ''))
  raw.content <- httr::content(res, as = 'raw')
  content <- base::rawToChar(raw.content)
  fx <- jsonlite::fromJSON(content)
  fx <- fx %>% select(symbol, price)
  return(fx)
}