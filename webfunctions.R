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






# 
# library(tidyr)
# library(dplyr)
# 
# # Example data
# df <- tibble(
#   id = 1:3,
#   x1 = c(1, 2, 3),
#   x2 = c(4, 5, 6),
#   y1 = c(99, 100, 101),
#   y2 = c(102, 103, 104)
# )
# 
# df
# 
# # df %>% pivot_longer(cols = c( c("x1", "x2"), c("y1", "y2")), names_to = c('l1', 'l2'), names_pattern = '(.)(.)')
# 
# df_2 <- tibble(
#   id = 1:6,
#   l1 = c('x1', 'x1', 'x1', 'x2', 'x2', 'x2'),
#   l2 = c('y1', 'y1', 'y1', 'y2', 'y2', 'y2'),
#   v1 = c(1, 2, 3, 4, 5, 6),
#   v2 = c('a', 'b', 'c', 'd', 'e', 'f')
# )
# 
# 
# df_2
# 
# 
# df %>% pivot_longer(cols = c("x1", "x2", "y1", "y2"), names_to = 'l', values_to = 'v') %>% arrange(l) %>% mutate(i = substr(l, 1, 1)) %>% group_by(id) %>% pivot_wider(names_from = c(l), values_from = v)
