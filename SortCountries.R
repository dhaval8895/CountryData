year <- function(y){
  data <- read.csv("Countries.csv")
  yr <- factor(1960:2015)
  year_flag <- FALSE
  for(i in 1: length(yr)){ 
      if(y == yr[i]){
          year_flag <- TRUE
      } 
  }
  if(!year_flag){
    stop("Please Enter Valid Year from 1960-2015")
  }
  a <- data.frame(data[, 1], data[, y-1957])
  ordercon <- a[order(a[, 2], decreasing = TRUE), ]
  d <- head(data.frame(ordercon[, 1], ordercon[, 2]))
  colnames(d) <- c("Countries sorted according to highest population"
                   , "Population")
  h <- head(d)
  h
  ##library(ggplot2)
  ##qplot(h[, 1], h[, 2], xlab = "Countries sorted according to highest population"
  ##        , ylab = "Population", geom = "jitter")
  ##plot(h)
}