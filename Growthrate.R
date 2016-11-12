growthrate <- function(name){
  data <- read.csv("Countries.csv")
  connames <- levels(data[, 1])[data[, 1]]
  country_flag <- FALSE
  for( i in 1: length(connames)){
      if(name == connames[i]){
        country_flag <- TRUE
        con <- data[i, 3:58]
      }
  }
  if(!country_flag){
    stop("Please Enter Valid Country Name")
  }
  rate <- matrix(ncol = 1, nrow = 55)
  for(j in 1:55){
   rate[j, ] <- con[1, j + 1]/con[1, j]
  }
  rate <- as.numeric(rate)
  Year <- 1960:2014
  round <- round(rate, digits = 4)
  r <- cbind(round[1], round[2], round(mean(rate), digits = 4))
  rownames(r) <- name
  colnames(r) <- c("Min Growth", "Max Growth", "Mean Growth")
  print(r)
  df <- data.frame(Year, rate)
  library(ggplot2)
  ggplot(df, aes(x = Year, y = Growth$Rate, color = Country,
                 geom= "jitter")) +
    geom_point(aes(y = rate, col = name))
}