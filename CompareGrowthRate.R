cmpgrowthrate <- function(name, name1){
  data <- read.csv("Countries.csv")
  connames <- levels(data[, 1])[data[, 1]]
  connames1 <- levels(data[, 1])[data[, 1]]
  country_flag <- FALSE
  country_flag1 <- FALSE
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
  na <- rbind.data.frame(name, rate)
  rate <- as.numeric(rate)
##  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ ##
  round <- round(rate, digits = 4)
  for( i in 1: length(connames1)){
    if(name1 == connames1[i]){
      country_flag1 <- TRUE
      con1 <- data[i, 3:58]
    }
  }
  if(!country_flag1){
    stop("Please Enter Valid Country Name")
  }
  rate1 <- matrix(ncol = 1, nrow = 55)
  for(l in 1:55){
    rate1[l, ] <- con1[1, l + 1]/con1[1, l]
  }
  rate1 <- as.numeric(rate1)
  Year <- 1960:2014
  round1 <- round(rate1, digits = 4)
  summaryname <- cbind(round[1],round[2],round(mean(rate), digits = 4))
  summaryname1 <- cbind(round1[1],round1[2],round(mean(rate1), digits = 4))
  colnames(summaryname) <- c("Min Growth","Max Growth",
                             "Avg Growth")
  colnames(summaryname1) <- c("Min Growth","Max Growth",
                               "Avg Growth")
  rownames(summaryname) <- name
  rownames(summaryname1) <- name1
  r <- rbind(summaryname,summaryname1)
  print(r)
  library(ggplot2)
  df <- data.frame(Year, rate, rate1)
  ggplot(df, aes(x = Year, y = Growth$Rate, color = Country,
                 geowm = "jitter")) + 
    geom_point(aes(y = rate, col = name)) + 
    geom_point(aes(y = rate1, col = name1))
}