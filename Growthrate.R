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
  years <- 1960:2014
  round <- round(rate, digits = 4)
  print(paste("The least growth rate of", name, "is",
              as.character(round[1]), "&", "the maximum growth rate of",
              name, "is", as.character((round[2]))))
  library(ggplot2)
  qplot(years, rate, geom = "jitter", color = "Red",
        main = paste("Growth rate of", name), xlab = "Year",
        ylab = "Growth Rate")
}