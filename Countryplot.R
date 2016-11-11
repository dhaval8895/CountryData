con <- function(countryname){
  data <- read.csv("Countries.csv")
  name <- levels(data[,1])[data[,1]]
  country_flag <- FALSE
  for(i in 1:length(name)){
    if(countryname == name[i]){
        country_flag <- TRUE
        Population <- as.numeric(data[i,(3:58)])
    }
  }
  if(!country_flag){
     stop("Invalid Country Name") 
  }
  Year <- 1960:2015
  library(ggplot2)
  qplot(Year, Population, colour = 2, main = countryname, geom = "area", fill = 2)
}