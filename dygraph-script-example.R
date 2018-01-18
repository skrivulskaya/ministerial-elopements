#Building dygraphs

#Packages that you'll need
packages<- c("dygraphs", "xts")

#Load the packages
lapply(packages, require, character.only=T) 

#Set working directory
setwd("/Users/suzannakrivulskaya/Box Sync/Dissertation Stuff/Dissertation/Data/ministerial-elopements/SK-shiny-testing")

#Load the geocoded data
elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)

#write table in R and export it to .csv  
denombyyear <- table(elop.raw$Year, elop.raw$Denomination_for_Tableau)
write.csv(mydf, file = "denombyyear.csv")

#Load the new file
dby.raw <- read.csv("denombyyear.csv",stringsAsFactors = F)

#Build a dygraph with four major denominations represented in the dataset 
library(dygraphs)
library(xts)

Methodist <- as.xts(ts(start = c(1870), end=c(1914),
                       data = c(dby.raw$Methodist)))

Baptist <- as.xts(ts(start = c(1870), end=c(1914),
                     data = c(dby.raw$Baptist)))

Presbyterian <- as.xts(ts(start = c(1870), end=c(1914),
                          data = c(dby.raw$Presbyterian)))

Congregational <- as.xts(ts(start = c(1870), end=c(1914),
                            data = c(dby.raw$Congregational)))

majordems <- cbind(Methodist, Baptist, Presbyterian, Congregational)

dygraph(majordems) %>% 
  dyAxis("y", label = "Number of Runaway Ministers") %>% 
  dySeries(majordems$...1, label = "Methodist") %>% 
  dySeries(majordems$...2, label = "Baptist") %>% 
  dySeries(majordems$...3, label = "Presbyterian") %>% 
  dySeries(majordems$...4, label = "Congregational") %>% 
  dyRangeSelector(height = 30)