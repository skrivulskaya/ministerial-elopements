#Clear memory
rm(list=ls(all=TRUE)) 

#List of packages that you'll need
packages<- c("dygraphs", "maptools","rgdal","leaflet","htmlwidgets","shiny","ggmap","rsconnect","ggplot2") 

#Load the packages, if they don't load you might need to install them first
lapply(packages, require, character.only=T) 
library(shiny)
library(dygraphs)
library(dplyr)
library(xts)

#Set working directory
setwd("/Users/suzannakrivulskaya/Box Sync/Dissertation Stuff/Dissertation/Data/ministerial-elopements/SK-shiny-testing")

#Load the geocoded data
elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)

#Write table in R
denombyyear <- table(elop.raw$Year, elop.raw$Denomination_for_Tableau)

#Load the new file
dby.raw <- read.csv("denombyyear.csv",stringsAsFactors = F)

#Build a dygraph
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

#Build Shiny Interface
ui <- fluidPage(
  titlePanel("Runaways by Denomination and Year"),
  sidebarLayout(
    sideparPanel(
      selectInput("Denomination", "Denomination",
                  choices = dyAxis = "Methodist", "Baptist", "Presbyterian", "Congregational"),
      mainPanel(
        dygraphOutput("majordems")
      )
    )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)

#Visualizations with dygraphs

  