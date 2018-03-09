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
write.csv(denombyyear, file = "denombyyear.csv")

#Load the new file
dby.raw <- read.csv("denombyyear.csv",stringsAsFactors = F)

#build a dygraph with four major denominations represented in the dataset 
library(dygraphs)
library(xts)
library(datasets)

#build xts objects for dygraph tab
Methodist <- as.xts(ts(start = c(1870), end=c(1914),
                       data = c(dby.raw$Methodist)))
Baptist <- as.xts(ts(start = c(1870), end=c(1914),
                     data = c(dby.raw$Baptist)))
Presbyterian <- as.xts(ts(start = c(1870), end=c(1914),
                          data = c(dby.raw$Presbyterian)))
Congregational <- as.xts(ts(start = c(1870), end=c(1914),
                            data = c(dby.raw$Congregational)))

#combine xts objects for dygraph
majordems <- cbind(Methodist, Baptist, Presbyterian, Congregational)

#this can get built in Shiny server
# dygraph(majordems) %>% 
#   dyAxis("y", label = "Number of Runaway Ministers") %>% 
#   dySeries(majordems$...1, label = "Methodist") %>% 
#   dySeries(majordems$...2, label = "Baptist") %>% 
#   dySeries(majordems$...3, label = "Presbyterian") %>% 
#   dySeries(majordems$...4, label = "Congregational") %>% 
#   dyRangeSelector(height = 30) #don't need this independent of Shiny server?

#build Shiny app
ui <- fluidPage(
  titlePanel("Number of Runaway Ministers among Four Major Denominations by Year, 1970-1914"),
  verbatimTextOutput("dygraph_description"),
  br(),
  wellPanel(dygraphOutput("dygraph"))
  )#end fluidPage

server <- function(input, output) {
  output$dygraph_description <- renderText({
    "This graph represents the annual number of elopements among the ministers in four major Protestant denominations.
The Methodists were most represented, with 90 elopers. 
The Baptists came in second at 56. 
The Presbyterians were third with 17. 
Finally, the Congregationalists came in at 11 total elopements. 
Hovering over any year with the mouse will summarize the number of eloping ministers by denomination for that year. 
Use the slider at the bottom to zoom in on a specific date range.
Please note that since exact elopements dates were not always available in the dataset, the month of January was used by default for every year represented in the graph."
  })
  output$dygraph <- renderDygraph({
    dygraph(majordems)%>% 
      dyAxis("y", label = "Number of Runaway Ministers", valueRange = c(0, 6)) %>% 
      dySeries(majordems$...1, label = "Methodist",
              drawPoints = TRUE, color = "#CC6677", pointSize = 4, strokeWidth = 2) %>% 
      dySeries(majordems$...2, label = "Baptist",
               drawPoints = TRUE, color = "#44AA99", pointSize = 4, strokeWidth = 2) %>% 
      dySeries(majordems$...3, label = "Presbyterian",
               drawPoints = TRUE, color = "#473335", pointSize = 4, strokeWidth = 2) %>% 
      dySeries(majordems$...4, label = "Congregational",
               drawPoints = TRUE, color = "#A09435", pointSize = 4, strokeWidth = 2) %>% 
      dyRangeSelector(height = 30)
  })
}
shinyApp(ui, server)
