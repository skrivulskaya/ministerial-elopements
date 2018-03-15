#
# Help from: https://r-dir.com/blog/2013/06/working-with-csv-files-in-r.html


library(shiny)
library(magrittr)
library(dplyr)

testvar <- "Hey there!"
mData <- read.csv("mec.csv", header=TRUE)

denoms <- mData$Denomination_for_Tableau

# dateleft <- mData[4]
# decadeLeft <- as.Date(as.character(mData$Decade), "%Y")
decadeLeft <- mData$Decade

# Get a list of the different denomTypes
#denomTypes <- levels(denoms)


df <- data.frame(decadeLeft, denoms)

#An attempt to get all denomination counts for a given decade
#decadeDenoms <- df$freq[df$decadeLeft="1870s"]

#This works with barplot
denomTypes <- table(denoms)


barplot(denomTypes)


# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Ministers by Denomination"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Decade",
                     "Number of bins:",
                     #min = 1,
                     #min = as.integer(min(df$decadeLeft)),
                     #min = tapply(df$decadeLeft, FUN = min),
                     min = 1870,
                     max = 1910,
                     value = 1880,
                     step = 10,
                     width = 400)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("barplot", width=1200, height=600)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$barplot <- renderPlot({
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
     
      realDecade <- "1870s"
      
      
      if(input$Decade == 1910){
      #  print("Positive number")
      realDecade <- "1910-1914"
      } else {
        #realDecade <- toString(input$Decade) + "s"
        #realDecade <- input$Decade + "s"
        realDecade <- paste(toString(input$Decade), "s", sep="")
        #realDecade <- paste(input$Decade, "s", sep="")
        #system('CMD /C "ECHO The R process has finished running && PAUSE"', invisible=FALSE, wait=FALSE)
        
      }
      
      
      
      #dfSummed <- df %>%
      #select(decadeLeft, denoms) %>%
      #group_by(decadeLeft) %>%
      #summarise(decader = count(decadeLeft), denomer = count(denoms))
   
      dfSummed <- data.frame(df)
      
      #https://stackoverflow.com/questions/26581410/summarizing-count-and-conditional-aggregate-functions-on-the-same-factor
      
      #dfSummed %>%
      #  group_by(decadeLeft,denoms) %>% 
      #  summarise(total.count=n(),
      #          count=sum(is.na(denoms)) 
      #          )
      
      
      #dfSummed <- df[which()]
      
      newDF <- dfSummed %>%
        filter(decadeLeft == realDecade)
     
      
      # draw the histogram 
      # barplot(table(df$denoms), col = 'darkgray', border = 'white')
      
      # draw the histogram 
      # barplot(table(df$denoms), col = 'darkgray', border = 'white')
      par(las=2) # make label text perpendicular to axis
      barplot(table(newDF$denoms), col = 'darkgray', border = 'white', ylim=c(0,30))
      #barplot(newDF$denoms, col = 'darkgray', border = 'white')
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

