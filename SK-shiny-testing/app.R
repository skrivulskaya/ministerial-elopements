#Clear memory
rm(list=ls(all=TRUE)) 

#List of packages that you'll need
packages<- c("dygraphs", "maptools","rgdal","leaflet","htmlwidgets","shiny","ggmap","rsconnect","ggplot2") 

#Load the packages, if they don't load you might need to install them first
lapply(packages, require, character.only=T) 

#Set working directory
setwd("/Users/suzannakrivulskaya/Box Sync/Dissertation Stuff/Dissertation/Data/ministerial-elopements/SK-shiny-testing")

#Load the geocoded data
elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)

#Build Shiny Interface
ui <- fluidPage(
  
  #Name the app 
  titlePanel("Runaway Reverends, 1870-1914", windowTitle="Runaway Reverends in the Gilded Age and Progressive Era"),
  
  #Build the layout
  sidebarLayout(
    
    #Inputs go here
    sidebarPanel(
      
      #Select dates
      sliderInput("YearInput", "Date Range:",
                  min = 1870, max = 1914,
                  value = c(1870,1914), sep = ""),
      
      #Select denomination
      checkboxGroupInput("DenominationInput", "Denomination:", 
                         choices = c("All", sort(unique(elop.raw$Denomination_for_Tableau))),
                         selected = "All", inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL),
      
      #Select state
      selectizeInput("StateInput", "State of Origin:",
                     choices = c("All", sort(unique(elop.raw$State_Origin))))
    ),
    
    #Results go here
    mainPanel(
      plotOutput("YearDenominationState"),
      br(),br(),
      tableOutput("Results")
      )
  )
)

server <- function(input, output, session) {
  
  #figuring out visualizations
  # decade_by_denomination <- elop.raw %>%
  #   group_by(Decade, Denomination_for_Tableau) %>%
  #   tally()
  # ggplot(data = decade_by_denomination, aes(x = Decade, y = n, group = Denomination_for_Tableau, color = Denomination_for_Tableau)) +
  #   geom_line()
  
  
  #moving on with Shiny visualizations
    filtered <-reactive({
      elop.raw %>%
        filter(Year == input$YearInput,
               Denomination == input$DenominationInput
        )
    })
      
   output$YearDenominationState <- renderPlot({ decade_by_denomination <- elop.raw %>%
      group_by(Decade, Denomination_for_Tableau) %>%
      tally()
    ggplot(data = decade_by_denomination, aes(x = Decade, y = n, group = Denomination_for_Tableau, color = Denomination_for_Tableau)) +
      geom_line()
  })
   
   output$Results <- renderTable({
     filtered()
   })
}

shinyApp(ui, server)
