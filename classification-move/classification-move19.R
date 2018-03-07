library(shiny)

mData <- read.csv("mec2.csv", header=TRUE)

# Get our origin and destination classifications, and the decades
origClass <- mData$Location_Origin_Size_Classification
destClass <- mData$Location_Found_Size_Classification
origPlace <- mData$Location_Origin
destPlace <- mData$Location_Found
yearLeft <- mData$Year

# Construct our data frame, making sure the string values are not factors
df <- data.frame(yearLeft, origClass, destClass, origPlace, destPlace, stringsAsFactors = FALSE)



ui <- fluidPage(
  
  titlePanel("Movement by Settlement Size"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("theYear",
                  "Number of bins:",
                  #min = 1,
                  #min = as.integer(min(df$decadeLeft)),
                  #min = tapply(df$decadeLeft, FUN = min),
                  min = 1870,
                  max = 1914,
                  value = c(1880, 1890),
                  step = 1,
                  width = 400,
                  dragRange = TRUE,
                  sep= '') 
    
      ), #Side bar panel
    
  
  
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barplot", width=600, height=600),
      htmlOutput("text_summary")

      
      
    )
    
  ) # sidebarLayout
  
  
) #UI fluidpage


# Define server logic required to draw visualization
server <- function(input, output) {
  

  
  locationChange <- reactive({
    
    #initialize our variables
    sameSize <- 0
    upSize <- 0
    downSize <- 0
    sameCity <- 0
    
    print("HEY! HEY!")
    
    # Loop through the selected years
    for(i in seq(input$theYear[1],input$theYear[2],1))
    {
      
      
      # Just a reminder:
      # df <- data.frame(yearLeft, origClass, destClass, origPlace, destPlace)
 
      
      
      
      ##########
      #SAME SIZE
      ##########
      
      # adds up the number of times the source and destination cities are the same, for the given year, i
      currentSum <- sum(
        
        # conditions of sum 
        (
          # Test the two city size designations
          ((df[2]) == (df[3]))
          & 
          # Test for the chosen year
          apply(df, 1, function(v) {
            #if the year is correct and if the origin and destination are not the same
            (v[1] == i) & (v[4] != v[5])
            
            } )
          
        ) # conditions of sum
        , na.rm=TRUE
      ) # End sum
      
      sameSize = sameSize + currentSum
      print(sameSize)
        
        
        
        
        
      ###############
      #ORIGIN SMALLER
      ###############
      
        
      # adds up the number of times source city is smaller than the destination, for the given year, i
      currentSum <- sum(
        
        # conditions of sum 
        (
          # Test the two city size designations
          ((df[2]) < (df[3]))
          & 
            # Test for the chosen year
            apply(df, 1, function(v) {
              #if the year is correct 
              (v[1] == i)
              
            } )
          
        ) # conditions of sum
        , na.rm=TRUE
      ) # End sum
      
      upSize =upSize + currentSum
      print(upSize)
        
      
      
      
      
      ##############
      #ORIGIN LARGER
      ##############
      
      # adds up the number of times source city is larger than the destination, for the given year, i
      currentSum <- sum(
        
        # conditions of sum 
        (
          # Test the two city size designations
          ((df[2]) > (df[3]))
          & 
            # Test for the chosen year
            apply(df, 1, function(v) {
              #if the year is correct 
              (v[1] == i)
              
            } )
          
        ) # conditions of sum
        , na.rm=TRUE
      ) # End sum
      
      downSize = downSize + currentSum
      print(downSize)
      
      
      

      #################
      # If the origin city and destination city are the same, add to the same city variable.
      #############
      
      currentSum <- sum(
        
        # conditions of sum 
        (
          # Test the two city size designations
          ((df[2]) == (df[3]))
          & 
            # Test for the chosen year
            apply(df, 1, function(v) {
              #if the year is correct and if the origin and destination are not the same
              (v[1] == i) & (v[4] == v[5])
              
            } )
          
        ) # conditions of sum
        , na.rm=TRUE
      ) # End sum
      
      
      sameCity = sameCity + currentSum
      
        

      
    } #for statement, looping through the years selected
    

    
    
    
    
    summedList <- c(upSize, sameSize, downSize, sameCity)
    return(summedList)
    
  }) #locationChange reactive function
  
  
  
  wentSmaller <- 1
  wentLarger <- 3
  
  output$barplot <- renderPlot({
    
    # Render a barplot

    
    barplot(
      c(locationChange()[1],locationChange()[2],locationChange()[3],locationChange()[4]),
            ylim=c(0,40),
            names.arg=c("Moved Larger", "Same Size", "Moved Smaller", "Same City")
            
    )
    
    
  })
  
  output$text_summary <- renderUI({ 
  
    #"hello\nworld\n"
    #cat("hello\nworld\n")
    
    #paste("Moved Larger: ", locationChange()[1], 
    #      "Same Size: ", locationChange()[2],
    #      "Moved Smaller: ", locationChange()[3],
    #      "Moved Smaller: ", locationChange()[4]
    #      
    #      )
    str1 <- paste("Moved Larger: ", locationChange()[1])
    str2 <- paste("Same Size: ", locationChange()[2])
    str3 <- paste("Moved Smaller: ", locationChange()[3])
    str4 <- paste("Returned to Location: ", locationChange()[4])
    
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
    
  
  })
  
} #server





# Run the application
shinyApp(ui = ui, server = server)
