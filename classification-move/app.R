library(shiny)

mData <- read.csv("mec2.csv", header=TRUE)

# Get our origin and destination classifications, and the decades
origClass <- mData$Location_Origin_Size_Classification
destClass <- mData$Location_Found_Size_Classification
decadeLeft <- mData$Decade

df <- data.frame(decadeLeft, origClass, destClass)


#Temporary playaround variable
theDecade <- "1870s"


#Get all unique decades:
uniqueDecades <- unique(unlist(df[[1]], use.names = FALSE))


# Makes a vector for decade counts and assigns to it the appropriate decade labels for analysis later
decadeCount <- vector(mode="integer", length=length(uniqueDecades))
names(decadeCount) <- (uniqueDecades)



  

ui <- fluidPage(
  
  titlePanel("Movement by Settlement Size"),
  
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
                  value = c(1880, 1890),
                  step = 10,
                  width = 400,
                  dragRange = TRUE,
                  sep="") #,
      
      # Selector for type of move -- not needed, I think
      #selectInput("select", 
      #            h3("Location Change"),
      #            choices = list(
      #              "Moved to Larger" = 1, 
      #              "Moved to Smaller" = 2,
      #              "Stayed the same" = 3), 
      #            selected = 1
      #)
    
      
      
      ), #Side bar panel
    
  
  
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barplot", width=600, height=600)
      #textOutput("selected_var")
      
      
      
    )
    
  ) # sidebarLayout
  
  
) #UI fluidpage


# Define server logic required to draw visualization
server <- function(input, output) {
  
 
  
  stayedSame <- reactive({
    
    summedUp <- 0
    
    for(i in seq(input$Decade[1],input$Decade[2],10))
    {
      
      
      # convert decade to proper string
      if (i == 1910){
        
        iFixed = "1910-1914"
        
      }else{
        
        iFixed = paste(i,"s", sep="")
        
      }
      
      
      # Returns a "1" to currentSum if the source and destination cities are the same, and the decade is correct
      currentSum <- sum(
        
        # conditions of sum 
        (
          # Test the two city size designations
          ((df[2]) == (df[3]))
          & 
          # Test for the chosen decade
          apply(df, 1, function(v) {
            v[1] == iFixed
            })
        ) # conditions of sum
        , na.rm=TRUE
      ) # End sum
    
      summedUp = summedUp + currentSum
    
      
        
      
    } #for statement
    
    print(summedUp)
    
  }) #stayedSame reactive function
  
  
  
  
  
  
  
  wentLarger <- reactive({
    
    summedUp <- 0
    
    for(i in seq(input$Decade[1],input$Decade[2],10))
    {
      
      
      # convert decade to proper string
      if (i == 1910){
        
        iFixed = "1910-1914"
        
      }else{
        
        iFixed = paste(i,"s", sep="")
        
      }
      
      
      # Returns a "1" to currentSum if the source and destination cities are the same, and the decade is correct
      currentSum <- sum(
        
        # conditions of sum 
        (
          # Test the two city size designations
          ((df[2]) < (df[3]))
          & 
            # Test for the chosen decade
            apply(df, 1, function(v) {
              v[1] == iFixed
            })
        ) # conditions of sum
        , na.rm=TRUE
      ) # End sum
      
      summedUp = summedUp + currentSum
      
      
      
      
    } #for statement
    
    print(summedUp)
    
  }) #wentLarger reactive function
  
  
  
  
  
  
  
  
  
  wentSmaller <- reactive({
    
    summedUp <- 0
    
    for(i in seq(input$Decade[1],input$Decade[2],10))
    {
      
      
      # convert decade to proper string
      if (i == 1910){
        
        iFixed = "1910-1914"
        
      }else{
        
        iFixed = paste(i,"s", sep="")
        
      }
      
      
      # Returns a "1" to currentSum if the source and destination cities are the same, and the decade is correct
      currentSum <- sum(
        
        # conditions of sum 
        (
          # Test the two city size designations
          ((df[2]) > (df[3]))
          & 
            # Test for the chosen decade
            apply(df, 1, function(v) {
              v[1] == iFixed
            })
        ) # conditions of sum
        , na.rm=TRUE
      ) # End sum
      
      summedUp = summedUp + currentSum
      
      
      
      
    } #for statement
    
    print(summedUp)
    
  }) #wentSmaller reactive function
  
  
  
  
  
  
  #output$selected_var <- renderText({ 
    
  #  paste("Stayed Same: ", stayedSame(), "  Went Larger: ", wentLarger(),
  #        "  Went Smaller: ", wentSmaller()
          
  #        )
    
  #})
  
  output$barplot <- renderPlot({
    
    # Render a barplot

    
    barplot(c(wentSmaller(),stayedSame(),wentLarger()),
            ylim=c(0,40),
            names.arg=c("Smaller", "Same", "Larger")
            
            
            )
    
    
  })
  
} #server





# Run the application
shinyApp(ui = ui, server = server)
