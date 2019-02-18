rm(list=ls(all=TRUE)) #clear memory

#load required packages
library(rgdal)
library(leaflet)
library(shiny)
library(geosphere)
library(dygraphs)
library(dplyr)
library(ggplot2)
library(xts)
library(plotly)
library(DT)
library(crosstalk)
library(datasets)
library(shinydashboard)
library(shinyWidgets)

# setwd("/Users/suzannakrivulskaya/Box Sync/Dissertation Stuff/Dissertation/Data and Research/ministerial-elopements")
# setwd("E:\\GIT_Checkouts\\R_Scripts\\ministerial-elopements")
# setwd("/home/matthew/GIT/R_Scripts/ministerial-elopements")

latlong <- "+init=epsg:4326"

#load geocoded data
elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)
elop.raw$Longtitude_Found <- ifelse(elop.raw$Longtitude_Found > 100, -179.99, elop.raw$Longtitude_Found)

#build settlement size data frame
mData <- read.csv("ministerial_elopements_geocoded.csv", header=TRUE)

#get our origin and destination classifications, and the decades
origClass <- mData$Location_Origin_Size_Classification
destClass <- mData$Location_Found_Size_Classification
origPlace <- mData$Location_Origin
destPlace <- mData$Location_Found
yearLeft <- mData$Year

#construct our data frame, making sure the string values are not factors
df <- data.frame(yearLeft, origClass, destClass, origPlace, destPlace, stringsAsFactors = FALSE)

#build the raw data table
raw.data.tab <- read.csv("raw_data_tab.csv",stringsAsFactors = F)
rdt <- raw.data.tab %>%
  tibble::rownames_to_column()

#build the dygraph tables
dby.raw <- as.data.frame.matrix(table(elop.raw$Year, elop.raw$Denomination_for_Tableau))

#build xts objects for dygraph tab
Methodist <- as.xts(ts(start = c(1870), end=c(1914), data = c(dby.raw$Methodist)))
Baptist <- as.xts(ts(start = c(1870), end=c(1914), data = c(dby.raw$Baptist)))
Presbyterian <- as.xts(ts(start = c(1870), end=c(1914), data = c(dby.raw$Presbyterian)))
Congregational <- as.xts(ts(start = c(1870), end=c(1914), data = c(dby.raw$Congregational)))

#combine xts objects for dygraph
majordems <- cbind(Methodist, Baptist, Presbyterian, Congregational)

#generate new locations for duplicate places 
elop.raw$dup_origin <- elop.raw$Location_Origin %in% elop.raw[duplicated(elop.raw$Location_Origin),]$Location_Origin
elop.raw$dup_found <- elop.raw$Location_Found %in% elop.raw[duplicated(elop.raw$Location_Found),]$Location_Found
elop.raw$Latitude_Origin <- ifelse(elop.raw$dup_origin, elop.raw$Latitude_Origin - (runif(nrow(elop.raw))-.5)/20,elop.raw$Latitude_Origin)
elop.raw$Longtitude_Origin <- ifelse(elop.raw$dup_origin, elop.raw$Longtitude_Origin - (runif(nrow(elop.raw))-.5)/20,elop.raw$Longtitude_Origin)
elop.raw$Latitude_Found <- ifelse(elop.raw$dup_found, elop.raw$Latitude_Found - (runif(nrow(elop.raw))-.5)/20,elop.raw$Latitude_Found)
elop.raw$Longtitude_Found <- ifelse(elop.raw$dup_found, elop.raw$Longtitude_Found - (runif(nrow(elop.raw))-.5)/20,elop.raw$Longtitude_Found)

#generate html popup
elop.raw$popupw <- paste(sep = "",  "<b>",elop.raw$Full_Name,"</b><br/>",
                         "Denomination: ",elop.raw$Denomination_for_Tableau, "<br/>",
                         "Age: ",elop.raw$Age, "<br/>",
                         "Year Eloped: ",elop.raw$Year,"<br/>",
                         "Origin: ",elop.raw$Location_Origin,"<br/>",
                         "Accusation: ",elop.raw$Accusations,"<br/>",
                         "Age of Female: ",elop.raw$Female_Age, "<br/>",
                         "Found: ",elop.raw$Location_Found,"<br/>",
                         "Year Found: ",elop.raw$Year_Found,"<br/>")

#generate directional information
elop.raw$bearing[(!is.na(elop.raw$Latitude_Found))& (elop.raw$Location_Origin != elop.raw$Location_Found)] <- bearingRhumb(elop.raw[((!is.na(elop.raw$Latitude_Found))& (elop.raw$Location_Origin != elop.raw$Location_Found)),c("Longtitude_Origin","Latitude_Origin")],elop.raw[((!is.na(elop.raw$Latitude_Found))& (elop.raw$Location_Origin != elop.raw$Location_Found)),c("Longtitude_Found","Latitude_Found")])
elop.raw$bearClass[(elop.raw$bearing < 45 ) | (elop.raw$bearing >= 315)] <- "North"
elop.raw$bearClass[(elop.raw$bearing < 135) & (elop.raw$bearing >= 45)] <- "East"
elop.raw$bearClass[(elop.raw$bearing < 225) & (elop.raw$bearing >= 135)] <- "South"
elop.raw$bearClass[(elop.raw$bearing < 315) & (elop.raw$bearing >= 225)] <- "West"
elop.raw[which(elop.raw$Location_Origin == elop.raw$Location_Found),]$bearClass <- 'Same'
elop.raw[is.na(elop.raw$Location_Found),"bearClass"] <- 'Never'
elop.raw$bearClass <- factor(elop.raw$bearClass, levels=c("North","South", "East", "West","Same","Never"))
bearClass.rounder <- 10*ceiling(max(table(elop.raw$bearClass))/10) #this keeps the plotly chart standardized

#create lines
elop.comp <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
row.names(elop.comp) <- NULL
a <- (gcIntermediate(elop.comp[,c("Longtitude_Origin","Latitude_Origin")], elop.comp[,c("Longtitude_Found","Latitude_Found")],addStartEnd = T, breakAtDateLine = T, n=150, sp = T) )
complete.lines <- SpatialLinesDataFrame(a,elop.comp)
#Fixing a dateline issue for that one that ran to new zealand: TODO: Still not perfect
# negs <- as.matrix(coordinates(complete.lines[94,])[[1]][[2]])
# negs[,1] <- (negs[,1])-360
# complete.lines@lines[[94]]@Lines[[2]]@coords[] <- negs

#make directional arrows for the lines
markers.df <- elop.comp[which(elop.comp$Location_Origin != elop.comp$Location_Found),]
markers.df$midlong <- apply(markers.df[,c("Longtitude_Origin","Longtitude_Found")], 1, mean) 
markers.df$midlat <- apply(markers.df[,c("Latitude_Origin","Latitude_Found")], 1, mean) 
a <- (gcIntermediate(markers.df[,c("Longtitude_Origin","Latitude_Origin")], markers.df[,c("Longtitude_Found","Latitude_Found")], n=1) )
a <- do.call(rbind.data.frame, a)
markers.df$midlong <- a$lon
markers.df$midlat <- a$lat
# markers.df$bearing <- bearingRhumb(markers.df[,c("Longtitude_Origin","Latitude_Origin")],markers.df[,c("Longtitude_Found","Latitude_Found")])
#calculate the direction of travel
arrow.scale <- 4
arrow.angle <- 30

build.arrowheads <-function(arrow.scale = 4, df = markers.df){
  lengs <- c(100000,100000,100000,100000,60000,50000,30000,18000,10000,5000,3000,2000,1000)
  arrow.length <- lengs[arrow.scale]
  # print(paste(arrow.scale, arrow.length))
  a <- data.frame(destPoint(df[,c( "midlong","midlat")], d = arrow.length, b = df$bearing + arrow.angle -180 ))
  df$arrow1Lat <- a$lat
  df$arrow1Lon <- a$lon
  
  a <- data.frame(destPoint(df[,c( "midlong","midlat")], d = arrow.length, b = df$bearing - arrow.angle-180 ))
  df$arrow2Lat <- a$lat
  df$arrow2Lon <- a$lon
  
  row.names(df) <- NULL
  polys <- list()
  for (i in 1:nrow(df)) { 
    polys[[i]] = Polygons(list(Polygon(rbind(c(df$midlong[i],df$midlat[i]), c(df$arrow2Lon[i], df$arrow2Lat[i]), c(df$arrow1Lon[i], df$arrow1Lat[i]),c(df$midlong[i],df$midlat[i])))), as.character(i))
  }
  arrowheads <- SpatialPolygonsDataFrame(SpatialPolygons(polys),df)
  proj4string(arrowheads) <- CRS(latlong)
  return(arrowheads)
}

poly.arrrows <- build.arrowheads()

#mapping section: convert to point data frames for mapping, randomize identical points
orig.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Origin)),]
a<- data.frame(table(orig.spdf$Location_Origin))
a$Location_Origin <- as.character(a$Var1)
a$Var1 <- NULL
orig.spdf <- merge(orig.spdf,  a, by="Location_Origin",all=T)
remove(a)
orig.spdf$Latitude_Origin <-ifelse((orig.spdf$Freq > 1),  orig.spdf$Latitude_Origin - (runif(nrow(orig.spdf))-.5)/40,orig.spdf$Latitude_Origin)
orig.spdf$Longtitude_Origin <-ifelse((orig.spdf$Freq > 1), orig.spdf$Longtitude_Origin - (runif(nrow(orig.spdf))-.5)/40,orig.spdf$Longtitude_Origin)

coordinates(orig.spdf)=~Longtitude_Origin+Latitude_Origin
proj4string(orig.spdf) <- CRS(latlong)

found.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
elop.raw$Longtitude_Found <- ifelse(elop.raw$Longtitude_Found > 100, elop.raw$Longtitude_Found - 360, elop.raw$Longtitude_Found)

coordinates(found.spdf)=~Longtitude_Found+Latitude_Found
proj4string(found.spdf) <- CRS(latlong)

same.spdf <- elop.comp[which(elop.comp$Location_Origin == elop.comp$Location_Found),]
coordinates(same.spdf)=~Longtitude_Found+Latitude_Found
proj4string(same.spdf) <- CRS(latlong)


#build Shiny interface
ui <- fluidPage(
  title = "Runaway Reverends",
  navbarPage("Runaway Reverends",
             tabPanel("Home",
    fluidRow(
    column(3,
           (wellPanel(
             sliderInput("range", "Date Range:",
                         min = 1870, max = 1914,
                         value = c(1870,1914), sep = ""),
             selectizeInput("denomination", "Denomination(s):",
                            choices = c(sort(unique(elop.raw$Denomination_for_Tableau))),
                            multiple = TRUE,
                            options = list(plugins= list('remove_button'), placeholder = 'All Denominations')
                            ),
             # actionButton("reset_input", "Reset Denomination(s)"),
             hr(),
             materialSwitch(inputId = "CompCheck", value = FALSE, right = TRUE, status = "primary", 
                            label = "Limit to ministers who were found, arrested, or returned home"),
             checkboxGroupInput("direction", label = ("Direction:"), 
                                choices = c("Same","North", "East", "West", "South"),
                                selected = c("Same", "North", "East", "West", "South"))))),
    column(9,(wellPanel(leafletOutput("mymap"))))),
    #end first ui row with selection panel and leaflet map
    hr(),
    fluidRow(
    column(2, h5("Current Filter: ")),
    column(10, verbatimTextOutput("textHeader"),
      tags$head(tags$style("#textHeader{color:#367CBB; font-size:12px; font-style:regular;
      overflow-y:scroll; max-height: 100px; background: ghostwhite;}"))),
    column(6, h5("Direction of Movement: ")),
    column(6, h5("Number of Ministers by Denomination: ")),
    column(6, plotlyOutput("directionPlotly")),
    column(6, plotlyOutput("denominationPieChart")),
    column(12, wellPanel(htmlOutput("home_about")))
     )),
    
    tabPanel("Settlement Size",
             sidebarLayout(
               column(3, offset = 0, style='padding-right:10px;',
                 wellPanel(sliderInput("theYear","Date Range:", min = 1870, max = 1914, value = c(1870, 1914),
                             step = 1, width = 400, dragRange = TRUE, sep= ''))),
               column(9, wellPanel(
                 plotOutput("barplot"), br(), htmlOutput("text_summary")))),
             wellPanel(htmlOutput("settlement_about"))
             ),
    tabPanel("Four Major Denominations",
             fluidRow(
               wellPanel(dygraphOutput("dygraph"),
                         br(),
                         htmlOutput("dygraph_description")))), 
    tabPanel("Raw Data",
             fluidRow(
               wellPanel(DT::dataTableOutput("x1"),
                         fluidRow(p(class = 'text-center'))))
             )))


server <- function(input, output, session) {
  
  points.orig <- eventReactive(c(input$range, input$denomination, input$CompCheck, input$direction), {
 
    working.spdf <- orig.spdf
    if(input$CompCheck){
      working.spdf <- orig.spdf[which(!is.na(orig.spdf$Latitude_Found)),]
    }
    working.spdf <- working.spdf[which(working.spdf$bearClass %in% c(input$direction, "Never")),]
    
    if (is.null(input$denomination)){
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2]),])
    }else{
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2] & working.spdf$Denomination_for_Tableau %in% input$denomination),])  
    }
    
  }, ignoreNULL = FALSE)#end points.orig
  
  points.found <- eventReactive(c(input$range, input$denomination, input$direction), {
    working.found <- found.spdf[which(found.spdf$bearClass %in% input$direction),]

    if (is.null(input$denomination)){
      working.found <- working.found[which(working.found$Year >= input$range[1] & working.found$Year <= input$range[2]),]
    }else{
      working.found <- working.found[which(working.found$Year >= input$range[1] & working.found$Year <= input$range[2] & working.found$Denomination_for_Tableau %in% input$denomination),]  
    }
    return(working.found)
    
  }, ignoreNULL = FALSE)#end points.found
  
  points.connections <- eventReactive(c(input$range, input$denomination, input$direction), {
    #Fix this so it works like the others: with the temp.variable: currently not reactive on dates.
    if (!"Same" %in% input$direction){
      return(same.spdf [which(same.spdf$bearClass == "Jeff"),])
    }
    if (is.null(input$denomination)){
      temp.same <- same.spdf
    }else{
      temp.same <- same.spdf[which(same.spdf$Denomination_for_Tableau %in% input$denomination),]
    }
    #filter years
    temp.same <- temp.same[which(temp.same$Year >= input$range[1] & temp.same$Year <= input$range[2]),]
   
    return(temp.same)
    
  }, ignoreNULL = FALSE)#end points.found
  
  lines.connections <- eventReactive(c(input$range, input$denomination, input$direction), {
    if (is.null(input$denomination)){
      temp.lines <- complete.lines
    }else{
      temp.lines <- complete.lines[which(complete.lines$Denomination_for_Tableau %in% input$denomination),]
          }
    #filter years
    temp.lines <- temp.lines[which(temp.lines$Year >= input$range[1] & temp.lines$Year <= input$range[2]),]
    #filter bearing
    temp.lines <- temp.lines[which(temp.lines$bearClass %in% input$direction),]
    # return(complete.lines[
    return(temp.lines)
    
  }, ignoreNULL = FALSE)#end lines.connections
  
  arrowheads.connections <- eventReactive(c(input$range, input$denomination, input$direction,input$mymap_zoom), {
    if(is.null((input$mymap_zoom))){
      poly.arrows <- build.arrowheads(arrow.scale = 4)
    }else{
      poly.arrows <- build.arrowheads(arrow.scale = input$mymap_zoom)
    }
    
    if (is.null(input$denomination)){
      temp.arrows <- poly.arrows
    }else{
      temp.arrows <- poly.arrows[which(poly.arrows$Denomination_for_Tableau %in% input$denomination),]
    }
    #filter years
    temp.arrows <- temp.arrows[which(temp.arrows$Year >= input$range[1] & temp.arrows$Year <= input$range[2]),]
    #filter bearing
    temp.arrows <- temp.arrows[which(temp.arrows$bearClass %in% input$direction),]
    #return(complete.lines[
    return(temp.arrows)
    
  }, ignoreNULL = FALSE)#end lines.connections
  
  text.filter <- eventReactive(c(input$range, input$denomination, input$CompCheck, input$direction),{
    print(input$denomination)
    out.string <- ""
    if (input$range[1] == 1870 & input$range[2]  == 1914){
      out.string <- paste(out.string, "All years, ", sep = "")
    } else{
      out.string <- paste(out.string, input$range[1], " - ", input$range[2], ", ", sep="")
    } 
    if (is.null(input$denomination)){
      out.string <- paste(out.string, "All demoninations, ", sep = "")
    }else{
      out.string <- paste(out.string, "Demoninations =  ",paste(input$denomination,collapse = ", "), ", ", sep="")
    }
    if (length(input$direction) == 5){
    out.string <- paste(out.string, "All directions ", sep = "")
      
    }else{
      dir.string <- paste(unlist(input$direction), collapse =", ")
      out.string <- paste(out.string, "Directions = ",dir.string, sep="")
    }
    if (length(input$direction) == 0){
      out.string <- "Nothing selected"
    }
    
    return(out.string)
  })
  
  output$value <- renderPrint({ input$direction })
  
  output$home_about <- renderUI({
    "The Runaway Reverends project is an interactive digital map of American Protestant pastors who eloped with women who were not their wives between the years 1870 and 1914.
    Red dots represent places of origin. Blue dots are destinations. Lime green dots indicate ministers who returned home (or were arrested in the city of origin).
    Clicking on each dot will provide additional information about the minister.
    You may also filter by year, denomination, and direction of movement in the left-hand column. Statistical data about the cohort in the lower half of the site will refresh each time you apply a new filter.
    The project was created by Suzanna Krivulskaya in collaboration with Matthew Sisk and Daniel Johnson of the Navari Family Center for Digital Scholarship at the University of Notre Dame."
  })
  
  #create an output table with directions
  # output$directionTable <- renderTable({
  #   direction.table.maker <- table(points.orig()@data[,c("bearClass")])
  #   direction.table.maker <- c(direction.table.maker[1:5],"All Found" = sum(direction.table.maker[1:5]),direction.table.maker["Never"])
  #   direction.table.maker <- t(direction.table.maker)
  #   colnames(direction.table.maker)<- c("North", "South",  "East",  "West",  "Same City", "All Found", "Never Found")
  #   direction.table.maker
  # }, colnames = T, align = "l", hover = T, spascing = 'xs')
  
  output$directionPlotly <- renderPlotly({
    new.table <- data.frame(table(points.orig()@data[,c("bearClass")]))
    levels(new.table$Var1) <- c("North","South","East","West", "Same City","Never Found")
    p<- plot_ly(new.table,
            x = ~Var1,
            y = ~Freq,
            type = "bar",
            height = 300) %>% 
      layout(xaxis= list(title = ""), 
             yaxis = list(title = "Frequency", 
             range = c(0, bearClass.rounder))) #range keeps the plotly chart standardized
    p$elementId <- NULL #this syntax removes problems with warnings
    p 
    
  })
  
  #reset Denominations input <- fix; not working
  # observe({
  #   input$reset_input
  #   updateselectizeInput(session, "denomination", 
  #                        choices = c(sort(unique(elop.raw$Denomination_for_Tableau))),
  #                        multiple = TRUE,
  #                        options = list(plugins= list('remove_button'), placeholder = 'All Denominations'), 
  #                        server = TRUE)
  # })
  
  #create a piechart with denominations
  output$denominationPieChart <- renderPlotly({
   ax <- list(
     title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE)
    d <- plot_ly(data.frame(table(points.orig()@data[,c("Denomination_for_Tableau")])),
                 labels = ~Var1,
                 values = ~Freq,
                 type = 'pie',
                 height = 350,
                 textposition = 'inside',
                 textinfo = 'value+label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE) %>%
      layout(xaxis= ax, 
             yaxis = ax)
      d$elementId <- NULL #this syntax removes problems with warnings
      d 
    
  })

  output$textHeader <- renderText(text.filter())  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      fitBounds(-129,24.2,-65.58,50.54)%>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(minZoom = 1, maxZoom = 13, noWrap = F)) %>%
      addLayersControl(
        overlayGroups = c("Origin", "Found","Connections"),
        options = layersControlOptions(collapsed = FALSE)
      )       

  })
  observe({
    leafletProxy("mymap") %>%
      clearMarkers() %>% 
      clearMarkerClusters() %>%
      clearShapes()%>%
      addCircleMarkers(data = points.orig(), popup = ~popupw, group = "Origin",color = "#FF2000",radius=4, opacity = .8)%>%
      addCircleMarkers(data = points.found(), popup = ~popupw, group = "Found",color = "blue",radius=4, opacity = .6)%>% #,clusterOptions = markerClusterOptions())
      addCircleMarkers(data = points.connections(), popup = ~popupw, group = "Connections",color = "lime",radius=4, opacity = .8) %>%
      addPolylines(data = lines.connections(), popup = ~popupw, group = "Connections", color = "grey", opacity = .3)  %>%
      addPolygons(data=arrowheads.connections(), group = "Connections",  fillOpacity = .3, opacity = .3, popup = ~popupw, color = "grey", fillColor = "grey", stroke = F )
      zoom <- input$mymap_zoom
    
  })
  
  #output raw data table
  d <- SharedData$new(rdt, ~rowname)
  
  #highlight selected rows in the table
  output$x1 <- DT::renderDataTable(
    {rdt2 <- rdt[d$selection(),]
    dt <- DT::datatable(rdt, rownames = FALSE, 
                        options = list(
                          columnDefs = list(list(visible=FALSE,targets=c(0))),
                          pageLength = 20, 
                          lengthMenu = c (20, 50, 100, 200, 266)))
    if (NROW(rdt2) == 0) {
      dt
    } else {
      DT::formatStyle(dt, "Rowname", target = "row",
                      color = DT::styleEqual(rdt2$rowname, rep("white", length(rdt2$rowname))),
                      backgroundColor = DT::styleEqual(rdt2$rowname, rep("black", length(rdt2$rowname))))  
    }
    })
  
  #output dygraph
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
  
  output$dygraph_description <- renderUI({
    "This graph represents the annual number of elopements among the ministers in four major Protestant denominations.
    The Methodists were the most represented, with 90 elopers. 
    The Baptists came in second at 56. 
    The Presbyterians were third with 17. 
    Finally, the Congregationalists came in at 11 total elopements. 
    Hovering over any year with the mouse will summarize the number of eloping ministers by denomination for that year. 
    Use the slider at the bottom to zoom in on a specific date range.
    Please note that since exact elopements dates were not always available in the dataset, the month of January was used by default for every year represented in the graph."
  })
  
  output$settlement_about <- renderUI({
    "This page tracks where the Runaway Reverends chose to elope over time.
    We were interested in whether most preferred to disappear in larger cities and, if so, whether that pattern changed over time.
    Select a year or a span of years to see how the desired new location patterns changed over time."
  })
  
  #output settlment size data
  locationChange <- reactive({
    #initialize the variables
    sameSize <- 0
    upSize <- 0
    downSize <- 0
    sameCity <- 0
    
    #loop through selected years
    for(i in seq(input$theYear[1],input$theYear[2],1))
    {#SAME SIZE
      #adds up the number of times the source and destination cities are the same, for the given year, i
      currentSum <- sum(
        #conditions of sum 
        ( #test the two city size designations
          ((df[2]) == (df[3]))
          & 
            #test for the chosen year
            apply(df, 1, function(v) {
              #if the year is correct and if the origin and destination are not the same
              (v[1] == i) & (v[4] != v[5])})
       ) #conditions of sum
        , na.rm=TRUE) #end sum
      
      sameSize = sameSize + currentSum

      #ORIGIN SMALLER
      currentSum <- sum((((df[2]) < (df[3])) & apply(df, 1, function(v) { (v[1] == i)} )), na.rm=TRUE)
      upSize =upSize + currentSum
      print(upSize)
      
      #ORIGIN LARGER
      currentSum <- sum((((df[2]) > (df[3])) & apply(df, 1, function(v) { (v[1] == i)})), na.rm=TRUE)
      downSize = downSize + currentSum
      print(downSize)
      
      #SAME ORIGIN AND DESTINATION
      currentSum <- sum((((df[2]) == (df[3])) & apply(df, 1, function(v) {(v[1] == i) & (v[4] == v[5])} )), na.rm=TRUE)
      sameCity = sameCity + currentSum}
    
    summedList <- c(upSize, sameSize, downSize, sameCity)
    return(summedList)}) #locationChange reactive function
  
  wentSmaller <- 1
  wentLarger <- 3
  
  output$barplot <- renderPlot({
    barplot(
      c(locationChange()[1],locationChange()[2],locationChange()[3],locationChange()[4]),
      ylim=c(0,40),
      col = "#367CBB",
      names.arg=c("Larger", "Same Size", "Smaller", "Same City"))})
  
  output$text_summary <- renderUI({ 
    str1 <- paste("Moved to a larger city: ", locationChange()[1])
    str2 <- paste("Moved to a city of the same size: ", locationChange()[2])
    str3 <- paste("Moved to a smaller city: ", locationChange()[3])
    str4 <- paste("Returned to the same city: ", locationChange()[4])
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))})
  }

shinyApp(ui, server)

# library(rsconnect)
# deployApp()
