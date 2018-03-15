rm(list=ls(all=TRUE)) #clear memory

# new to do - March 9, 2019:

# to do - March 7, 2018:
  # on the home page, display the following output tables:
    # brainstorm to see if there is any other information that would be helpful to display
  # add raw data tab (Suzanna) - done 
  
library(rgdal)
library(leaflet)
library(shiny)
library (geosphere)
library(dygraphs)
library(dplyr)
library(xts)
library(plotly)
library(DT)
library(crosstalk)
library(dygraphs)
library(datasets)

# setwd("/Users/suzannakrivulskaya/Box Sync/Dissertation Stuff/Dissertation/Data/ministerial-elopements")
# setwd("/home/matthew/GIT/R_Scripts/ministerial-elopements")
# setwd("E:\\GIT_Checkouts\\R_Scripts\\ministerial-elopements")

latlong <- "+init=epsg:4326"

#load geocoded data
elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)


#build the raw data table
raw.data.tab <- read.csv("raw_data_tab.csv",stringsAsFactors = F)
rdt <- raw.data.tab %>%
  tibble::rownames_to_column()
#end raw data table


#build the dygraph tables
denombyyear <- table(elop.raw$Year, elop.raw$Denomination_for_Tableau)
write.csv(denombyyear, file = "denombyyear.csv")
dby.raw <- read.csv("denombyyear.csv",stringsAsFactors = F)
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
#end dygraph tables


#generate new locations for duplicate places 
elop.raw$dup_origin <- elop.raw$Location_Origin %in% elop.raw[duplicated(elop.raw$Location_Origin),]$Location_Origin
elop.raw$dup_found <- elop.raw$Location_Found %in% elop.raw[duplicated(elop.raw$Location_Found),]$Location_Found
elop.raw$Latitude_Origin <- ifelse(elop.raw$dup_origin, elop.raw$Latitude_Origin - (runif(nrow(elop.raw))-.5)/20,elop.raw$Latitude_Origin)
elop.raw$Longtitude_Origin <- ifelse(elop.raw$dup_origin, elop.raw$Longtitude_Origin - (runif(nrow(elop.raw))-.5)/20,elop.raw$Longtitude_Origin)
elop.raw$Latitude_Found <- ifelse(elop.raw$dup_found, elop.raw$Latitude_Found - (runif(nrow(elop.raw))-.5)/20,elop.raw$Latitude_Found)
elop.raw$Longtitude_Found <- ifelse(elop.raw$dup_found, elop.raw$Longtitude_Found - (runif(nrow(elop.raw))-.5)/20,elop.raw$Longtitude_Found)
#end locations for duplicate places

#generate html popup
elop.raw$popupw <- paste(sep = "",  "<b>",elop.raw$Full_Name,"</b><br/>",
                         "Denomination: ",elop.raw$Denomination_for_Tableau, "<br/>",
                         "Age: ",elop.raw$Age, "<br/>",
                         "Year Eloped: ",elop.raw$Year,"<br/>",
                         "Origin: ",elop.raw$Location_Origin,"<br/>",
                         "Accusation: ",elop.raw$Accusations,"<br/>",
                         "Age of Female: ",elop.raw$Female_Age, "<br/>",
                         "Found: ",elop.raw$Location_Found,"<br/>",
                         "Year Found: ",elop.raw$Year_Found,"<br/>"
) #end html popup

#generate directional information
elop.raw$bearing[(!is.na(elop.raw$Latitude_Found))& (elop.raw$Location_Origin != elop.raw$Location_Found)] <- bearingRhumb(elop.raw[((!is.na(elop.raw$Latitude_Found))& (elop.raw$Location_Origin != elop.raw$Location_Found)),c("Longtitude_Origin","Latitude_Origin")],elop.raw[((!is.na(elop.raw$Latitude_Found))& (elop.raw$Location_Origin != elop.raw$Location_Found)),c("Longtitude_Found","Latitude_Found")])
elop.raw$bearClass[(elop.raw$bearing < 45 ) | (elop.raw$bearing >= 315)] <- "North"
elop.raw$bearClass[(elop.raw$bearing < 135) & (elop.raw$bearing >= 45)] <- "East"
elop.raw$bearClass[(elop.raw$bearing < 225) & (elop.raw$bearing >= 135)] <- "South"
elop.raw$bearClass[(elop.raw$bearing < 315) & (elop.raw$bearing >= 225)] <- "West"
elop.raw[which(elop.raw$Location_Origin == elop.raw$Location_Found),]$bearClass <- 'Same'
elop.raw[is.na(elop.raw$Location_Found),"bearClass"] <- 'Never'
elop.raw$bearClass <- factor(elop.raw$bearClass, levels=c("North","South", "East", "West","Same","Never"))
# table(elop.raw$bearClass)
# elop.raw$popupw <- paste (sep = "", elop.raw$popupw,"bearing: ",elop.raw$bearClass,"<br/>")
#end directional information

#create lines
elop.comp <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
row.names(elop.comp) <- NULL
a <- (gcIntermediate(elop.comp[,c("Longtitude_Origin","Latitude_Origin")], elop.comp[,c("Longtitude_Found","Latitude_Found")],addStartEnd = T, breakAtDateLine = T, n=150, sp = T) )
complete.lines <- SpatialLinesDataFrame(a,elop.comp)
#Fixing a dateline issue for that one that ran to new zealand: TODO: Still not perfect
negs <- as.matrix(coordinates(complete.lines[94,])[[1]][[2]])
negs[,1] <- (negs[,1])-360
complete.lines@lines[[94]]@Lines[[2]]@coords[] <- negs
#end create lines

#make directional arrows for the lines
markers.df <- elop.comp[which(elop.comp$Location_Origin != elop.comp$Location_Found),]
markers.df$midlong <- apply(markers.df[,c("Longtitude_Origin","Longtitude_Found")], 1, mean) 
markers.df$midlat <- apply(markers.df[,c("Latitude_Origin","Latitude_Found")], 1, mean) 
a <- (gcIntermediate(markers.df[,c("Longtitude_Origin","Latitude_Origin")], markers.df[,c("Longtitude_Found","Latitude_Found")], n=1) )
a <- do.call(rbind.data.frame, a)
markers.df$midlong <- a$lon
markers.df$midlat <- a$lat
# markers.df$bearing <- bearingRhumb(markers.df[,c("Longtitude_Origin","Latitude_Origin")],markers.df[,c("Longtitude_Found","Latitude_Found")])
#Now can calculate the direction of travel
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
  # print(arrowheads)
  return(arrowheads)
}

poly.arrrows <- build.arrowheads()
#end directional arrows

#mapping section
#convert to point data frames for mapping
#randomize identical points

orig.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Origin)),]
a<- data.frame(table(orig.spdf$Location_Origin))
a$Location_Origin <- as.character(a$Var1)
a$Var1 <- NULL
orig.spdf <- merge(orig.spdf,  a, by="Location_Origin",all=T)
orig.spdf$Latitude_Origin <-ifelse((orig.spdf$Freq > 1),  orig.spdf$Latitude_Origin - (runif(nrow(orig.spdf))-.5)/40,orig.spdf$Latitude_Origin)
orig.spdf$Longtitude_Origin <-ifelse((orig.spdf$Freq > 1), orig.spdf$Longtitude_Origin - (runif(nrow(orig.spdf))-.5)/40,orig.spdf$Longtitude_Origin)

coordinates(orig.spdf)=~Longtitude_Origin+Latitude_Origin
proj4string(orig.spdf) <- CRS(latlong)

found.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
coordinates(found.spdf)=~Longtitude_Found+Latitude_Found
proj4string(found.spdf) <- CRS(latlong)

same.spdf <- elop.comp[which(elop.comp$Location_Origin == elop.comp$Location_Found),]
coordinates(same.spdf)=~Longtitude_Found+Latitude_Found
proj4string(same.spdf) <- CRS(latlong)
#end mapping section


#table testing
#change the order by factorizing
table(elop.raw$bearClass)
table(elop.raw$Denomination_for_Tableau)



#build Shiny interface
ui <- fluidPage(
  title = "Runaway Reverends",
  navbarPage("Runaway Reverends",
             tabPanel("Home",
    fluidRow(
    column(3,offset = 0, style='padding-right:10px;',
           (wellPanel(
             sliderInput("range", "Date Range:",
                         min = 1870, max = 1914,
                         value = c(1870,1914), sep = ""),
             selectizeInput("denomination", "Denomination:",
                            choices = c("All", sort(unique(elop.raw$Denomination_for_Tableau)))),
             checkboxInput("CompCheck","Only Found", value = FALSE, width = NULL),
             checkboxGroupInput("direction", label = ("Direction:"), 
                                choices = c("Same","North", "East", "West", "South"),
                                selected = c("Same", "North", "East", "West", "South"))))),
    column(9,(wellPanel(leafletOutput("mymap")))),
    column(12, textOutput("textHeader")),
    column(12, br()),
    column(12, tableOutput("directionTable")),
    column(12, hr()),
    column(12, h4("Denominations: ")),
    column(12, tableOutput("denominationTable"))#end column
    #end column
    ) #end fluidrow
    ),#end first tabPanel
    
    tabPanel("Settlement Size"),
    
    tabPanel("Four Major Denominations",
             fluidRow(titlePanel("Number of Runaway Ministers among Four Major Denominations by Year, 1970-1914"),
                      verbatimTextOutput("dygraph_description"),
                      br(),
                      wellPanel(dygraphOutput("dygraph")))),
    
    tabPanel("Raw Data",
             fluidRow(
               wellPanel(DT::dataTableOutput("x1"),
                         fluidRow(p(class = 'text-center')))))
  ) #end navbarPage
) #end fluidpage


server <- function(input, output, session) {
  
  points.orig <- eventReactive(c(input$range, input$denomination, input$CompCheck, input$direction), {
    #TODO: Should be able to make this pull and generate from one data frame all at the same time
    #TODO: Make this work incrementially and return a final data frame rather than do everything independantly

    working.spdf <- orig.spdf
    if(input$CompCheck){
      working.spdf <- orig.spdf[which(!is.na(orig.spdf$Latitude_Found)),]
    }
    working.spdf <- working.spdf[which(working.spdf$bearClass %in% c(input$direction, "Never")),]
    
    if (input$denomination == "All"){
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2]),])
    }else{
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2] & working.spdf$Denomination_for_Tableau == input$denomination),])  
    }
    
    
  }, ignoreNULL = FALSE)#end points.orig
  
  points.found <- eventReactive(c(input$range, input$denomination, input$direction), {
    working.found <- found.spdf[which(found.spdf$bearClass %in% input$direction),]

    if (input$denomination == "All"){
      working.found <- working.found[which(working.found$Year >= input$range[1] & working.found$Year <= input$range[2]),]
    }else{
      working.found <- working.found[which(working.found$Year >= input$range[1] & working.found$Year <= input$range[2] & working.found$Denomination_for_Tableau == input$denomination),]  
    }
    return(working.found)
    
  }, ignoreNULL = FALSE)#end points.found
  
  points.connections <- eventReactive(c(input$range, input$denomination, input$direction), {
    #Fix this so it works like the others: with the temp.variable: currently not reactive on dates.
    if (!"Same" %in% input$direction){
      return(same.spdf [which(same.spdf$bearClass == "Jeff"),])
    }
    if (input$denomination == "All"){
      temp.same <- same.spdf
    }else{
      temp.same <- same.spdf[which(same.spdf$Denomination_for_Tableau == input$denomination),]
    }
    #filter years
    temp.same <- temp.same[which(temp.same$Year >= input$range[1] & temp.same$Year <= input$range[2]),]
   
    return(temp.same)
    
  }, ignoreNULL = FALSE)#end points.found
  
  lines.connections <- eventReactive(c(input$range, input$denomination, input$direction), {
    if (input$denomination == "All"){
      temp.lines <- complete.lines
    }else{
      temp.lines <- complete.lines[which(complete.lines$Denomination_for_Tableau == input$denomination),]
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
      # print("build as is")
      poly.arrows <- build.arrowheads(arrow.scale = 4)
    }else{
      poly.arrows <- build.arrowheads(arrow.scale = input$mymap_zoom)
    }
    
    # print(zoom)
    if (input$denomination == "All"){
      temp.arrows <- poly.arrows
    }else{
      temp.arrows <- poly.arrows[which(poly.arrows$Denomination_for_Tableau == input$denomination),]
    }
    #filter years
    temp.arrows <- temp.arrows[which(temp.arrows$Year >= input$range[1] & temp.arrows$Year <= input$range[2]),]
    #filter bearing
    temp.arrows <- temp.arrows[which(temp.arrows$bearClass %in% input$direction),]
    # return(complete.lines[
    return(temp.arrows)
    
  }, ignoreNULL = FALSE)#end lines.connections
  
  text.filter <- eventReactive(c(input$range, input$denomination, input$CompCheck, input$direction),{
    out.string <- "Filter = "
    if (input$range[1] == 1870 & input$range[2]  == 1914){
      out.string <- paste(out.string, "All years, ", sep = "")
    } else{
      out.string <- paste(out.string, input$range[1], " - ", input$range[2], ", ", sep="")
    } 
    if (input$denomination == "All"){
      out.string <- paste(out.string, "All demoninations, ", sep = "")
    }else{
      out.string <- paste(out.string, "just ",input$denomination, sep="")
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
  
  #create an output table with directions
 
  output$directionTable <- renderTable(table(points.orig()@data[,c("bearClass")]),striped = T, colnames = F)
  output$denominationTable <- renderTable(table(points.orig()@data[,c("Denomination_for_Tableau")]),striped = T, colnames = F)
  
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
      addCircleMarkers(data = points.orig(), popup = ~popupw, group = "Origin",color = "#882255",radius=4, opacity = .8)%>%
      addCircleMarkers(data = points.found(), popup = ~popupw, group = "Found",color = "#44AA99",radius=4, opacity = .8)%>% #,clusterOptions = markerClusterOptions())
      addCircleMarkers(data = points.connections(), popup = ~popupw, group = "Connections",color = "grey",radius=4, opacity = .8) %>%
      addPolylines(data = lines.connections(), popup = ~popupw, group = "Connections", color = "grey", opacity = .3)  %>%
      addPolygons(data=arrowheads.connections(), group = "Connections",  fillOpacity = .3, opacity = .3, popup = ~popupw, color = "grey", fillColor = "grey", stroke = F )
      zoom <- input$mymap_zoom
    
  })
  
  #output raw data table
  #outputting table with raw data
  d <- SharedData$new(rdt, ~rowname)
  
  # highlight selected rows in the table
  output$x1 <- DT::renderDataTable(
    {rdt2 <- rdt[d$selection(),]
    dt <- DT::datatable(rdt, rownames = FALSE, 
                        options = list(
                          #hide the "rownames" column
                          columnDefs = list(list(visible=FALSE,targets=c(0))),
                          #define default number of rows displayed
                          pageLength = 20, 
                          #select view options for number of rows displayed
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
#end Shiny app

# library(rsconnect)
#deployApp()
