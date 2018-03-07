rm(list=ls(all=TRUE)) # clear memory

# to do - March 7, 2018:
  # city size chart & intergration into the Shiny app tabs (Dan)
  # fix the New Zealand line (Mat)
  # integrate the dygraphs thing into another tab
      # example: https://faidherbard.shinyapps.io/joburgdygraph/
      # code for example: https://stackoverflow.com/questions/30176303/using-dygraph-with-shiny
  # minor - rename tabs so they make sense as to what they are actually doing (Suzanna)
  # on the home page, display the following output tables:
    # summary table of the direction in which ministers moved (like the old table we had as output but prettier)
    # summary table of the number of ministers by denomination
    # summary table of how many ministers are found or return to the same place (vs. those who are never found)
    # brainstorm to see if there is any other information that would be helpful to display
    # Add text describing the filter above the tables
    # Add complete cases into the table of where things are found.
  # find a good-looking way to display the raw data on that tab
    # include the following columns: Full_Name, Year, Age, Location_Origin, Denomination, Accusations, Female_Involved, Female_Age, Found_Y.N., Year_Found, Location_Found, Arrested_Y_N


# packages<- c("rgdal","leaflet","htmlwidgets","shiny","ggmap")
library(rgdal)
library(leaflet)
library(shiny)
library (geosphere)
library(dygraphs)
library(dplyr)
library(xts)
# library(markdown)
# library(ggmap)
# library(leaflet.minicharts)
# library(maptools)

# setwd("/Users/suzannakrivulskaya/Box Sync/Dissertation Stuff/Dissertation/Data/ministerial-elopements")
# setwd("/home/matthew/GIT/R_Scripts/ministerial-elopements")
# setwd("E:\\GIT_Checkouts\\R_Scripts\\ministerial-elopements")

latlong <- "+init=epsg:4326"

#Load  geocoded data
elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)

#Generate new locations for duplicate places 

elop.raw$dup_origin <- elop.raw$Location_Origin %in% elop.raw[duplicated(elop.raw$Location_Origin),]$Location_Origin
elop.raw$dup_found <- elop.raw$Location_Found %in% elop.raw[duplicated(elop.raw$Location_Found),]$Location_Found
elop.raw$Latitude_Origin <- ifelse(elop.raw$dup_origin, elop.raw$Latitude_Origin - (runif(nrow(elop.raw))-.5)/20,elop.raw$Latitude_Origin)
elop.raw$Longtitude_Origin <- ifelse(elop.raw$dup_origin, elop.raw$Longtitude_Origin - (runif(nrow(elop.raw))-.5)/20,elop.raw$Longtitude_Origin)
elop.raw$Latitude_Found <- ifelse(elop.raw$dup_found, elop.raw$Latitude_Found - (runif(nrow(elop.raw))-.5)/20,elop.raw$Latitude_Found)
elop.raw$Longtitude_Found <- ifelse(elop.raw$dup_found, elop.raw$Longtitude_Found - (runif(nrow(elop.raw))-.5)/20,elop.raw$Longtitude_Found)
#end locations for duplicate places

#Generate html popup
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

#Generate directional information
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

#Create lines
elop.comp <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
row.names(elop.comp) <- NULL

a <- (gcIntermediate(elop.comp[,c("Longtitude_Origin","Latitude_Origin")], elop.comp[,c("Longtitude_Found","Latitude_Found")],addStartEnd = T, breakAtDateLine = T, n=150, sp = T) )
complete.lines <- SpatialLinesDataFrame(a,elop.comp)
#Fixing a dateline issue for that one that ran to new zealand: TODO: Still not perfect
negs <- as.matrix(coordinates(complete.lines[94,])[[1]][[2]])
negs[,1] <- (negs[,1])-360
complete.lines@lines[[94]]@Lines[[2]]@coords[] <- negs
#end create lines

#Make directional arrows for the lines
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

#Mapping Section
#Convert to point data frames for mapping
#Randomize identical points

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





#Build Shiny interface
ui <- fluidPage(
  title = "Runaway Reverends",
  navbarPage("Runaway Reverends",
             tabPanel("Map",
                      
    fluidRow(
    column(3,offset = 0, style='padding-right:20px;',
           (wellPanel(
             sliderInput("range", "Date Range:",
                         min = 1870, max = 1914,
                         value = c(1870,1914), sep = ""),
             selectizeInput("denomination", "Denomination:",
                            choices = c("All", sort(unique(elop.raw$Denomination_for_Tableau)))),
             checkboxInput("CompCheck","Complete cases", value = FALSE, width = NULL),
             checkboxGroupInput("direction", label = ("Direction:"), 
                                choices = c("Same","North", "East", "West", "South"),
                                selected = c("Same", "North", "East", "West", "South"))
             ))),
    
    column(9,(wellPanel(leafletOutput("mymap")))),
    
    column(12,
           tableOutput("thisTable"))
    
    ) #end column
    ),#end first tabPanel
    tabPanel("Summary"),
    tabPanel("Raw Data")
    
) #end tabpanel Map
)
  
#end fluidpage

server <- function(input, output, session) {
  
  points.orig <- eventReactive(c(input$range, input$denomination, input$CompCheck), {
    #TODO: Should be able to make this pull and generate from one data frame all at the same time
    #TODO: Make this work incrementially and return a final data frame rather than do everything independantly
    #TODO: Get in the bearClass filter: temp.lines <- temp.lines[which(temp.lines$bearClass %in% input$direction),]

    working.spdf <- orig.spdf
    if(input$CompCheck){
      working.spdf <- orig.spdf[which(!is.na(orig.spdf$Latitude_Found)),]
    }
    if (input$denomination == "All"){
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2]),])
    }else{
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2] & working.spdf$Denomination_for_Tableau == input$denomination),])  
    }
    
    
  }, ignoreNULL = FALSE)#end points.orig
  
  points.found <- eventReactive(c(input$range, input$denomination), {
    if (input$denomination == "All"){
      found.spdf[which(found.spdf$Year >= input$range[1] & found.spdf$Year <= input$range[2]),]
    }else{
      found.spdf[which(found.spdf$Year >= input$range[1] & found.spdf$Year <= input$range[2] & found.spdf$Denomination_for_Tableau == input$denomination),]  
    }
    
    
  }, ignoreNULL = FALSE)#end points.found
  
  points.connections <- eventReactive(c(input$range, input$denomination, input$direction), {
    if (!"Same" %in% input$direction){
      return(same.spdf [which(same.spdf$bearClass == "Same"),])
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
  
  output$value <- renderPrint({ input$direction })
  
  #creating an output table with directions
 
  output$thisTable <- renderTable(table(points.orig()@data[,c("bearClass")]),striped = T, colnames = F)  
    
    
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
      addCircleMarkers(data = points.orig(), popup = ~popupw, group = "Origin",color = "brown",radius=4)%>%
      addCircleMarkers(data = points.found(), popup = ~popupw, group = "Found",color = "green",radius=4)%>%#,clusterOptions = markerClusterOptions())
      addCircleMarkers(data = points.connections(), popup = ~popupw, group = "Connections",color = "navy",radius=4, opacity = 1) %>%
      addPolylines(data = lines.connections(), popup = ~popupw, group = "Connections", color = "grey", opacity = .5)  %>%
      addPolygons(data=arrowheads.connections(), group = "Connections",  fillOpacity = .5, opacity = .5, popup = ~popupw, color = "grey", fillColor = "grey", stroke = F )
      zoom <- input$mymap_zoom
    
  })
}

shinyApp(ui, server)
#end Shiny app

# library(rsconnect)
# deployApp()
