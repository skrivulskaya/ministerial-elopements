rm(list=ls(all=TRUE)) # clear memory

packages<- c("rgdal","leaflet","htmlwidgets","shiny","ggmap") # list the packages that you'll need
library(rgdal)
library(leaflet)
library(shiny)
library(ggmap)


setwd("/Users/suzannakrivulskaya/Box Sync/Dissertation Stuff/Dissertation/Data/ministerial-elopements")
latlong <- "+init=epsg:4326"


#Loading the geocoded data
elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)

# (sep = "",  "<b>",elop.raw$Full_Name,"</b><br/>",
#"Name = ",elop.raw$Accusations,"<br/>",

elop.raw$popupw <- paste(sep = "",  "<b>",elop.raw$Full_Name,"</b><br/>",
                         "Denomination: ",elop.raw$Denomination_for_Tableau, "<br/>",
                         "Age: ",elop.raw$Age, "<br/>",
                         "Year Eloped: ",elop.raw$Year,"<br/>",
                         "Origin: ",elop.raw$Location_Origin,"<br/>",
                         "Accusation: ",elop.raw$Accusations,"<br/>",
                         "Age of Female: ",elop.raw$Female_Age, "<br/>",
                         "Found: ",elop.raw$Location_Found,"<br/>",
                         "Year Found: ",elop.raw$Year_Found,"<br/>"
) #A bit of HTML To make the popups on the lines


#Creating the Line objects out of the point objects:
elop.comp <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
row.names(elop.comp) <- NULL
lines <- list()
for (i in 1:nrow(elop.comp)) { 
  lines[[i]] <- Lines(list(Line(rbind(c(elop.comp$Longtitude_Origin[i],elop.comp$Latitude_Origin[i]), c(elop.comp$Longtitude_Found[i], elop.comp$Latitude_Found[i]) ))), as.character(i)) 
  #print(i)
}
complete.lines <- SpatialLinesDataFrame(SpatialLines(lines),elop.comp)



#Mapping Section

#converting to point data frames for mapping
orig.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Origin)),]
coordinates(orig.spdf)=~Longtitude_Origin+Latitude_Origin
proj4string(orig.spdf) <- CRS(latlong)

found.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
coordinates(found.spdf)=~Longtitude_Found+Latitude_Found
proj4string(found.spdf) <- CRS(latlong)



#Building Shiny Interface
ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  sliderInput("range", "Range:",
              min = 1870, max = 1915,
              value = c(1870,1915), sep = ""),
 # selectInput("decade", "Decade:",
               #c("all", "1870s","1880s","1890s","1900s","1910-1914")),
  selectizeInput("denomination", "Denomination:",
                 choices = c("All", sort(unique(elop.raw$Denomination_for_Tableau)))),
  checkboxInput("CompCheck","Complete cases", value = FALSE, width = NULL)
)

server <- function(input, output, session) {
  
  points <- eventReactive(c(input$range, input$denomination, input$CompCheck), {
    working.spdf <- orig.spdf
    if(input$CompCheck){
      working.spdf <- orig.spdf[which(!is.na(orig.spdf$Latitude_Found)),]
    }
    if (input$denomination == "All"){
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2]),])
    }else{
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2] & working.spdf$Denomination_for_Tableau == input$denomination),])  
    }
    
    
  }, ignoreNULL = FALSE)#end points1
  
  points2 <- eventReactive(c(input$range, input$denomination), {
    if (input$denomination == "All"){
      found.spdf[which(found.spdf$Year >= input$range[1] & found.spdf$Year <= input$range[2]),]
    }else{
      found.spdf[which(found.spdf$Year >= input$range[1] & found.spdf$Year <= input$range[2] & found.spdf$Denomination_for_Tableau == input$denomination),]  
    }
    
    
  }, ignoreNULL = FALSE)#end points1
  
  lines <- eventReactive(c(input$range, input$denomination), {
    if (input$denomination == "All"){
      complete.lines[which(complete.lines$Year >= input$range[1] & complete.lines$Year <= input$range[2]),]
      
    }else{
      complete.lines[which(complete.lines$Year >= input$range[1] & complete.lines$Year <= input$range[2] & complete.lines$Denomination_for_Tableau == input$denomination),]  
    }
    
    
  }, ignoreNULL = FALSE)#end points1
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      fitBounds(-129,24.2,-65.58,50.54)%>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addLayersControl(
        overlayGroups = c("Origin", "Found","Connections"),
        options = layersControlOptions(collapsed = FALSE)
      )       
    # addPolylines(data = complete.lines, popup = ~popupw, group = "Connections")
    
    
    # %>% addMarkers(data = points())
  })
  observe({
    leafletProxy("mymap") %>%
      clearMarkers() %>% 
      clearMarkerClusters() %>%
      clearShapes()%>%
      # addMarkers(data = points()) %>%
      # addMarkers(data = points(), popup = ~popupw, group = "Origin") %>%
      # addMarkers(data = points2(), popup = ~popupw, group = "Found")#,clusterOptions = markerClusterOptions())
      addCircleMarkers(data = points(), popup = ~popupw, group = "Origin",color = "brown",radius=3)%>%
      addCircleMarkers(data = points2(), popup = ~popupw, group = "Found",color = "green",radius=3)%>%#,clusterOptions = markerClusterOptions())
      addPolylines(data = lines(), popup = ~popupw, group = "Connections")
    
  })
}

shinyApp(ui, server)
