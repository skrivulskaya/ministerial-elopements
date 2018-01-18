rm(list=ls(all=TRUE)) # clear memory


#MLS Todo: 
#Add Arrowheads to main map. 
#Deal with multiple people in same place (randomize points). 
  #Find a good symbol for people that are found in the same city
# classification direction

packages<- c("maptools","rgdal","leaflet","htmlwidgets","shiny","ggmap","rsconnect", "leaflet.minicharts") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("/home/matthew/GIT/R_Scripts/ministerial-elopements")
setwd("E:\\GIT_Checkouts\\R_Scripts\\ministerial-elopements")
latlong <- "+init=epsg:4326"


#Loading the geocoded data
elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)





elop.raw$popupw <- paste(sep = "",  "<b>",elop.raw$Full_Name,"</b><br/>",
                         "What = ",elop.raw$Accusations,"<br/>",
                         "Origin = ",elop.raw$Location_Origin,"<br/>",
                         "Found = ",elop.raw$Location_Found,"<br/>"
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



#Examples of figures
b <- rbind(table(elop.raw$Decade),table(elop.raw$Eloped_with_Married_Woman, elop.raw$Decade))
barplot(b)

#by decade



#Mapping Section

#converting to point data frames for mapping
orig.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Origin)),]
orig.spdf$Latitude_Origin <- orig.spdf$Latitude_Origin - (runif(nrow(orig.spdf))-.5)/20
orig.spdf$Longtitude_Origin <- orig.spdf$Longtitude_Origin - (runif(nrow(orig.spdf))-.5)/20


coordinates(orig.spdf)=~Longtitude_Origin+Latitude_Origin
proj4string(orig.spdf) <- CRS(latlong)

found.spdf <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
coordinates(found.spdf)=~Longtitude_Found+Latitude_Found
proj4string(found.spdf) <- CRS(latlong)

elop.map <-elop.comp[which(elop.comp$Location_Origin != elop.comp$Location_Found),]
same.spdf <- elop.comp[which(elop.comp$Location_Origin == elop.comp$Location_Found),]
coordinates(same.spdf)=~Longtitude_Found+Latitude_Found
proj4string(same.spdf) <- CRS(latlong)


#New Testing
m <- leaflet()
m %>% addTiles() %>%
  addMarkers(data = orig.spdf, popup = ~popupw, group = "Origin") %>%
  addMarkers(data = found.spdf, popup = ~popupw, group = "Found",clusterOptions = markerClusterOptions()) %>%
  addCircleMarkers(data = same.spdf, popup = ~popupw, group = "Connections",color = "navy",radius=5) %>%
  
  addLayersControl(
    overlayGroups = c("Origin", "Found","Connections"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
addFlows(
  elop.map$Longtitude_Origin, elop.map$Latitude_Origin, elop.map$Longtitude_Found, elop.map$Latitude_Found,
  # flow = .01,
  maxThickness = 2,
  color = "navy",
  popup = popupArgs(showTitle = F, showValues = T, labels = NULL,
  supValues = NULL, supLabels = colnames(elop.map), html = "popupw",
  noPopup = T, digits = NULL)
)%>% # end add flows
addPolylines(data = complete.lines, popup = ~popupw, group = "Connections") 
  



#end new testing


#Basic Leaflet Maps
m <- leaflet()
m %>% addTiles() %>%
  addPolylines(data = complete.lines, popup = ~popupw, group = "Connections") %>%
  # addFlows(
  #   elop.raw$Longtitude_Origin, elop.raw$Latitude_Origin, elop.raw$Longtitude_Found, elop.raw$Latitude_Found,
  #   flow = 1
  # ) %>%
  # addMarkers(data = orig.spdf, popup = ~popupw, group = "Origin",clusterOptions = markerClusterOptions()) %>%
  # addMarkers(data = found.spdf, popup = ~popupw, group = "Found",clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    overlayGroups = c("Origin", "Found","Connections"),
    options = layersControlOptions(collapsed = FALSE)
  )





#Building Shiny Interface
ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  sliderInput("range", "Range:",
              min = 1870, max = 1915,
              value = c(1870,1915), sep = ""),
  # selectInput("decade", "Decade:",
  #             c("all", "1870s","1880s","1890s","1900s","1910-1914")),
  selectizeInput("denomination", "Denomination:",
                 choices = c("all", sort(unique(elop.raw$Denomination_for_Tableau)))),
  checkboxInput("CompCheck","Complete cases", value = FALSE, width = NULL)
)

server <- function(input, output, session) {
  
  points <- eventReactive(c(input$range, input$denomination, input$CompCheck), {
    working.spdf <- orig.spdf
    if(input$CompCheck){
      working.spdf <- orig.spdf[which(!is.na(orig.spdf$Latitude_Found)),]
    }
    if (input$denomination == "all"){
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2]),])
    }else{
      return(working.spdf[which(working.spdf$Year >= input$range[1] & working.spdf$Year <= input$range[2] & working.spdf$Denomination_for_Tableau == input$denomination),])  
    }
    
    
  }, ignoreNULL = FALSE)#end points1
  
  points2 <- eventReactive(c(input$range, input$denomination), {
    if (input$denomination == "all"){
      found.spdf[which(found.spdf$Year >= input$range[1] & found.spdf$Year <= input$range[2]),]
    }else{
      found.spdf[which(found.spdf$Year >= input$range[1] & found.spdf$Year <= input$range[2] & found.spdf$Denomination_for_Tableau == input$denomination),]  
    }
    
    
  }, ignoreNULL = FALSE)#end points1
  
  lines <- eventReactive(c(input$range, input$denomination), {
    if (input$denomination == "all"){
      print(nrow(complete.lines))
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
      addCircleMarkers(data = points(), popup = ~popupw, group = "Origin",color = "green",radius=3)%>%
      addCircleMarkers(data = points2(), popup = ~popupw, group = "Found",color = "red",radius=3)%>%#,clusterOptions = markerClusterOptions())
      addPolylines(data = lines(), popup = ~popupw, group = "Connections")
    
  })
}

shinyApp(ui, server)



elop.map$deltaPop <- elop.map$Location_Origin_Approximate_Population - elop.map$Location_Found_Approximate_Population
plot(elop.map$Year[which(elop.map$Location_Found_Approximate_Population<100000)], elop.map$deltaPop[which(elop.map$Location_Found_Approximate_Population<100000)])
aggregate(elop.comp$deltaPop,by=list(elop.comp$Decade), FUN = sd, na.rm=T)
table(elop.comp$Decade)
