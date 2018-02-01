rm(list=ls(all=TRUE)) # clear memory


#MLS Todo: 
#Add Arrowheads to main map. 
  #Deal with multiple people in same place (randomize points). 
  #Find a good symbol for people that are found in the same city
# classification direction

packages<- c("maptools","rgdal","leaflet","htmlwidgets","shiny","ggmap","rsconnect", "leaflet.minicharts") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("/home/matthew/GIT/R_Scripts/ministerial-elopements")
# setwd("E:\\GIT_Checkouts\\R_Scripts\\ministerial-elopements")
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

markers.spdf <- elop.comp
markers.spdf$midlong <- apply(markers.spdf[,c("Longtitude_Origin","Longtitude_Found")], 1, mean) 
markers.spdf$midlat <- apply(markers.spdf[,c("Latitude_Origin","Latitude_Found")], 1, mean) 

# coordinates(markers.spdf)=midPoint(elop.comp[,c("Longtitude_Origin", "Latitude_Origin")],elop.comp[,c("Longtitude_Found", "Latitude_Found")],f=0)
# coordinates(markers.spdf)=gcIntermediate(elop.comp[,c("Longtitude_Origin", "Latitude_Origin")],elop.comp[,c("Longtitude_Found", "Latitude_Found")], n=1, breakAtDateLine=FALSE, addStartEnd=FALSE, sp=FALSE)
coordinates(markers.spdf) = ~midlong+midlat
proj4string(markers.spdf) <- CRS(latlong)

icons <- makeAwesomeIcon(icon = "ios-close", library = "glyphicon",
                         markerColor = "blue", iconColor = "white", iconRotate = 0,
                         fontFamily = "monospace", text = NULL)

#New Testing
m <- leaflet()
m %>% addTiles() %>%
  # addMarkers(data = orig.spdf, popup = ~popupw, group = "Origin") %>%
  # addMarkers(data = found.spdf, popup = ~popupw, group = "Found",clusterOptions = markerClusterOptions()) %>%
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
  layerId = koop,
  popup = popupArgs(showTitle = F, showValues = T, labels = NULL,
  supValues = NULL, supLabels = colnames(elop.map), html = "popupw",
  noPopup = T, digits = NULL)
)%>% # end add flows
  # plotArrows(lines.connections(), fraction=0.9, length=0.15, first='', add=FALSE)
  addPolylines(data = complete.lines, popup = ~popupw, group = "Connections", label =  "<   ", 
               labelOptions = labelOptions(noHide = T, textsize = "10px"))
  # addAwesomeMarkers(data = markers.spdf,group="Connections", icon=icons)
  # addMarkers(data = markers.spdf,group="Connections")


  


# plotArrows(complete.lines, fraction=0.9, length=0.15, first='', add=FALSE)



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



