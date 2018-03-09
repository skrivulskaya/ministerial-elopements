rm(list=ls(all=TRUE)) # clear memory


#MLS Todo: 
#Add Arrowheads to main map. 
  #Deal with multiple people in same place (randomize points). 
  #Find a good symbol for people that are found in the same city
# classification direction

packages<- c("maptools","rgdal","leaflet","htmlwidgets","shiny","ggmap","rsconnect", "leaflet.minicharts") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
# setwd("/home/matthew/GIT/R_Scripts/ministerial-elopements")
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
# lines <- list()
# for (i in 1:nrow(elop.comp)) { 
#   lines[[i]] <- Lines(list(Line(rbind(c(elop.comp$Longtitude_Origin[i],elop.comp$Latitude_Origin[i]), c(elop.comp$Longtitude_Found[i], elop.comp$Latitude_Found[i]) ))), as.character(i)) 
#   #print(i)
# }
# complete.lines <- SpatialLinesDataFrame(SpatialLines(lines),elop.comp)

a <- (gcIntermediate(elop.comp[,c("Longtitude_Origin","Latitude_Origin")], elop.comp[,c("Longtitude_Found","Latitude_Found")],addStartEnd = T, breakAtDateLine = T, n=150, sp = T) )
complete.lines <- SpatialLinesDataFrame(a,elop.comp)

negs <- as.matrix(coordinates(complete.lines[94,])[[1]][[2]])
negs[,1] <- (negs[,1])-360
complete.lines@lines[[94]]@Lines[[2]]@coords[] <- negs



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

markers.spdf <- elop.map[,c("Location_Origin","Latitude_Origin","Longtitude_Origin","Longtitude_Found","Latitude_Found","Location_Found")]


markers.spdf$midlong <- apply(markers.spdf[,c("Longtitude_Origin","Longtitude_Found")], 1, mean) 
markers.spdf$midlat <- apply(markers.spdf[,c("Latitude_Origin","Latitude_Found")], 1, mean) 
a <- (gcIntermediate(markers.spdf[,c("Longtitude_Origin","Latitude_Origin")], markers.spdf[,c("Longtitude_Found","Latitude_Found")], n=1) )
a <- do.call(rbind.data.frame, a)
markers.spdf$midlong <- a$lon
markers.spdf$midlat <- a$lat


library (geosphere)
library(maptools)
markers.spdf$bearing <- bearingRhumb(markers.spdf[,c("Longtitude_Origin","Latitude_Origin")],markers.spdf[,c("Longtitude_Found","Latitude_Found")])
#Now can calculate the direction of travel



# coordinates(markers.spdf)=midPoint(elop.comp[,c("Longtitude_Origin", "Latitude_Origin")],elop.comp[,c("Longtitude_Found", "Latitude_Found")],f=0)
# coordinates(markers.spdf)=gcIntermediate(elop.comp[,c("Longtitude_Origin", "Latitude_Origin")],elop.comp[,c("Longtitude_Found", "Latitude_Found")], n=1, breakAtDateLine=FALSE, addStartEnd=FALSE, sp=FALSE)
arrow.length <- 60000
arrow.angle <- 30

a <- data.frame(destPoint(markers.spdf[,c( "midlong","midlat")], d = arrow.length, b = markers.spdf$bearing + arrow.angle -180 ))
markers.spdf$arrow1Lat <- a$lat
markers.spdf$arrow1Lon <- a$lon

a <- data.frame(destPoint(markers.spdf[,c( "midlong","midlat")], d = arrow.length, b = markers.spdf$bearing - arrow.angle-180 ))
markers.spdf$arrow2Lat <- a$lat
markers.spdf$arrow2Lon <- a$lon

# lines2 <- list()
# for (i in 1:nrow(markers.spdf)) { 
#   lines2[[i]] <- Lines(list(Line(rbind(c(markers.spdf$midlong[i],markers.spdf$midlat[i]), c(markers.spdf$arrow1Lon[i], markers.spdf$arrow1Lat[i]) ))), as.character(i)) 
#   #print(i)
# }
# row.names(markers.spdf) <- NULL
# arrow01.lines <- SpatialLinesDataFrame(SpatialLines(lines2),markers.spdf)
# proj4string(arrow01.lines) <- CRS(latlong)
# 
# 
# lines3 <- list()
# for (i in 1:nrow(markers.spdf)) { 
#   lines3[[i]] <- Lines(list(Line(rbind(c(markers.spdf$midlong[i],markers.spdf$midlat[i]), c(markers.spdf$arrow2Lon[i], markers.spdf$arrow2Lat[i]) ))), as.character(i)) 
#   #print(i)
# }
row.names(markers.spdf) <- NULL
polys <- list()
for (i in 1:nrow(markers.spdf)) { 
  # p = Polygon(rbind(c(markers.spdf$midlong[i],markers.spdf$midlat[i]), c(markers.spdf$arrow2Lon[i], markers.spdf$arrow2Lat[i]), c(markers.spdf$arrow1Lon[i], markers.spdf$arrow1Lat[i]),c(markers.spdf$midlong[i],markers.spdf$midlat[i])))
  polys[[i]] = Polygons(list(Polygon(rbind(c(markers.spdf$midlong[i],markers.spdf$midlat[i]), c(markers.spdf$arrow2Lon[i], markers.spdf$arrow2Lat[i]), c(markers.spdf$arrow1Lon[i], markers.spdf$arrow1Lat[i]),c(markers.spdf$midlong[i],markers.spdf$midlat[i])))), as.character(i))
  # polys[[i]] = SpatialPolygons(list(ps))
}
poly.arrows <- SpatialPolygonsDataFrame(SpatialPolygons(polys),markers.spdf)
proj4string(poly.arrows) <- CRS(latlong)



# arrow02.lines <- SpatialLinesDataFrame(SpatialLines(lines3),markers.spdf)
# proj4string(arrow02.lines) <- CRS(latlong)

# complete.lines$fdfdf <- row.names(complete.lines)

m<-leaflet() %>%
  addTiles() %>%
  addPolylines(data = (complete.lines), popup = ~popupw, group = "Connections", opacity = 1) %>%
  # addPolylines(data = (arrow01.lines), group = "Connections", color = "green")%>%
  # addPolylines(data = (arrow02.lines), group = "Connections", color = "red") %>%
  addPolygons(data=poly.arrows, group = "Connections",  fillOpacity = 1, opacity = 1, popup = ~popupw )

m
# 
# 
# 
# #New Testing
# m <- leaflet()
# m<- leaflet() %>% addTiles() %>%
#   # addMarkers(data = orig.spdf, popup = ~popupw, group = "Origin") %>%
#   # addMarkers(data = found.spdf, popup = ~popupw, group = "Found",clusterOptions = markerClusterOptions()) %>%
#   # addCircleMarkers(data = same.spdf, popup = ~popupw, group = "Connections",color = "navy",radius=5) %>%
#   
#   addLayersControl(
#     overlayGroups = c("Origin", "Found","Connections"),
#     options = layersControlOptions(collapsed = FALSE)
#   )%>%
# addFlows(
#   elop.map$Longtitude_Origin, elop.map$Latitude_Origin, elop.map$Longtitude_Found, elop.map$Latitude_Found,
#   # flow = .01,
#   maxThickness = 2,
#   color = "navy",
#   # layerId = koop,
#   popup = popupArgs(showTitle = F, showValues = T, labels = NULL,
#   supValues = NULL, supLabels = colnames(elop.map), html = "popupw",
#   noPopup = T, digits = NULL)
# )%>% # end add flows
#   # plotArrows(lines.connections(), fraction=0.9, length=0.15, first='', add=FALSE)
#   addPolylines(data = complete.lines, popup = ~popupw, group = "Connections")
#   # addAwesomeMarkers(data = markers.spdf,group="Connections", icon=icons)
#   # addMarkers(data = markers.spdf,group="Connections")
# 
# a <- addFlows(m,
#   elop.map$Longtitude_Origin, elop.map$Latitude_Origin, elop.map$Longtitude_Found, elop.map$Latitude_Found,
#   # flow = .01,
#   maxThickness = 2,
#   color = "navy",
#   # layerId = koop,
#   popup = popupArgs(showTitle = F, showValues = T, labels = NULL,
#                     supValues = NULL, supLabels = colnames(elop.map), html = "popupw",
#                     noPopup = T, digits = NULL)
# )
# #Crazy attempts
# 
# 
# library(htmltools)
# library(htmlwidgets)
# library(leaflet)
# 
# # this is taken from: https://gist.github.com/jcheng5/c084a59717f18e947a17955007dc5f92
# rotatedMarker <- htmlDependency(
#   "Leaflet.rotatedMarker",
#   "0.1.2",
#   src = normalizePath("."),
#   script = "extended\\leaflet.rotatedMarker.js"
# )
# fancyPolyline <- htmlDependency(
#   "leaflet.polylineDecorator",
#   "99.99.99",
#   src = normalizePath("."),
#   script = "extended\\leaflet.polylineDecorator.js"
# )
# 
# 
# 
# # this is taken from: https://gist.github.com/jcheng5/c084a59717f18e947a17955007dc5f92
# registerPlugin <- function(map, plugin) {
#   map$dependencies <- c(map$dependencies, list(plugin))
#   map
# }
# 
# a <- leaflet() %>%
#   addTiles() %>%
#   # setView(127.074167,34.456806, zoom = 9) %>%
#   registerPlugin(rotatedMarker) %>%
#   registerPlugin(fancyPolyline) %>%
#   onRender("function(el, x) {
#            var mymap = this;
# 
#            var pathPattern = L.marker([34.45, 127.07], {rotationAngle: 12}).addTo(this);
#            var arrow = L.polyline([[57, -19], [60, -12]], {}).addTo(mymap);
#            var arrowHead = L.polylineDecorator(arrow, {
#            patterns: [
#            {offset: '100%', repeat: 0}
#            ]
#            }).addTo(mymap);
#            }")%>%
# 
#   addPolylines(data = complete.lines, popup = ~popupw, group = "Connections",  opacity = 1)%>%
#   addCircles(lng = 127.07, lat = 34.45, weight = 10, radius = 100)%>%
#   addAwesomeMarkers(lng = 10, lat = 12, {iconRotate = 90})
#   
# a
# 
