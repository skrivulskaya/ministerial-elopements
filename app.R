
packages<- c("rgdal","leaflet","htmlwidgets","shiny","ggmap") # list the packages that you'll need
library(rgdal)
library(leaflet)
library(shiny)
library(ggmap)


# setwd("E:/GIT_Checkouts/R_Scripts/ministerial-elopements")
latlong <- "+init=epsg:4326"


#Loading the geocoded data
elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)



elop.raw$popupw <- paste(sep = "",  "<b>",elop.raw$Full_Name,"</b><br/>",
                         "Name = ",elop.raw$Accusations,"<br/>",
                         "Origin = ",elop.raw$Location_Origin,"<br/>",
                         "Found = ",elop.raw$Location_Found,"<br/>"
) #A bit of HTML To make the popups on the lines


#Creating the Line objects out of the point objects:
elop.comp <- elop.raw[which(!is.na(elop.raw$Latitude_Found)),]
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
  #             c("all", "1870s","1880s","1890s","1900s","1910-1914")),
  selectizeInput("denomination", "Denomination:",
                 choices = c("all", sort(unique(elop.raw$Denomination_for_Tableau))))
)

server <- function(input, output, session) {
  
  points <- eventReactive(c(input$range, input$denomination), {
    if (input$denomination == "all"){
      orig.spdf[which(orig.spdf$Year >= input$range[1] & orig.spdf$Year <= input$range[2]),]
    }else{
      orig.spdf[which(orig.spdf$Year >= input$range[1] & orig.spdf$Year <= input$range[2] & orig.spdf$Denomination_for_Tableau == input$denomination),]  
    }
    
    
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      fitBounds(-129,24.2,-65.58,50.54)%>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) 
    # %>% addMarkers(data = points())
  })
  observe({
    leafletProxy("mymap") %>%
      clearMarkers() %>%  
      addMarkers(data = points())
  })
}

shinyApp(ui, server)
