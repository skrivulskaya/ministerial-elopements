rm(list=ls(all=TRUE)) # clear memory

packages<- c("maptools","rgdal","leaflet","htmlwidgets","shiny","ggmap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("E:/GIT_Checkouts/R_Scripts/ministerial-elopements")




elop.raw <- read.csv("ministerial_elopements.csv",stringsAsFactors = F)
elop.ba


for(i in 1:nrow(elop.raw)){
  if (is.na(elop.raw$Acc_Origin[i])){
    result <- tryCatch({
      geocode(elop.raw$Location_Origin[i], output = "more", source = "google")
    },warning=function(w){     
        result$lon <- NA
        result$lat <- NA
        result$loctype <- "FAILED"
        return(result)
    }, error = function(e) {      
        result$lon <- NA
        result$lat <- NA
        result$loctype <- "FAILED"
        return(result)
    }
      
    )
      
    elop.raw$Longtitude_Origin[i] <- as.numeric(result$lon)
    elop.raw$Latitude_Origin[i] <- as.numeric(result$lat)
    elop.raw$Acc_Origin[i] <- as.character(result$loctype)
  }
  
}

for(i in 1:nrow(elop.raw)){
  if (!is.na(elop.raw$Location_Found[i])){
    result <- tryCgeocode(elop.raw$Location_Origin[i], output = "more", source = "google")
    elop.raw$Longtitude_Origin[i] <- as.numeric(result$lon)
    elop.raw$Latitude_Origin[i] <- as.numeric(result$lat)
    elop.raw$Acc_Origin[i] <- as.character(result$loctype)
    Sys.sleep(.2)
  }
  
}