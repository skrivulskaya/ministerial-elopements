rm(list=ls(all=TRUE)) # clear memory

packages<- c("maptools","rgdal","leaflet","htmlwidgets","shiny","ggmap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("E:/GIT_Checkouts/R_Scripts/ministerial-elopements")
latlong <- "+init=epsg:4326"


geocode.it <- FALSE

if (geocode.it){
  #NOTE: This is a fantastically stupid way of doing this. Someday I will change it to a function and make it pretty
  
  elop.raw <- read.csv("ministerial_elopements.csv",stringsAsFactors = F)
  elop.raw$Acc_Found<-NA
  elop.raw$Longtitude_Found<-NA
  elop.raw$Latitude_Found<-NA
  for(i in 1:nrow(elop.raw)){
    if (is.na(elop.raw$Acc_Origin[i])){ #uses the accuracy variable as a check if it has been done
      result <- tryCatch({
        #Try this, if it fails because of something on Googles end, do the other stuff
        geocode(elop.raw$Location_Origin[i], output = "more", source = "google")
      },warning=function(w){#in case of a warning     
        result$lon <- NA
        result$lat <- NA
        result$loctype <- "FAILED"
        return(result)
      }, error = function(e) { #in case of an error     
        result$lon <- NA
        result$lat <- NA
        result$loctype <- "FAILED"
        return(result)
      }#end error
      
      )#end trycatch
      #Then give it all the values
      elop.raw$Longtitude_Origin[i] <- as.numeric(result$lon)
      elop.raw$Latitude_Origin[i] <- as.numeric(result$lat)
      elop.raw$Acc_Origin[i] <- as.character(result$loctype)
    }#end of the if statement for origin geocoding
    if (is.na(elop.raw$Acc_Found[i])&elop.raw$Location_Found != ""){ #beginging of if statement for destination geocoding
          result <- tryCatch({
            #Try this, if it fails because of something on Googles end, do the other stuff
                  geocode(elop.raw$Location_Found[i], output = "more", source = "google")
            },warning=function(w){#in case of a warning     
                  result$lon <- NA
                  result$lat <- NA
                  result$loctype <- "FAILED"
                  return(result)
            }, error = function(e) { #in case of an error     
                  result$lon <- NA
                  result$lat <- NA
                  result$loctype <- "FAILED"
                  return(result)
            }#end error
          
          )#end trycatch
      #Then give it all the values
      elop.raw$Longtitude_Found[i] <- as.numeric(result$lon)
      elop.raw$Latitude_Found[i] <- as.numeric(result$lat)
      elop.raw$Acc_Found[i] <- as.character(result$loctype)
    }#end of the if statement
    
    
    
  }#end of the for loop
  if (!file.exists("ministerial_elopements_geocoded.csv")){
    write.csv(elop.raw,"ministerial_elopements_geocoded.csv", row.names = F)
  }# end if statement in output
}else{ #else if already geocoded, just read the saved version
  elop.raw <- read.csv("ministerial_elopements_geocoded.csv",stringsAsFactors = F)
} 



