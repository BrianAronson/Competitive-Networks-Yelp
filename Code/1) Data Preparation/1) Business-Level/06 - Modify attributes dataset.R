#' 6 - Modify and Merge attributes
#' @description This function modifies and merges the attributes dataset into the restaurants dataset, removing the previous attributes variable within it
#' @param Directory Directory of files.
#' @export
#' @import data.table mice
#' @details 
#' This script does the following:
#' 0 - Set Directory  
#' 1 - Read Data
#' 2 - Reformat attributes variables to better classes
#' 3 - Delete categorical variables with absurd rates of missingness
#' 4 - Merge Attributes into restaurants dataset; remove former attributes variable
#' 5 - Save RDS

yelp_Merge_Attributes <- function(Directory){
    #0 - Set Directory
        setwd(Directory)

    #1 - Read Data
        Attr <- readRDS("Attributes.rds")
        Bus <- readRDS("Restaurants.rds")
  
    #2 - Reformat attributes variables to better classes
        Attr$WiFi <- ifelse(Attr$WiFi=="paid", "free", Attr$WiFi)
        Attr$beer_and_wine <- ifelse(Attr$Alcohol=="beer_and_wine", T, F)
        Attr$full_bar <- ifelse(Attr$Alcohol=="full_bar", T, F)
        Attr$Alcohol <- NULL
        Attr$NoiseLevel <- as.integer(ifelse(Attr$NoiseLevel=="quiet", 1, ifelse(Attr$NoiseLevel=="average", 2, ifelse(Attr$NoiseLevel=="loud", 3, ifelse(Attr$NoiseLevel=="very_loud", 4, Attr$NoiseLevel)))))
        Attr$Price <- as.integer(Attr$RestaurantsPriceRange2)
        Attr$RestaurantsPriceRange2 <- NULL
        Attr$CasualAttire <- ifelse(Attr$RestaurantsAttire=="casual", T, F)
        Attr$RestaurantsAttire <- NULL
        Attr$WiFi <- ifelse(Attr$WiFi=="free", T, F)
        Attr$Smoking <- ifelse(Attr$Smoking=="no" | Attr$Smoking=="outdoor", F, T)
        
        # Convert all character variables to binary
          kill <- vector()
          for(i in 1:length(Attr)){
            if(length(table(Attr[, i]))==0){
              kill[i] <- T
              next()
            }
              kill[i] <- F
            if(names(table(Attr[, i]))[1]=="False"){
              Attr[, i] <- as.logical(Attr[, i])
            }
          }
        Attr[, kill] <- NULL

    #3 - Delete categorical variables with absurd rates of missingness
        KeepAttribute <- vector()
        meanAttribute <- vector()
        for(i in 1:length(Attr)){
          meanAttribute[i] <- mean(!is.na(Attr[, i]))    
          KeepAttribute[i] <- mean(!is.na(Attr[, i]))>.1
        }
        Attr <- Attr[, KeepAttribute]
        Attr$no_music <- NULL
        
        
    #4 - Merge Attributes into restaurants dataset; remove former attributes variable
        Bus$attributes <- NULL
        Bus <- cbind(Bus, Attr)  
        
    #5 - SaveRDS
        saveRDS(Bus, "Restaurants.rds")
      
}
