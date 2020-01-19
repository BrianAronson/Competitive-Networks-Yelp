#' 13 - Create Distance Adjacency Lists
#' @description This function returns a vector of the distances between each business, ordered id1, id2
#' @export
#' @import data.table
#' @import geosphere

yelp_Distance_Adj_Function <- function(Directory, City){
    #0 - Set Directory
        setwd(Directory)
    
    #1 - Read Data
        Bus <- readRDS("Restaurants2.rds")
    
    #2 - Create temporary dataframe consisting of just the businesses in the specified city.
        Bus2 <- Bus[Bus$city_super==City, ]
    
    #3 - Estimate physical distance between each business (super efficient method)
        a  <-  cbind(Bus2$longitude, Bus2$latitude)
        temp <- c(distm(a))
        
    #4 - convert to miles
        Dist <- temp*0.000621371
    
    #5 - Estimate physical distance to city center
        citycenter <- data.frame(longitude=median(Bus2$longitude), latitude=median(Bus2$latitude))
        locations <- data.frame(longitude=(Bus2$longitude), latitude=(Bus2$latitude))
        a <- as.vector(distm(citycenter, locations)*0.000621371)
        if(!("dist_center" %in% names(Bus))){
          Bus$dist_center <- NA
        }
        Bus$dist_center[Bus$city_super==City] <- a
        
    #6 - Save
        saveRDS(Dist, paste(City, "_dist.rds", sep=""))
        saveRDS(Bus, "Restaurants2.rds")

}


