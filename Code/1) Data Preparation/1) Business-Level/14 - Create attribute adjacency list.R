#' 14 - Create attribute adjacency list
#' @description This function returns a vector of similarity among all restaurants
#' @export
#' @import data.table
#' @import cluster
#' @details 
#' This script does the following:
#' 0 - Set Directory  
#' 1 - Read Data
#' 2 - Create temporary dataframe consisting of just the businesses in the specified city.
#' 3 - Remove potentially confounding variables
#' 4 - Create index of potentially unimportant variables.
#' 5 - Add other potentially useful variables
#' 6 - Convert characters and logicals to factor for gower function.
#' 7 - Create reduced weight for potentially unimportant variables
#' 8 - Find gower distances 
#' 9 - Convert to adj vector
#' 10 - Save results

yelp_Attribute_Adj_Function <- function(Directory, City){

    #0 - Set Directory
        setwd(Directory)
    
    #1 - Read Data
        Bus <- readRDS("Restaurants2.rds")
        
    #2 - Create temporary dataframe consisting of just the businesses in the specified city.
        Bus2 <- Bus[Bus$city_super==City, ]
        Bus4 <- Bus2
    
    #3 - Remove variables
        #confounding
            Bus2[, c("address", "Alive2004", "Alive2005", "Alive2006", "Alive2007", "Alive2008", "Alive2009", "Alive2010", "Alive2011", "Alive2012", "Alive2013", "Alive2014", "Alive2015", "attributes", "cat_super", "city_super", "date_close", "date_open", "Hour_FridayClose", "Hour_FridayOpen", "Hour_MondayClose", "Hour_MondayOpen", "Hour_SaturdayClose", "Hour_SaturdayOpen", "Hour_SundayClose", "Hour_SundayOpen", "Hour_ThursdayClose", "Hour_ThursdayOpen", "Hour_TuesdayClose", "Hour_TuesdayOpen", "Hour_WednesdayClose", "Hour_WednesdayOpen", "id", "is_open", "latitude", "logged_rev_count", "longitude", "name", "neighborhood", "pop", "pop_adj", "postal_code", "rev_count", "rev_count_nonelites", "revers", "stars", "state", "type", "cat", "age", "chain_freq", "city", "Restaurants")] <- NULL
        #meaningless
            meaningless <- vector()
            for(i in 1:length(Bus2)){
                meaningless[i] <- all(duplicated(Bus2[, i])[2:nrow(Bus2)])
                
                # TEMPORARY PARTIAL FIX = change the above to exclude NAs.
                meaningless[i] <- all(duplicated(Bus2[, i][!is.na(Bus2[, i])])[2:nrow(Bus2[!is.na(Bus2[, i]), ])])
            }
            removed <- names(Bus2)[meaningless]
            Bus2 <- Bus2[, which(c(!meaningless))]
        #unimportant variables.
            unim <- which(names(Bus2) %in% c("background_music", "BikeParking", "breakfast", "brunch", "BusinessAcceptsCreditCards", "casual", "CasualAttire", "Caters", "city", "classy", "CoatCheck", "dessert", "dinner", "divey", "dj", "DogsAllowed", "garage", "GoodForDancing", "GoodForKids", "HappyHour", "HasTV", "hipster", "intimate", "jukebox", "karaoke", "latenight", "live", "lot", "lunch", "OutdoorSeating", "RestaurantsGoodForGroups", "romantic", "street", "touristy", "trendy", "upscale", "valet", "validated", "video", "WheelchairAccessible", "WiFi", "RestaurantsTakeOut", "NoiseLevel", "Hour_Open", "Hour_Close", "beer_and_wine", "dist_center"))
            Bus2[, c(unim)] <- NULL
            Bus2$chain_super
  
    #4 - Convert characters and logicals to factor for gower function.
        for(i in 1:length(Bus2))  {
            if(class(Bus2[, i])=="character"){
                Bus2[, i] <- as.factor(Bus2[, i])
            }
            if(class(Bus2[, i])=="logical"){
                Bus2[, i] <- as.numeric(ifelse(Bus2[, i]==T, 1, 0))
            }
        }
        # Set categories to factors
            temp <- 10:(length(Bus2))
            for(i in 1:length(temp)){
                Bus2[, temp[i]] <- as.factor(Bus2[, temp[i]])
            }

    #5 - Alter variables
          Bus2$PriceHigh <- ifelse(Bus2$Price>2, 1, 0)
          Bus2$PriceHigh <- factor(Bus2$PriceHigh)
          Bus2$RestaurantsDelivery[Bus2$RestaurantsDelivery<0] <- 0
          Bus2$RestaurantsDelivery <- as.factor(round(Bus2$RestaurantsDelivery, 0))
          Bus2$RestaurantsReservations <- as.factor(round(Bus2$RestaurantsReservations, 0))
          Bus2$DriveThru <- ifelse(Bus2$DriveThru<0, 0, Bus2$DriveThru)
          Bus2$DriveThru <- as.factor(round(Bus2$DriveThru, 0))
          Bus2$RestaurantsTableService[Bus2$RestaurantsTableService>1] <- 1
          Bus2$RestaurantsTableService <- as.factor(round(Bus2$RestaurantsTableService, 0))
          Bus2$full_bar <- as.factor(round(Bus2$full_bar, 0))
          Bus2$chain <- as.factor(round(Bus2$chain, 0))
          Bus2$chain_super <- as.factor(round(as.numeric(as.character(Bus2$chain_super)), 0))

    #6 - Bring in new review categories from review dataset
        #load and prep data        
            propmat <- readRDS("propmat.rds")
            vars <- readRDS("vars.rds")
            propmat2 <- propmat[Bus$city_super==City, ]
            #normalize propmats  
                propalt <- propmat2
                propalt <- as.data.frame(sapply(propalt, function(x) ifelse(x>.1, 1, ifelse(x>.05, .66, ifelse(x>0, .1, 0)))))
            #kill super uncommon info
                temp <- sapply(propalt, function(x) sum(x, na.rm = T))
                propalt <- propalt[, temp>(nrow(Bus2)/5000)]
                
    #7 - Remove rows with no useful information
          df1 <- vector()
          df2 <- vector()
          for(i in 1:length(Bus2)){
            df1[i] <- length(unique(Bus2[, i]))
            df2[i] <- length(unique(Bus4[, i]))
          }        
          Bus2 <- Bus2[, df1!=1]
          Bus4 <- Bus4[, df2!=1]
                       
    #8 - normalize/duplicate category info of chains     
         modefunc  <-  function(x){
            tabresult  <-  table(x)
            themode  <-  names(which(tabresult == max(tabresult)))
            if(sum(tabresult == max(tabresult))>1) themode  <-  "1"
            return(themode)
         }
         temppropalt <- propalt
        #for each business 
            for(i in 1:nrow(Bus2)){
              #if the business is a chain and others exist in the city
                  if(Bus4$chain_id[i]!="0" & sum(Bus4$chain_id==Bus4$chain_id[i])>1){
                    #find places with same chainid
                        simplaces <- which(Bus4$chain_id[i]==Bus4$chain_id)
                    #replace business's propalt with mean propalt of simplaces
                        propalt[i, ] <- colMeans(propalt[simplaces, ])
                    #replace business's bus2 with mode bus2 of simplaces
                        temp <- unlist(sapply(Bus2[simplaces, ], modefunc))
                        Bus2[i, ] <- temp
                  }
              setTxtProgressBar(txtProgressBar(max = nrow(Bus2), style = 3), i)
            }       
         #error just refers to id variable
    
    #9 - Deal with places that have barely any reviews
        #run two similarity matrix just based on category info
            temp <- Bus2[, 11:length(Bus2)-1]
            gower_mattemp <- as.matrix(daisy(temp, metric = "gower"))
            gower_mattemp2 <- as.matrix(daisy(temp, metric = "gower", type=list(asymm=3:length(temp))))

        #Find out if just named "restaurants, food or nightlife"
            catlist <- Bus4$cat
            catlist <- sapply(catlist, setdiff, "Restaurants" )
            catlist <- sapply(catlist, setdiff, "Nightlife" )
            catlist <- sapply(catlist, setdiff, "Food" )
            catnum <- vector()
            for(i in 1:length(catlist)){
                catnum[i] <- length(catlist[[i]])
            }
        #for each business 
            for(i in 1:nrow(Bus2)){
              #if the business has less than 6 reviews and more than 1 category and is not a chain
                  if(Bus4$rev_count[i]<6 & catnum[i]>0 & Bus4$chain_id[i]=="0"){
                      #find places that have very similar category info
                          simplaces <- which(gower_mattemp[i, ]<=quantile(gower_mattemp[i, ], .01))
                      #remove simplaces that are just categorized as restaurants food or nightlife
                          simplaces <- simplaces[catnum[simplaces]!=0]
                      #remove places that do not match on the second gower matrix
                          simplaces <- simplaces[simplaces %in% which(gower_mattemp2[i, ]<1)]
                      #remove places with less than 5 reviews
                          simplaces <- simplaces[Bus4$rev_count[simplaces]>5]
                      #get mean info on propalt of simplaces
                          temp <- colMeans(propalt[simplaces, ])
                      #Average restaurant's propalt (slightly) with means in simplaces
                          temp <- (temp*4+ propalt[i, ])/5
                      #replace business's info with this new info
                          temppropalt[i, ] <- temp
                  }
              setTxtProgressBar(txtProgressBar(max = nrow(Bus2), style = 3), i)
            }
        #Restore price to numeric
            Bus2[, which(names(Bus2)=="Price")] <- as.numeric(Bus2[, which(names(Bus2)=="Price")])
        #kill chainid 
            Bus2$chain_id <- NULL
        #finalize replacements
            propalt <- temppropalt
            
    #10 - merge business and propalt info
        Bus3 <- cbind(Bus2, propalt)
  
    #11 - weight data, first by general principles, then custom concepts
        substrRight  <-  function(x, n){
          substr(x, nchar(x)-n+1, nchar(x))
        }
        weights <- c(rep(.1, length(Bus2)), rep(1, sum(substrRight(names(propalt), 1)=="1")), rep(.5, sum(substrRight(names(propalt), 1)=="2")), rep(1, length(intersect(vars$definedvars, names(propalt)))), rep(.5, length(vars$attributevars)), rep(.5, length(vars$customvars)))
        
        #reliable/useful category info
            weights[1:8] <- .5
            #weights[which(names(Bus3)=="chain_id")] <- 1 #probably unecessary; slighty screws up adjacency among dissimilar fast food places; makes them hyper specialized
            weights[which(names(Bus3)=="chain_super")] <- .5
            weights[which(names(Bus3)=="chain")] <- 1
          #unweight crappy vars
            weights[which(names(Bus3)=="lunch")] <- .1
            weights[which(names(Bus3)=="dinner")] <- .1
            weights[which(names(Bus3)=="Salad2")] <- .1
            weights[which(names(Bus3)=="Soup2")] <- .1
            weights[which(names(Bus3)=="vegetable")] <- .1
            weights[which(names(Bus3)=="Sandwich2")] <- .25
            weights[which(names(Bus3)=="breakfast")] <- .0 #redundant
            weights[which(names(Bus3)=="steak")] <- .1
            weights[which(names(Bus3)=="fish")] <- .1
            weights[which(names(Bus3)=="raw")] <- .25
            weights[which(names(Bus3)=="cocktail")] <- .25
            weights[which(names(Bus3)=="breakfast2")] <- .25
            weights[which(names(Bus3)=="american2")] <- .25
            weights[which(names(Bus3)=="chain")] <- .25
            weights[which(names(Bus3)=="expensive")] <- .25
            weights[which(names(Bus3)=="cheap")] <- .1
            weights[which(names(Bus3)=="Dessert2")] <- .1
            weights[which(names(Bus3)=="French2")] <- .1
            weights[which(names(Bus3)=="kids")] <- .1
            weights[which(names(Bus3)=="local")] <- .1
            weights[which(names(Bus3)=="fruit")] <- .1
            weights[which(names(Bus3)=="ice cream")] <- .1
            weights[which(names(Bus3)=="parking lot")] <- .1
            weights[which(names(Bus3)=="jazz")] <- .25
            weights[which(names(Bus3)=="sports")] <- .25
            weights[which(names(Bus3)=="late")] <- .25
            weights[which(names(Bus3)=="Restaurants")] <- .5
            #paste(weights, names(Bus3))
            
    #12 - Make most of original bus cats asymetric
        Bus2$Smoking <- (round(as.numeric(as.character(Bus2$Smoking))))
        Bus2$Smoking[Bus2$Smoking<0] <- 0
        Bus2$Smoking <- factor(Bus2$Smoking)
        a <- sapply(Bus2, function(x) length(unique(x)))
        tempasym <- c(which(a==2))
        
    #13 - Kill any remaining businesses of non interest
        #fast food/drive through chains
            tokill <- unique(Bus4$name[Bus3$`Fast Food`==1 & Bus4$chain_freq>20 & Bus3$DriveThru==1])
            kill1 <- ifelse(Bus4$name %in% tokill, 1, 0)
        #places that are too isolated (no places within .5 miles, only a few places within 3 miles)
            Dist <- readRDS(paste(City, "_dist.rds", sep=""))
            DT <- data.table(Dist=Dist, Bus1=rep(Bus4$id, each=nrow(Bus4)), Bus2=rep(Bus4$id, nrow(Bus4)))
            DT <- DT[DT$Dist<3, ]
            kill2 <- DT[, .(sums=.N), by=Bus1]
            DT <- DT[DT$Dist<.5, ]
            kill3 <- DT[, .(sums=.N), by=Bus1]
            kill <- ifelse(kill1==1 | kill2$sums<20 | kill3$sums==1, 1, 0)
            killids <- Bus4$id[kill==1]
        #reduce the business data frame and the distance data frame
            DT <- data.table(Dist=Dist, Bus1=rep(Bus4$id, each=nrow(Bus4)), Bus2=rep(Bus4$id, nrow(Bus4)))
            DT <- DT[!(DT$Bus1 %in% killids) & !(DT$Bus2 %in% killids), ]
            Dist <- DT$Dist
            
            Bus <- readRDS("Restaurants.rds")
            Bus <- Bus[!(Bus$id %in% killids), ]
            Bus3 <- Bus3[!(Bus4$id %in% killids), ]
            Bus4 <- Bus4[!(Bus4$id %in% killids), ]
            
            a <- sapply(Bus3, function(x) length(unique(x)))

        #for champaign, fix weights by reducing to length of bus3
            if(length(weights)>length(Bus3)){
              weights <- weights[1:length(Bus3)]
            }
            Bus3$Smoking <- as.numeric(Bus3$Smoking)            
            
    #14 - Find gower distances 
        gower_mat  <-  as.matrix(daisy(Bus3, metric = "gower", weights=weights)) #

    #15 - Convert to adj vector
        temp <- c(gower_mat)
         
    #16 - Save results
        saveRDS(Bus, "Restaurants.rds")
        saveRDS(Dist, paste(Directory, City, "_dist.rds", sep=""))
        saveRDS(temp, paste(Directory, City, "_atr.rds", sep=""))
        Bus3$id <- Bus4$id
        saveRDS(Bus3, paste(Directory, City, "_gowerinput.rds", sep=""))
}


