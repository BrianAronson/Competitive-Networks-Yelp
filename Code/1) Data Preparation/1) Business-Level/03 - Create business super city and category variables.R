#' 3 - Create business super category, city variables, and alive variables
#' @description This function creates the key business super city and category variables, as well as the alive variables the business dataset. Specifically, it creates a variable for super category (cat_super), super city (city_super), and alive (Alive_2004 - Alive_2017). It also renames the category variable to "cat"
#' @param Directory Directory of files.
#' @param Parallel Specify cores if you want to do parallel processing. If not, will run with normal if statement.
#' @export
#' @import data.table
#' @import Imap
#' @import parallel
#' @details 
#' This script does the following:
#' 0 - Set Directory  
#' 1 - Register cores      
#' 2 - Read Data
#' 3 - Sort Revs and Bus by business_id and converts to data.table
#' 4 - Create variables within Bus for business review counts (rev_count), business reviewers (revers), date opened (date_open), date closed (date_close), life span (age), and popularity (pop)
#' 5 - Create variable within Bus for number of reviewers by non elites (rev_count_nonelites)
#' 6 - Save RDS

yelp_Super_Variables  <- function(Directory, Parallel=F){
    #0 - Set Directory
        setwd(Directory)
  
    #1 - Use all cores if parallel
        if(Parallel){
          Cores=detectCores()
          registerDoParallel(cores=Cores)
        }
        
    #2 - Read Data
        Bus <- readRDS("BusinessData.rds")
        Bus$categories <- Bus$cat
      
    #3 - Create variable for super cat information; delete businesses without super categories.
        #Split categories 
            Bus$categories <- str_split(Bus$categories, ", ")
            Bus$categories <- sapply(Bus$categories, function(x) gsub(".*? (.+)", "\\1", x))
        #Remove businesses without any cat
            temp <- unlist(lapply(Bus$categories, length))
            Bus <- Bus[temp!=0, ]
        #Determine business supercategories
            catlookup <- function(x, y){
             ifelse(sum(match(x, y, nomatch=0))==1, y, 0)
            }
            supercats <- c("Restaurants", "Shopping", "Food", "Beauty & Spas", "Nightlife", "Health & Medical", "Automotive", "Home Services", "Active Life", "Event Planning & Services", "Local Services", "Hotels & Travel", "Arts & Entertainment", "Pets", "Real Estate", "Professional Services", "Financial Services", "Education", "Public Services & Government", "Local Flavor", "Religious Organizations", "Mass Media")
            tempdataframe <- data.frame(matrix(NA, nrow=length(Bus$business_id), ncol=length(supercats)))
        #Below separated based on whether I select to use parallel processing. (4 cores shaves 40% off time)
            if(Cores==1){
                #make dataframe where each column represents a different cat; enter 0 if a business is not in that cat_super
                    for(i in 1:length(supercats)){ 
                        tempdataframe[, i] <- unlist(lapply(Bus$categories, catlookup, supercats[i]))
                        }
                #collapse separate columns into one; remove all 0s and just keep supercategories
                    temp <- list()
                    for(i in 1:length(tempdataframe[, 1])){ 
                        temp[i] <- tempdataframe[i, ][is.na(match(tempdataframe[i, ], 0))][1]
                        }
                    Bus$cat_super <- unlist(temp)
            }
            if(Cores>1){
              #make dataframe where each column represents a different cat; enter 0 if a business is not in that cat_super
                  tempdataframe <- foreach(i=1:length(supercats), .combine=cbind) %dopar%{
                      unlist(lapply(Bus$categories, catlookup, supercats[i]))
                  }
              #collapse separate columns into one; remove all 0s and just keep supercategories
                  temp <- foreach(i=1:length(tempdataframe[, 1])) %dopar%{
                    tempdataframe[i, ][is.na(match(tempdataframe[i, ], 0))][1]
                    }
                  Bus$cat_super <- unlist(temp)
            }

    #4 - Create variables to measure when a business was alive
        Bus$Alive2004 <- ifelse(Bus$date_open<as.Date("2005-01-01") & Bus$date_close>=as.Date("2004-01-01, 1, 0"), 1, 0)
        Bus$Alive2005 <- ifelse(Bus$date_open<as.Date("2006-01-01") & Bus$date_close>=as.Date("2005-01-01, 1, 0"), 1, 0)
        Bus$Alive2006 <- ifelse(Bus$date_open<as.Date("2007-01-01") & Bus$date_close>=as.Date("2006-01-01, 1, 0"), 1, 0)
        Bus$Alive2007 <- ifelse(Bus$date_open<as.Date("2008-01-01") & Bus$date_close>=as.Date("2007-01-01, 1, 0"), 1, 0)  
        Bus$Alive2008 <- ifelse(Bus$date_open<as.Date("2009-01-01") & Bus$date_close>=as.Date("2008-01-01, 1, 0"), 1, 0)
        Bus$Alive2009 <- ifelse(Bus$date_open<as.Date("2010-01-01") & Bus$date_close>=as.Date("2009-01-01, 1, 0"), 1, 0)
        Bus$Alive2010 <- ifelse(Bus$date_open<as.Date("2011-01-01") & Bus$date_close>=as.Date("2010-01-01, 1, 0"), 1, 0)
        Bus$Alive2011 <- ifelse(Bus$date_open<as.Date("2012-01-01") & Bus$date_close>=as.Date("2011-01-01, 1, 0"), 1, 0)
        Bus$Alive2012 <- ifelse(Bus$date_open<as.Date("2013-01-01") & Bus$date_close>=as.Date("2012-01-01, 1, 0"), 1, 0)
        Bus$Alive2013 <- ifelse(Bus$date_open<as.Date("2014-01-01") & Bus$date_close>=as.Date("2013-01-01, 1, 0"), 1, 0)
        Bus$Alive2014 <- ifelse(Bus$date_open<as.Date("2015-01-01") & Bus$date_close>=as.Date("2014-01-01, 1, 0"), 1, 0)
        Bus$Alive2015 <- ifelse(Bus$date_open<as.Date("2016-01-01") & Bus$date_close>=as.Date("2015-01-01, 1, 0"), 1, 0)
        Bus$Alive2015 <- ifelse(Bus$date_open<as.Date("2017-01-01") & Bus$date_close>=as.Date("2016-01-01, 1, 0"), 1, 0)
        
    #5 - Create Super City variable
        #Determine which cities are actually connected
            #Create list of cities by city and state (some cities from different states have the same name)
                Cities <- as.data.frame(table(Bus$city, Bus$state))
                Cities <- Cities[Cities$Freq>0, ]
            #Sort list for future ease
                Cities <- Cities[order(Cities$Freq), ]
            #Create mean latitude and longitude for each city
                Cities$longitude <- 0
                Cities$latitude <- 0
                for(i in 1:length(Cities$Freq)){
                  Cities[i, 4] <- mean(Bus[Bus$city==Cities[i, 1] & Bus$state==Cities[i, 2], ]$longitude)
                  Cities[i, 5] <- mean(Bus[Bus$city==Cities[i, 1] & Bus$state==Cities[i, 2], ]$latitude)
                }
                names(Cities)[names(Cities)=="Var2"] <- "State"
        #fix var1 class
            Cities$Var1 <- as.character(Cities$Var1)
            Cities$Var1 <- iconv(Cities$Var1, to="ASCII//TRANSLIT")
        #Fix montreal name
            Cities$Var1[Cities$Var1=="Montreal"] <- "Montreal1"
            Cities$Var1[Cities$Var1=="MontrACal"] <- "Montreal"
        #Fix edinburgh info
            Cities$Var1[Cities$Var1=="Edinburgh" & Cities$State!="EDH"] <- "Edinburgh1"
        #create dataframe of supercities info
            city_supernames <- c("Las Vegas", "Phoenix", "Charlotte", "Pittsburgh", "Cleveland", "Madison", "Champaign", "Toronto", "Montreal", "Calgary", "Durham")
            SuperCities <- Cities[Cities$Var1 %in% city_supernames, ]
        #Determine distance between all businesses and supercities
            latitudes <- Bus$latitude
            longitudes <- Bus$longitude
            latitudes2 <- SuperCities$latitude
            longitudes2 <- SuperCities$longitude
            #Create dataframe consisting of a column for each super city, and a row for each city.
                tempdf <- as.data.frame(matrix(nrow=nrow(Bus), ncol=nrow(SuperCities)))
            #for each city, find distance from each super city
                for(i in 1:nrow(SuperCities)){
                    tempdf[, i] <- mapply(lat.1=latitudes, lon.1=longitudes, lat.2=latitudes2[i], lon.2=longitudes2[i], FUN=gdist)  
                    print(i)
                }
            #Determine closest super city for each city and extract that distance
                dist_to_super <- apply(tempdf, 1, function(x) min(x, na.rm=T))
                super_index <- which(tempdf==dist_to_super, arr.ind=T)
                super_index <- super_index[order(super_index[, 1]), ]
                super_index <- super_index[, 2]
                super_city <- SuperCities$Var1[super_index]
                Bus$dist <- dist_to_super
                Bus <- Bus[!(is.na(Bus$latitude) | is.na(Bus$longitude)), ]
                Bus$city_super <- super_city

        #Remove cities that are not near any super city
            Bus <- Bus[Bus$dist<50, ]
            # Bus <- Bus[!(Bus$dist>5 & Bus$city_super=="Durham"), ]
        #Extract distance info for later
            dist_super <- Bus$dist
            Bus$dist <- NULL
            saveRDS(dist_super, "DistanceToSuperCities.rds")
        #Rename category variable        
            names(Bus)[names(Bus)=="categories"] <- "cat"
        #Order businesses by id
            Bus <- Bus[order(Bus$business_id), ]

    #6 - Save RDS
        saveRDS(Bus, "BusinessData.rds")
            
}
