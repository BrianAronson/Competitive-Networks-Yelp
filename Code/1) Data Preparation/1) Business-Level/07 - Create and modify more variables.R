#' 7 - Create and modify more variables
#' @description Creates and modifies more variables
#' @param Directory Directory of files.
#' @export
#' @import data.table
#' @details 
#' This script does the following:
#' 0 - Set Directory  
#' 1 - Read Data
#' 2 - Create an indicator for chain restaurants and chain_supers
#' 3 - Create uniform street names
#' 4 - Create a normalized variable for revieq count
#' 5 - Delete duplicate variable
#' 6 - Create hours variables
#' 7 - Append hours to Bus and remove hours variable from Bus
#' 8 - Save RDS

yelp_More_Vars <- function(Directory){
    #0 - Set Directory
        setwd(Directory)
          
    #1 - Read Data
        Bus <- readRDS("Restaurants.rds")
        Bus <- as.data.frame(Bus)
        Bus <- Bus[, !duplicated(names(Bus))]
        Bus <- data.table(Bus)
        Bus$id <- Bus$business_id
        
    #2 - Create an indicator for chain restaurants and chain_super
        # Table
            temp <- as.data.table(table(Bus$name))
        # assume that places with identical names that only occur a few times are not chains but rather places that happen to have identical names
            temp$N <- ifelse(temp$N<10, 1, temp$N)
            names(temp) <- c("name", "chain_freq")
        # merge the Frequency data into Bus
            temp2 <- merge(Bus, temp, by="name")
        # Search for inexact name matches to chain names and replace those names
            chain_names <- unique(temp2$name[temp2$chain_freq>20])
            for(i in 1:length(chain_names)){
              b <- agrep(chain_names[i], temp2$name) 
              temp2$name[b] <- chain_names[i]
              print(i)
            }
        # tabulate chain freqs again
            temp <- as.data.table(table(temp2$name))
            temp$N <- ifelse(temp$N<10, 1, temp$N)
            names(temp) <- c("name", "chain_freq")
        # merge them with temp2
            temp2$chain_freq <- NULL
            temp2 <- merge(temp2, temp, by="name")
        # create chain and super_chain indicators
            temp2$chain <- ifelse(temp2$chain_freq>1, 1, 0)
            temp2$chain_super <- ifelse(temp2$chain_freq>30, 1, 0)
        # for super chains, create unique identifiers
            temp2$chain_id <- ifelse(temp2$chain==1, temp2$name, 0)
        #replace Bus
            temp2 <- temp2[order(match(temp2$id, Bus$id)), ]
            Bus <- temp2
    
    #3 - Create uniform street names
        #remove everything after "\n in address"
            Bus$address <- gsub("\n.*$", "", Bus$address)
        #remove house number from full address
            Bus$address <- gsub("[0-9]+", "", Bus$address)
            Bus$address <- gsub('^\\ ', '', Bus$address)
            Bus$address <- gsub('^\\ ', '', Bus$address)
            Bus$address <- gsub('^\\ ', '', Bus$address)
            Bus$address <- gsub('\\ $', '', Bus$address)
            
    #4 - Create a normalized variable for revieq count
        Bus$logged_rev_count <- log(Bus$rev_count)
    
    #5 - Delete duplicate variable
        Bus$review_count <- NULL        
    
    #6 - Create hours variables
        # Create mini hours dataframe
            #load properly formatted data
                setwd((paste(prep$mainDir, "0 - Initial Steps/yelp_dataset_challenge_academic_dataset", sep="")))
                temp <- readRDS("BusinessData.rds")
                setwd(prep$derDir)
            #match hours with business
                hours <- temp$hours
                hours$business_id <- temp$business_id
                hours <- hours[hours$business_id %in% Bus$business_id, ]
                newdf <- data.table(business_id=Bus$business_id)
                hours <- merge(newdf, hours, by="business_id", all=T)
                hours$business_id <- NULL
                
        # Create empty dataframe of hours
            hoursnames <- c("Monday", "Monday", "Tuesday", "Tuesday", "Wednesday", "Wednesday", "Thursday", "Thursday", "Friday", "Friday", "Saturday", "Saturday", "Sunday", "Sunday")
            HoursDF <- matrix(ncol=length(hoursnames), nrow=nrow(Bus))
        # Fill in dataframe
            for(i in 1:length(hours)){
                if(is.null(as.data.frame(hours)[, i])){
                    next
                }
                temp <- as.data.frame(hours)[, i]
                a <- (strsplit(temp, "[ -]"))
                temp1 <- sapply(a, head, n = 1L)
                temp2 <- sapply(a, tail, n = 1L)
                HoursDF[, (2*i)-1] <- temp1
                HoursDF[, (2*i)] <- temp2
                print(i)
            }
             #I'm ignoring gaps in places that list multiple hours   
             
        # Convert times to numbers
            tempfunction <- function(x) {
              b <- as.numeric(unlist(strsplit(x, "[:]")))
              x <- b[1]+b[2]/60
            }
            
            for(i in 1:ncol(HoursDF)){
              HoursDF[, i] <- unlist(sapply(HoursDF[, i], tempfunction))
              print(i)          
            }
            HoursDF <- matrix(as.numeric(HoursDF), ncol=14, nrow=nrow(Bus), byrow=F)
        # Fix hours after midnight
            for(i in 1:7){
              HoursDF[, 2*i] <- ifelse(HoursDF[, 2*i]<8, HoursDF[, 2*i]+24, HoursDF[, 2*i])
              print(i)
            }
        # Convert to dataframe
            HoursDF <- as.data.table(HoursDF)
            names(HoursDF) <- c("Hour_MondayOpen", "Hour_MondayClose", "Hour_TuesdayOpen", "Hour_TuesdayClose", "Hour_WednesdayOpen", "Hour_WednesdayClose", "Hour_ThursdayOpen", "Hour_ThursdayClose", "Hour_FridayOpen", "Hour_FridayClose", "Hour_SaturdayOpen", "Hour_SaturdayClose", "Hour_SundayOpen", "Hour_SundayClose")
        #Create an average hours open and close variable
            HoursDF$Hour_Open <- rowMeans(cbind(HoursDF$Hour_MondayOpen, HoursDF$Hour_TuesdayOpen, HoursDF$Hour_WednesdayOpen, HoursDF$Hour_ThursdayOpen, HoursDF$Hour_FridayOpen, HoursDF$Hour_SaturdayOpen, HoursDF$Hour_SundayOpen), na.rm=T)
            HoursDF$Hour_Close <- rowMeans(cbind(HoursDF$Hour_MondayClose, HoursDF$Hour_TuesdayClose, HoursDF$Hour_WednesdayClose, HoursDF$Hour_ThursdayClose, HoursDF$Hour_FridayClose, HoursDF$Hour_SaturdayClose, HoursDF$Hour_SundayClose), na.rm=T)
            
    #7 - Append hours to Bus and remove hours variable from Bus
        Bus <- cbind(Bus, HoursDF)
        Bus$hours <- NULL
    
    #8 - Save RDS
        saveRDS(Bus, "Restaurants.rds")

}
