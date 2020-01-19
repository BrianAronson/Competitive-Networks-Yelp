#' 16 - Create Dynamic Variables
#' @description Must specify the same intervals as in density dependence model
#' @export
#' @import data.table

yelp_Dynamic_Vars  <- function(Directory, BaseDirectory, City, Intervals){
    #1 - Read Data
        setwd(Directory)
        Bus <- readRDS("Restaurants.rds")
        Bus <- Bus[Bus$city_super==City, ]
        setwd(BaseDirectory)
        Rev <- readRDS("ReviewData.rds")
        setwd(Directory)
        
    #2 - Convert to data.table
        Rev <- data.table(Rev)
        
    #3 - restrict rev to those in bus
        Rev <- Rev[Rev$business_id %in% Bus$id, ]
        
    #4 - Create Interv vector
        Interv2 <- (2018.5-2011)/Intervals
        Interv <- c(2011+(0:(Interv2)*Intervals))
        
    #5 - Convert rev dates to numbers
        Rev$date <- as.numeric(substr(Rev$date, 1, 4))+ as.numeric(substr(Rev$date, 6, 7))/12
    
    #6 - Create index for all businesses
        IDs <- Bus$id
    
    #7 - Create dynamic review count and stars variables
        tempdfs <- vector(mode="list", length=length(Interv))
        for(i in 1:(length(Interv)-1)){ 
            tempdfs[[i]] <- matrix(nrow=length(IDs), ncol=2)
            tempRev <- Rev[Rev$date>=Interv[i] & Rev$date<(Interv[i]+Intervals), ]
            for(j in 1:length(IDs)){
                temp <- tempRev[tempRev$business_id==IDs[j], ]
                if(nrow(temp)==0){
                    next
                }
                tempdfs[[i]][j, 1] <- nrow(temp)
                tempdfs[[i]][j, 2] <- mean(temp$stars)
            }
        }
        #Create ongoing stars and reviews variables
            #Create a dataframe for each variable and each business across time
                DF1 <- tempdfs[[1]][, 1]
                DF2 <- tempdfs[[1]][, 2]
                for(i in 2:length(Interv)){
                   DF1 <- cbind(DF1, tempdfs[[i]][, 1])
                   DF2 <- cbind(DF2, tempdfs[[i]][, 2])
                }
            #Determine reviews and stars prior to first interval
                tempdf2 <- matrix(nrow=length(IDs), ncol=2)
                tempRev <- Rev[Rev$date<Interv[1], ]
                for(j in 1:length(IDs)){
                  temp <- tempRev[tempRev$business_id==IDs[j], ]
                  if(nrow(temp)==0){
                    next
                  }
                  tempdf2[j, 1] <- nrow(temp)
                  tempdf2[j, 2] <- mean(temp$stars)
                }
            #replace NAs with 0s in all dfs
                DF1 <- ifelse(is.na(DF1), 0, DF1)
                DF2 <- ifelse(is.na(DF2), 0, DF2)
                tempdf2 <- ifelse(is.na(tempdf2), 0, tempdf2)
            #Determine total cumulative stats by interval
                DF4 <- DF2
                DF3 <- DF1
                DF3[, 1] <- DF3[, 1]+tempdf2[, 1]
                DF4[, 1] <- ((DF4[, 1]*DF1[, 1])+(tempdf2[, 1]*tempdf2[, 2]))/DF3[, 1]
                DF4[, 1] <- ifelse(is.na(DF4[, 1]), 0, DF4[, 1])
                for(i in 2:ncol(DF3)){
                    DF3[, i] <- DF3[, i]+DF3[, i-1]
                    DF4[, i] <- ((DF2[, i]*DF1[, i])+(DF4[, i-1]*DF3[, i-1]))/DF3[, i]
                    DF4[, i] <- ifelse(is.na(DF4[, i]), 0, DF4[, i])
                }
        #Convert to long format
            DynVars <- data.frame(revs_int=c(DF1), stars_int=c(DF2), revs_cur=c(DF3), stars_cur=c(DF4))
            
    #8 - Save RDS
        saveRDS(DynVars, paste(Directory, City, "_DynVars.rds", sep=""))      
}

