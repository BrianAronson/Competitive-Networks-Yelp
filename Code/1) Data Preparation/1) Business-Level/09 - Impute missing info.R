#' 9 - Impute missing info
#' @description This function imputes key missing info. It takes the means of a OLS regression multiple imputation output (5 imputes) The function takes a relatively long time to run.
#' @param Directory Directory of files.
#' @export
#' @import data.table
#' @import mice
#' @details 
#' This script does the following:
#' 0 - Set Directory  
#' 1 - Read Data
#' 2 - Create dataset that contains only variables that may be relevent for imputation
#' 3 - Find all variables that have missing data
#' 4 - Impute missing data
#' 5 - Merge imputed data with dataset
#' 6 - Sort business names by business id'; rename business id to id
#' 7 - Save RDS

yelp_Impute_Missing <- function(Directory){
  #0 - Set Directory
      setwd(Directory)

  #1 - Read Data
      Bus <- readRDS("Restaurants2.rds")
      Bus <- as.data.frame(Bus)
      
  #2 - Create dataset that contains only variables that may be relevent for imputation
      Bus2 <- Bus
      Bus2 <- Bus2[, c(34:ncol(Bus2))]
      Bus2[, c("Hour_MondayOpen", "Hour_MondayClose", "Hour_TuesdayOpen", "Hour_TuesdayClose", "Hour_WednesdayOpen", "Hour_WednesdayClose", "Hour_ThursdayOpen", "Hour_ThursdayClose", "Hour_FridayOpen", "Hour_FridayClose", "Hour_SaturdayOpen", "Hour_SaturdayClose", "Hour_SundayOpen", "Hour_SundayClose", "cat")] <- NULL

  #3 - Find all variables that have missing data
      missing <- vector()
      for(i in 1:ncol(Bus2)){
        missing[i] <- any(is.na(Bus2[, i]))
      }

      #because imp function is finnicky, change variable names temporarily
          allnames <- names(Bus2)
          missingnames <- names(Bus2)[missing]
          names(Bus2) <- paste("var", 1:length(Bus2), sep="")
  #4 - Impute missing data
      #Use regression imputation
          imp  <-  mice(Bus2, method = "norm.predict", m = 5)
      #Take the mean value of the 5 imputations for each variable, and put them in the Bus dataset
          #subset tempData to just have imputations
              tempData <- imp$imp
          #Look through each variable and extract the mode
              for(m in 1:length(tempData)){
                  if(is.null(tempData[[m]])){
                    next
                  }
                  tempvariable <- tempData[[m]] #subset to one variable
                  tempmeans <- rowMeans(tempvariable)
                  Bus2[is.na(Bus2[, m]), m] <- unlist(tempmeans) # replace Bus NAs with imputed info.
                  print(m)
              }

  #5 - Merge imputed data with dataset
      names(Bus2) <- allnames
      Bus[, missingnames] <- Bus2[, missingnames]
    
  #6 - Sort business names by business id'; rename business id to id    
      names(Bus)[names(Bus)=="business_id"] <- "id"
      Bus <- Bus[order(Bus$id), ]  

  #7 - kill places with NA for open date (i.e. durham places)
      Bus <- Bus[!is.na(Bus$date_open), ]
  
  #8 - SaveRDS
      saveRDS(Bus, "Restaurants.rds")
      saveRDS(Bus, "Restaurants2.rds")
        
}


