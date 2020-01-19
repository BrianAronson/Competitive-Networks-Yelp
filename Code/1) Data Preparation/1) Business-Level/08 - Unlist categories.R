#' 8 - Unlist Categories
#' @description This function unlists the categories and appends them as columns in the restraurants dataframe.
#' @param Directory Directory of files.
#' @export
#' @import data.table
#' @details 
#' This script does the following:
#' 0 - Set Directory  
#' 1 - Read Data
#' 2 - Create a dataframe with a column for each (vaguely common) unique category
#' 3 - Fill dataframe with category information
#' 4 - Merge into Business
#' 5 - Merge redundant variables
#' 6 - Save RDS

yelp_Unlist_Categories <- function(Directory){
  #0 - Set Directory
    setwd(Directory)
  
  #1 - Read Data
    Attr <- readRDS("Attributes.rds")
    Bus <- readRDS("Restaurants.rds")
  
  #2 - Create a dataframe with a column for each (vaguely common) unique category
    cat <- as.data.frame(table(unlist(Bus$cat)))
    cat <- cat[cat$Freq>30, ]
    cat <- as.character(sort(cat$Var1))
    catDF <- as.data.frame(matrix(data=FALSE, nrow=nrow(Bus), ncol=length(cat)))
    names(catDF) <- cat
    
  #3 - Fill dataframe with category information
    for(i in 1:nrow(Bus)){
      temp <- Bus$cat[[i]]
      for(j in 1:length(temp)){
        temp[j]
        catDF[i, names(catDF)==temp[j]] <- TRUE
      }
    }
        
  #4 - Merge into Business
      Bus <- cbind(Bus, catDF)

  #5 - Merge redundant variables
      Bus$`Tapas Bars` <- ifelse(Bus$`Tapas Bars`==T | Bus$`Tapas/Small Plates`==T, T, F)
      Bus$`Tapas/Small Plates` <- NULL
      Bus$cat <- sapply(Bus$cat, setdiff, "Tapas/Small Plates")
      names(Bus)[names(Bus)=="& Event Spaces"] <- "Event Spaces"
      Bus$`Breakfast & Brunch` <- ifelse(Bus$`& Brunch`==T | Bus$`Breakfast & Brunch`==T, T, F)
      Bus$`Arts & Entertainment` <- ifelse(Bus$`& Entertainment`==T | Bus$`Arts & Entertainment`==T, T, F)
      Bus$`Coffee & Tea` <- ifelse(Bus$`& Tea`==T | Bus$`Coffee & Tea`==T, T, F)
      Bus$`American (New)` <- ifelse(Bus$`(New)`==T | Bus$`American (New)`==T, T, F)
      Bus$`American (Traditional)` <- ifelse(Bus$`(Traditional)`==T | Bus$`American (Traditional)`==T, T, F)
      Bus$`& Brunch` <- NULL
      Bus$`& Entertainment` <- NULL
      Bus$`& Tea` <- NULL
      Bus$`(New)` <- NULL
      Bus$`(Traditional)` <- NULL
      Bus$cat <- sapply(Bus$cat, setdiff, "& Brunch")
      Bus$cat <- sapply(Bus$cat, setdiff, "& Entertainment")
      Bus$cat <- sapply(Bus$cat, setdiff, "& Tea")
      Bus$cat <- sapply(Bus$cat, setdiff, "(New)")
      Bus$cat <- sapply(Bus$cat, setdiff, "(Traditional)")

  #6 - SaveRDS
      saveRDS(Bus, "Restaurants.rds")
        
}