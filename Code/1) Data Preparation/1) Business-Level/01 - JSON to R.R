#' 1 - JSON to R
#' @description This function assumes all file names are the yelp defaults (see in code below) and are all in the same directory. The files may need to be extracted from tar first, which can be done with the "tartool.exe." This function also assumes that the user has manually run a python tool for converting the review and user data into csv format, and that those files have been placed in the same directory as the other JSON files.
#' @param Directory Directory of files.
#' @param Subset Option to reduce file size by removing review text and user friends
#' @export
#' @import jsonlite
#' @import RJSONIO
#' @details 
#' This script does the following:
#' 0 - Set work directory
#' 1 - Read smaller JSONs to 
#' 2 - Read CSVs
#' 3 - Save copies of user and review data
#' 4 - Subset data before putting it online
#' 5 - Save as RDS

yelp_JSON_to_R<-function(Directory, Subset=T){
    #0 - Set work directory
        setwd(Directory)
    
    #1 - Read smaller JSONs to 
        BusinessData <- stream_in(file("yelp_academic_dataset_business.json"))
        CheckinData <- stream_in(file("yelp_academic_dataset_checkin.json"),pagesize=500,verbose = TRUE)
        TipData <- stream_in(file("yelp_academic_dataset_tip.json"))
        
    #2 - Read CSVs
        UserData<-read.csv("yelp_academic_dataset_user.csv", stringsAsFactors=FALSE)
        ReviewData<-read.csv("yelp_academic_dataset_review.csv", stringsAsFactors=FALSE)
        
    #3 - save copies of user and review data
        saveRDS(ReviewData,"ReviewData(Full).rds")
        saveRDS(UserData, "UserData(Full).rds")
        
    #4 - Subset data before putting it online
        if(Subset){
            ReviewData$text<-NULL
            UserData$friends<-NULL
        }
        
    #5 - Save as RDS
        saveRDS(BusinessData, "BusinessData.rds")
        saveRDS(CheckinData, "CheckinData.rds")
        saveRDS(TipData, "TipData.rds")
        saveRDS(UserData, "UserData.rds")
        saveRDS(ReviewData, "ReviewData.rds")
        
}

