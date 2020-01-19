#' 2 - Create business review variables
#' @description This function creates the key business review variables using the review and user dataset. Specifically, it creates a variable for business review counts (rev_count), business reviewers (revers), date opened (date_open), date closed (date_close), life span (age), popularity (pop), and review count by non elites (rev_count_nonelites). Since it only adds variables, the resulting business dataset overwrites the original.
#' @param Directory Directory of files.
#' @export
#' @import data.table LaF
#' @details 
#' This script does the following:
#' 0 - Set Directory  
#' 1 - Read Data
#' 2 - Convert to data.table and ort Revs and Bus by business_id
#' 3 - Create variables within Bus for business review counts (rev_count), business reviewers (revers), date opened (date_open), date closed (date_close), life span (age), and popularity (pop)
#' 4 - Create variable within Bus for number of reviewers by non elites (rev_count_nonelites)
#' 5 - Save RDS

yelp_Review_Variables  <- function(Directory, verbose=T){
    #0 - Set Directory
        setwd(Directory)

    #1 - Read Data
        Bus <- readRDS("BusinessData.rds")
        Rev <- readRDS("ReviewData.rds")
        Rev2 <- readRDS("ReviewData(Full).rds")
        User <- readRDS("UserData.rds")
 
    #2 - Convert to data.table and sort Revs and Bus by business_id
        Bus <- data.table(Bus)
        Rev <- data.table(Rev)
        Bus <- Bus[order(Bus$business_id), ]
        Rev <- Rev[order(Rev$business_id, Rev$date), ]

    #3 - Create variables within Bus for business Rev counts, business revers, date opened, date closed, life span, and popularity
        #Count Revs per business, and reserve order
            temp <- as.data.frame(table(Rev$business_id))
            colnames(temp) <- c("business_id", "rev_count")
            temp1 <- data.frame(business_id=unique(Rev$business_id), Index=1:length(unique(Rev$business_id)))
            temp <- merge(temp, temp1, by="business_id")
            temp <- temp[order(temp$Index), ]
        #Estimate date closed and date opened (this syntax works because Revs are listed by date and because order is reserved)
            temp$date_close <- Rev$date[c(cumsum(temp$rev_count))]
            temp$date_open <- Rev$date[c(1, cumsum(temp$rev_count[-length(temp$rev_count)])+1)]
        #Create list of business revers for each business - same as above
            #Create index of revers for each business
                temp$temp1=as.integer(c(1, cumsum(temp$rev_count[-length(temp$rev_count)])+1)) #first Rev
                temp$temp2 <- c(cumsum(temp$rev_count)) #last Rev
                tempfunction <- function(x, y){
                  x:y
                }
                temp$matches <- mapply(tempfunction, temp$temp1, temp$temp2) #sapply only works with one vector and one constant; mapply works across multiple vectors
            #Use Rever index to grab Rever names
                tempfunction <- function(x){
                  paste(Rev$user_id[c(x)], collapse = ", ")
                }
                temp1 <- temp$matches
                temp2 <- lapply(temp1, tempfunction)
                temp$revers <- temp2
                temp$Index <- NULL
                temp$temp1 <- NULL
                temp$temp2 <- NULL
                temp$matches <- NULL
        #Merge new info into Bus
            temp$business_id <- as.character(temp$business_id)
            Bus <- merge(Bus, temp, by="business_id")
        #Convert to dates
            Bus$date_open <- as.Date(Bus$date_open)
            Bus$date_close <- as.Date(Bus$date_close)
        #Estimate life span
            Bus$age <- as.numeric(Bus$date_close-Bus$date_open)
        #Estimate popularity
            Bus$pop <- Bus$rev_count/Bus$age
        #Adjust popularity for newly opened
            Bus$pop_adj <- ifelse(Bus$age<100, Bus$rev_count/182.5, Bus$pop)
            
    #4 - Create variable within Bus for number of Revs by non elites
        #prep user data
            User$elite <- ifelse(User$elite=="None", 0, 1)
            NonEliteUsers <- User[User$elite==0, ]$user_id
            keep <- Rev$user_id %in% NonEliteUsers
            tempRev <- Rev[keep, ]
        #count revers excluding nonelites
            Temprev_count <- as.data.frame(table(Rev$business_id))
            Bus$rev_count_nonelites <- Temprev_count$Freq
    
    #5 - Save RDS
        saveRDS(Bus, "BusinessData.rds")
            
}

