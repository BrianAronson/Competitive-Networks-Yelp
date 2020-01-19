#' 15 - Create Customer Adjacency Lists
#' @description This function returns a vector that contains the customer overlap of each business in a city, ordered by ID1, ID2. It's recommended that the user loops, assigns, and saves the data when calling the function
#' @export
#' @import data.table
#' @details 
#' This script does the following:
#' 0 - Set Directory  
#' 1 - Read Data
#' 2 - Create temporary dataframe consisting of just the businesses in the specified city.
#' 3 - Load and apply partition data if needed; otherwise, just string split
#' 4 - Simplify user names to reduce memory usage and create a list of reviewer names for each business
#' 5 - Create and return vector counting overlap between business (business 1 repeats, business 2 cycles)
#' 6 - Save

yelp_Customer_Adj_Function <- function(Directory, BaseDirectory, City, Partition){
    #0 - Set Directory
        setwd(Directory)
    
    #1 - Read Data
        Bus <- readRDS("Restaurants.rds")
    
    #2 - Create temporary dataframe consisting of just the businesses in the specified city.
        Bus2 <- Bus[Bus$city_super==City, ]
    
    #3 - Load and apply partition data if needed; otherwise, just string split
        if(Partition!="All"){
            #Read data
                User <- readRDS(paste(BaseDirectory, "/UserData.rds", sep=""))
            #Create an elite user variable
                if(Partition=="nonelite"){
                    User$elite <- ifelse(User$elite=="['None']", 0, 1)
                }
                if(Partition=="meaningful"){
                    User$elite <- ifelse(User$review_count>50, 1, 0)
                }
                EliteUsers <- User[User$elite==1, ]$user_id
            #limit elite users to users in city
                temp <- unique(unlist(strsplit(unlist(Bus2$revers), ", ")))
                EliteUsers <- EliteUsers[EliteUsers%in%temp]
            #remove elite users from reviewers list
                elite_fun <- function(x){
                    a <- unlist(strsplit(x, ", "))
                    a[is.na(match(a, EliteUsers))]
                }
                Bus2$revers <- sapply(Bus2$revers, elite_fun)
        }
        if(Partition=="All"){
            split_fun <- function(x){
                unlist(strsplit(x, ", "))
            }
            Bus2$revers <- sapply(Bus2$revers, split_fun)
        }
    
    #4 - Simplify user names to reduce memory usage and create a list of reviewer names for each business
        #find all revers
            revers <- unlist(Bus2$revers)
        #remove revers with only 1 review
            reversdf <- as.data.frame(table(revers))
            revers <- as.character(reversdf[reversdf$Freq>1, ]$revers)
            Bus2$revers <- sapply(Bus2$revers, function(x) return(x[x %in% revers]))
        #create list of revers ids
            a <- data.frame(a=unique(unlist(Bus2$revers)))
            a$b <- 1:nrow(a)
            reverslist <- sapply(Bus2$revers, function(x) a[match(x, a$a), ]$b)

    #5 - Create and return vector counting overlap between business (business 1 repeats, business 2 cycles)
        #edge list
            temp <- data.table(revers=unlist(reverslist), tie=1, col=rep(1:length(Bus2$id), sapply(reverslist, length)) )
        #edge list of all possible ties
            temp2 <- rep(0, length(Bus2$id)^2)
            
        #grab overlaps in loop
            for(i in 1:nrow(Bus2)){
                busi <- i
                rows <- nrow(Bus2)
                trevs <- temp[temp$col==busi]$revers
                tbus <- temp[temp$revers%in% trevs, ]
                tbus <- tbus[, .(sums=sum(tie)), by="col"]
                temp2[(i-1)*rows + tbus$col] <- tbus$sums
                setTxtProgressBar(txtProgressBar(min = 0, max = length(revers), style = 3), i)}
    
    #6 - Save
            saveRDS(temp2, paste(Directory, City, "_cust.rds", sep=""))

}
