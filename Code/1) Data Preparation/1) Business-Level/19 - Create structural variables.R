#' 19 - Create Structural Variables
#' @export
#' @import data.table
#' @details 
#' This script does the following:
#' 0 - Set Directory  
#' 1 - Read Data
#' 2 - Change dates to numeric. Assume date_close only true if business marked as closed; assign all other dates closed as 2100;
#' 3 - Create business ID vector
#' 4 - Create distance variables
#' 5 - Assign attribute thresholds
#' 6 - Create Interv vector
#' 7 - Create datatable of info    
#' 8 - Create list of all possible pair names
#' 9 - Create a list (length = interval) that contains empty matrices that have space for counting the number of ties for each Bus that satisfy each threshhold 
#' 10 - Create adjacency counts
#' 11 - Bind results
#' 12 - Save results

yelp_Structural_Vars <- function(Directory, City, Intervals, DistMax=NULL, DistCuts=NULL, CustDistCuts=NULL, WeightedAtr=NULL, AtrMax=NULL, AtrCuts=NULL, atrranks=F, atrrankcuts=NULL, CustAtrCuts=NULL, Customers="Yes", Parallel=F, Competitors="All"){
  #0 - Set Directory
      setwd(Directory)
  
  #1 - Read Data
      Bus <- readRDS("Restaurants.rds")
      Bus <- Bus[Bus$city_super==City, ]
      Dist <- readRDS(paste(City, "_dist.rds", sep=""))
      Cust <- readRDS(paste(City, "_nullties.rds", sep=""))
      
  #2 - Change dates to numeric. Assume date_close only true if business marked as closed; assign all other dates closed as 2100;
      Bus$date_close <- ifelse(Bus$is_open==0, as.numeric(substr(Bus$date_close, 1, 4))+as.numeric(substr(Bus$date_close, 6, 7))/12, 2100)
      Bus$date_open <- as.numeric(substr(Bus$date_open, 1, 4))+as.numeric(substr(Bus$date_open, 6, 7))/12
      #round down to .5s
          Bus$date_close <- .5*floor(Bus$date_close/.5) 
          Bus$date_open <- .5*floor(Bus$date_open/.5) 
          
  #3 - Create business ID vector
      IDs <- Bus$id

  #4 - Create distance variables
      #Create distance thresholds based on inputs
      Dist2 <- Dist    
      if(!is.null(CustDistCuts)){
            Dist <- cut(Dist, c(0, CustDistCuts), include.lowest=TRUE)
          }else{
            Dist <- cut(Dist, seq(from = 0, to = DistMax, by = DistMax/DistCuts), include.lowest=TRUE)
          }
      #Change to numeric
          Dist <- as.numeric(Dist)
      #Assign NAs to 1+ highest number
          Dist <- ifelse(is.na(Dist), max(Dist, na.rm = T)+1, Dist)
      
  #5 - Assign attribute thresholds
      Atr <- readRDS(paste(City, "_atr.rds", sep=""))
      #create functions
          range01  <-  function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
          perc.rank  <-  function(x) (rank(x)-1)/(length(x)-1)
      #expand to 0-1
          Atr2 <- range01(Atr)
          
  #5.1 - bring in topic models
          temp3 <- readRDS(paste(City, "_revtopics.rds", sep=""))
          temp3 <- range01(temp3)
          Atr2 <- rowMeans(cbind(Atr2, temp3), na.rm = T)
          Atr2 <- range01(Atr2)

  #5.2 - Alter to percentile ranks by row (i.e. find out which places ego is most and least similar to)
      temp <- matrix(Atr, nrow=sqrt(length(Atr)))
      Atr <- apply(temp, 1, perc.rank)
      Atr <- c(Atr)

  #6 - Create Interv vector
      Interv2 <- (2018.5-2011)/Intervals
      Interv <- c(2011+(0:(Interv2)*Intervals))
      
  #7 - Create datatable of info
        DT <- data.table(ID=rep(Bus$id, each=nrow(Bus)), ID2=rep(Bus$id, nrow(Bus)), Atr2, Dist, Cust=Cust$Proportion, open1=rep(Bus$date_open, each=nrow(Bus)), close1=rep(Bus$date_close, each=nrow(Bus)), open2=rep(Bus$date_open, nrow(Bus)), close2=rep(Bus$date_close, nrow(Bus)), Dist2=Dist2)
        #DT <- DT[DT$ID!=DT$ID2, ]
        #DT$ID2 <- NULL
  
  #9 - Create a list (length = interval) that contains empty matrices that have space for counting the number of ties for each Bus that satisfy each threshhold
      results <- list()
      df <- data.frame(ID=Bus$id)
      
  #10 - Create density dependence info and other traits
  for(k in 1:(length(Interv)-1)){
    index <- Interv[k]
     #Structural traits by distance
        #subset to those open during interval
            tempDT <- DT[DT$close1>=index & DT$close2>=index & DT$open1<=index & DT$open2<=index, c("ID", "ID2", "Atr2", "Dist", "Dist2")]
        #create datatable with rows for each distance
            thresholds  <-  data.table(dist_threshold=1:2)
            passes_threshold  <-  tempDT[thresholds, on=.(Dist <= dist_threshold), # non-equi join
                                   allow.cartesian=TRUE, # Fixes error, see details in ?data.table
                                   nomatch=NA # Do not include thresholds which no row satisfies (i.e. Dist < 1)
                                   ]
        #create list of alters for each ID at each distance
            alterlist <- passes_threshold[, .(ID2=list(ID2)), by=list(ID, Dist)]
        #create variables to append
            localcentrality <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
            localrankcentrality <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
            localwidth <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
            localcrowd.0 <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
            localcrowd.1 <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
            localcrowd.2 <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
            localcrowd.0w <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
            localcrowd.1w <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
            localcrowd.2w <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
            localcrowd.0rank <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
            localcrowd.1rank <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
            localcrowd.2rank <- as.data.frame(matrix(nrow=length(unique(alterlist$ID)), ncol=2))
        #find percentile rank similarity of each ego within each location (as a better marker of specialization; higher = more specialized, lower = more generalized), and niche width (based on diversity of atr)
            #subset to places that could possibly be within 4 miles of each other
                tempDT2 <- tempDT[Dist2<=4, ]
                tempDT2$Dist2 <- NULL
            #prep vars for loop
                tempbus <- unique(tempDT$ID)
                templist <- alterlist$ID2
                tempdist <- alterlist$Dist
            for(i in 1:length(tempbus)){
              tempbus2 <- tempbus[i]
              for(j in 1:2){ #do acrosss distance thresholds
                l <- 3-j
                #grab alter info by distance
                    templist1 <- unlist(alterlist$ID2[alterlist$ID==tempbus2 & alterlist$Dist==l])
                #grab datatables subsetting from bigger datatables
                    if(j==1){
                        tempDT1 <- tempDT2[ID %in% templist1 & ID2 %in% templist1, ]
                    }else{
                        tempDT1 <- tempDT1[ID %in% templist1 & ID2 %in% templist1, ]
                    }
                    if(nrow(tempDT1)<=1){
                      next()
                    }else{
                #create important stats
                    #crowding
                        #find rank similarity
                            tempDT1[, rank:=trunc(frank(Atr2, ties.method="min")), by="ID"]
                        #get mean sim among top x closest alters
                            a <- tempDT1[ID!=ID2 & rank<=(2+(sqrt(.N)/5)), ]
                            meansim5 <- a[, mean(Atr2), by="ID"]
                            a <- tempDT1[ID!=ID2 & rank<=(2+(sqrt(.N)/10)), ]
                            meansim10 <- a[, mean(Atr2), by="ID"]
                            a <- tempDT1[ID!=ID2 & rank<=(2+(sqrt(.N)/20)), ]
                            meansim20 <- a[, mean(Atr2), by="ID"]
                        #get mean sim overall
                            overallsim <- mean(tempDT1$Atr2)
                        #Generate crowding variables
                            localcrowd.0[i, l] <- meansim20$V1[meansim20$ID==tempbus2]
                            localcrowd.1[i, l] <- meansim10$V1[meansim10$ID==tempbus2]
                            localcrowd.2[i, l] <- meansim5$V1[meansim5$ID==tempbus2]
                            localcrowd.0w[i, l] <- meansim20$V1[meansim20$ID==tempbus2]/overallsim
                            localcrowd.1w[i, l] <- meansim10$V1[meansim10$ID==tempbus2]/overallsim
                            localcrowd.2w[i, l] <- meansim5$V1[meansim5$ID==tempbus2]/overallsim
                            localcrowd.0rank[i, l] <- ((rank(meansim20$V1)-1)/(length(meansim20$V1)-1))[meansim20$ID==tempbus2]
                            localcrowd.1rank[i, l] <- ((rank(meansim10$V1)-1)/(length(meansim10$V1)-1))[meansim10$ID==tempbus2]
                            localcrowd.2rank[i, l] <- ((rank(meansim5$V1)-1)/(length(meansim5$V1)-1))[meansim5$ID==tempbus2]
                    #centrality
                        #get mean sims
                            tempDT11 <- tempDT1[tempDT1$ID!=tempDT1$ID2, .(meansim=mean(Atr2)), by=ID]
                        #standard centrality
                            localcentrality[i, l] <- tempDT11$meansim[tempDT11$ID==tempbus2]
                        #rank centrality
                            localrankcentrality[i, l] <- perc.rank(tempDT11$meansim)[tempDT11$ID==tempbus2]
                        #niche width
                            localwidth[i, l] <- mean(tempDT11$meansim)
              }
             setTxtProgressBar(txtProgressBar(min = 0, max = length(tempbus), style = 3), i)
              }
            }
            #create data frame from this
                names(localcentrality) <- paste("localcentrality.", 1:2, sep="")
                names(localrankcentrality) <- paste("localrankcentrality.", 1:2, sep="")
                names(localwidth) <- paste("localwidth.", 1:2, sep="")
                names(localcrowd.0) <- paste("localcrowd.0.", 1:2, sep="")
                names(localcrowd.1) <- paste("localcrowd.1.", 1:2, sep="")
                names(localcrowd.2) <- paste("localcrowd.2.", 1:2, sep="")
                names(localcrowd.0w) <- paste("localcrowd.0w.", 1:2, sep="")
                names(localcrowd.1w) <- paste("localcrowd.1w.", 1:2, sep="")
                names(localcrowd.2w) <- paste("localcrowd.2w.", 1:2, sep="")
                names(localcrowd.0rank) <- paste("localcrowd.0rank.", 1:2, sep="")
                names(localcrowd.1rank) <- paste("localcrowd.1rank.", 1:2, sep="")
                names(localcrowd.2rank) <- paste("localcrowd.2rank.", 1:2, sep="")
            #Create variables at distance=.5
                #crowding
                    #find rank similarity
                        tempDT[, rank:=floor(rank(Atr2)), by="ID"]
                    #get mean sim among top x closest alters
                        a <- tempDT[ID!=ID2 & rank<=(2+(sqrt(.N)/5)), ]
                        meansim5 <- a[, mean(Atr2), by="ID"]
                        a <- tempDT[ID!=ID2 & rank<=(2+(sqrt(.N)/10)), ]
                        meansim10 <- a[, mean(Atr2), by="ID"]
                        a <- tempDT[ID!=ID2 & rank<=(2+(sqrt(.N)/20)), ]
                        meansim20 <- a[, mean(Atr2), by="ID"]
                    #get mean sim overall
                        overallsim <- mean(tempDT$Atr2)
                    #Generate crowding variables
                        localcrowd.0$localcrowd.0.5 <- meansim20$V1
                        localcrowd.1$localcrowd.1.5 <- meansim10$V1
                        localcrowd.2$localcrowd.2.5 <- meansim5$V1
                        localcrowd.0w$localcrowd.0w.5 <- meansim20$V1/overallsim
                        localcrowd.1w$localcrowd.1w.5 <- meansim10$V1/overallsim
                        localcrowd.2w$localcrowd.2w.5 <- meansim5$V1/overallsim
                        localcrowd.0rank$localcrowd.0rank.5 <- (rank(meansim20$V1)-1)/(length(meansim20$V1)-1)
                        localcrowd.1rank$localcrowd.1rank.5 <- (rank(meansim20$V1)-1)/(length(meansim20$V1)-1)
                        localcrowd.2rank$localcrowd.2rank.5 <- (rank(meansim20$V1)-1)/(length(meansim20$V1)-1)
                #centrality
                    #get mean sims
                        tempDT1 <- tempDT[tempDT$ID!=tempDT$ID2, .(meansim=mean(Atr2)), by=ID]
                    #standard centrality
                        localcentrality$localcentrality.5 <- tempDT1$meansim
                    #rank centrality
                        localrankcentrality$localrankcentrality.5 <- perc.rank(tempDT1$meansim)
                    #niche width
                        localwidth$localwidth.5 <- sd(tempDT1$meansim)
                alterlist <- cbind(localcentrality, localrankcentrality, localwidth, localcrowd.0, localcrowd.1, localcrowd.2, localcrowd.0w, localcrowd.1w, localcrowd.2w, localcrowd.0rank, localcrowd.1rank, localcrowd.2rank)
                alterlist$ID <- tempbus

        #merge results
            results[[k]] <- as.data.table(Reduce(function(x, y) merge(x, y, all=TRUE), list(df, alterlist)))
            print(k/length(Interv))
            
    }
    
    #11 - Merge density dependence data
        DF <- results[[1]]
        for(i in 2:(length(Interv)-1)){
          DF <- rbind(DF, results[[i]])
        }
    
    #12 - Save RDS
       saveRDS(DF, paste(Directory, City, "_StructuralVars.rds", sep=""))
      
}