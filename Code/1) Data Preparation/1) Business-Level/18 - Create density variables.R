#' 18 - Create Density Dependence Variables
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
#' 6 - Bring in topic models
#' 7 - Create Interv vector
#' 8 - Create datatable of info    
#' 9 - Create a list (length = interval) that contains empty matrices that have space for counting the number of ties for each Bus that satisfy each threshhold 
#' 10 - Create density dependence info and other traits
#' 11 - Merge results by interval
#' 12 - Save

yelp_Density_Vars <- function(Directory, City, Intervals, DistMax=NULL, DistCuts=NULL, CustDistCuts=NULL, WeightedAtr=NULL, AtrMax=NULL, AtrCuts=NULL, atrranks=F, atrrankcuts=NULL, CustAtrCuts=NULL, Customers="Yes", Parallel=F, Competitors="All"){
  #0 - Set Directory
      setwd(Directory)
      
  #1 - Read Data
      Bus <- readRDS("Restaurants.rds")
      Bus <- Bus[Bus$city_super==City, ]
      Dist <- readRDS(paste(City, "_dist.rds", sep=""))
      Cust <- readRDS(paste(City, "_cust.rds", sep=""))
      Cust <- data.frame(Proportion=Cust)
      
      
  #2 - Change dates to numeric. Assume date_close only true if business marked as closed; assign all other dates closed as 2100;
      Bus$date_close <- ifelse(Bus$is_open==0, as.numeric(substr(Bus$date_close, 1, 4))+as.numeric(substr(Bus$date_close, 6, 7))/12, 2100)
      Bus$date_open <- as.numeric(substr(Bus$date_open, 1, 4))+as.numeric(substr(Bus$date_open, 6, 7))/12
      Bus$date_close <- .5*floor(Bus$date_close/.5) 
      Bus$date_open <- .5*floor(Bus$date_open/.5) 
      
  #3 - Create business ID vector
      IDs <- Bus$id
  
  #4 - Create distance variables
      #Create distance thresholds based on inputs
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
          perc.rank  <-  function(x) trunc(rank(x))/length(x)
      #expand to 0-1
          Atr2 <- range01(Atr)
          
  #6 - Bring in topic models
          temp3 <- readRDS(paste(City, "_revtopics.rds", sep=""))
          temp3 <- range01(temp3)
          Atr2 <- rowMeans(cbind(Atr2, temp3), na.rm = T)
          Atr2 <- range01(Atr2)
          
      #Alter to percentile ranks by row (i.e. find out which places ego is most and least similar to)
          temp <- matrix(Atr, nrow=sqrt(length(Atr)))
          Atr <- apply(temp, 1, perc.rank)
          Atr <- c(Atr)
          
  #7 - Create Interv vector
      Interv2 <- (2018.5-2011)/Intervals
      Interv <- c(2011+(0:(Interv2)*Intervals))
  
  #8 - Create datatable of info
      DT <- data.table(ID=rep(Bus$id, each=nrow(Bus)), ID2=rep(Bus$id, nrow(Bus)), Atr, Atr2, Dist, Cust=Cust$Proportion, open1=rep(Bus$date_open, each=nrow(Bus)), close1=rep(Bus$date_close, each=nrow(Bus)), open2=rep(Bus$date_open, nrow(Bus)), close2=rep(Bus$date_close, nrow(Bus)))

  #9 - Create a list (length = interval) that contains empty matrices that have space for counting the number of ties for each Bus that satisfy each threshhold
      results <- list()
      df <- data.frame(ID=Bus$id)

  #10 - Create density dependence info and other traits
      for(k in 1:(length(Interv)-1)){
          index <- Interv[k]
          #subset data
              tempDT <- DT[close1>=index & close2>=index & open1<=index & open2<index & ID!=ID2, ]
          #Global variables/those that don't rely on closeness thresholds
              density.1 <- as.data.frame(tempDT[Dist<=1, .(density.1=.N), by=list(ID)])
              density.2 <- as.data.frame(tempDT[Dist<=2, .(density.2=.N), by=list(ID)])
              spec <- as.data.frame(tempDT[, .(spec=mean(Atr2)), by=list(ID)])
              atyp <- as.data.frame(tempDT[, .(atyp=min(Atr2)), by=list(ID)])
              
              cust_embed.5 <- as.data.frame(tempDT[, .(cust_embed.5=mean(Cust)), by=list(ID)])
              cust_embed_sim.5 <- as.data.frame(tempDT[Atr<.2, .(cust_embed_sim.5=mean(Cust)), by=list(ID)])
              cust_embed_dissim.5 <- as.data.frame(tempDT[Atr>.8, .(cust_embed_dissim.5=mean(Cust)), by=list(ID)])
              
              thresholds  <-  data.table(dist_threshold=1:3)
              tempDT  <-  tempDT[thresholds, on=.(Dist <= dist_threshold), # non-equi join
                               allow.cartesian=TRUE, # Fixes error, see details in ?data.table
                               nomatch=NA # Do not include thresholds which no row satisfies (i.e. Dist < 1)
                               ]
              temp1 <- as.data.frame(tempDT[, .(
                                         mean_sim=mean(Atr2), 
                                         mean_rank_sim=mean(Atr), 
                                         min_sim=min(Atr2), 
                                         min_rank_sim=min(Atr), 
                                         cust_embed=mean(Cust)), 
                                      by=list(ID, Dist)])
              temp2 <- as.data.frame(tempDT[Atr<.2, .(cust_embed_sim=mean(Cust)), by=list(ID, Dist)])
              temp3 <- as.data.frame(tempDT[Atr>.8, .(cust_embed_dissim=mean(Cust)), by=list(ID, Dist)])
              temp4 <- as.data.frame(tempDT[Atr<.1, .(Crowding=.N), by=list(ID, Dist)])
              temp5 <- as.data.frame(tempDT[Atr2<.1, .(Crowding1=.N), by=list(ID, Dist)])

              temp1 <- merge(temp1, temp2, by=c("ID", "Dist"), all=T)
              temp1 <- merge(temp1, temp3, by=c("ID", "Dist"), all=T)
              temp1 <- merge(temp1, temp4, by=c("ID", "Dist"), all=T)
              temp1 <- merge(temp1, temp5, by=c("ID", "Dist"), all=T)
              
          #transform to wide
              temp1 <- reshape(temp1, idvar = "ID", timevar = "Dist", direction = "wide")
          #merge in global measures
              temp1 <- merge(temp1, density.1, all=T)
              temp1 <- merge(temp1, density.2, all=T)
              temp1 <- merge(temp1, spec, all=T)
              temp1 <- merge(temp1, atyp, all=T)
              
              temp1 <- merge(temp1, cust_embed.5, all=T)
              temp1 <- merge(temp1, cust_embed_sim.5, all=T)
              temp1 <- merge(temp1, cust_embed_dissim.5, all=T)
          #merge results
              results[[k]] <- as.data.table(Reduce(function(x, y) merge(x, y, all=TRUE), list(df, temp1)))
              print(k/length(Interv))
      }
      
  #11 - Merge results by interval
      DF <- results[[1]]
      for(i in 2:(length(Interv)-1)){
        DF <- rbind(DF, results[[i]])
      }
  
  #12 -Save RDS
      saveRDS(DF, paste(Directory, City, "_DensityVars.rds", sep=""))
    
}
