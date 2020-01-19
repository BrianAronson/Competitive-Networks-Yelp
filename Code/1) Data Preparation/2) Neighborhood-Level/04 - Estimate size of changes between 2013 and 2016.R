#' pap2 - 4 - Estimate size of changes between 2013 and 2016
#' @export
#' @import data.table 
#' @import RCurl 
#' @import RJSONIO 
#' @import acs 
#' @import ggplot2 
#' @import tigris
#' @import sp 
#' @import raster 

yelp_size_change <- function(Directory, City){
  
#1 - load data
    setwd(Directory)
    City=prep$Cities[1]
    Bus <- readRDS("Restaurants.rds")
    Cen <- readRDS("Censusinfo.rds")
    clustdf <- readRDS(paste(City, "_clustdf.rds", sep=""))
    
#2 - clean variables
    Bus <- cbind(Bus, Cen)
    datefun <- function(x) as.numeric(substr(x, 1, 4))+(as.numeric(substr(x, 6, 7)))/12
    Bus$date_open <- datefun(Bus$date_open)
    Bus$date_close <- datefun(Bus$date_close)
    Bus$date_close <- ifelse(Bus$is_open==1, 2100, Bus$date_close)
    Bus$date_close <- .5*floor(Bus$date_close/.5)
    Bus$date_open <- .5*floor(Bus$date_open/.5)

#3 - remove duplicate names in cendf
    cendf <- readRDS(paste(City, "_cendf.rds", sep=""))
    cendf2 <- cendf
    for(i in 1:length(prep$Cities)){
      City <- prep$Cities[i]
      cendf <- readRDS(paste(City, "_cendf.rds", sep=""))
      #find names of duplicates
          tnames <- names(cendf)
          dups1 <- grepl("\\.x", tnames)
          dups2 <- grepl("\\.y", tnames)
          normnames <- tnames[dups1]
          normnames <- substr(normnames, 1, nchar(normnames)-2)
          dups3 <- names(cendf) %in% normnames
      #keep first version of those names
          tnames2 <- tnames
          tnames2[dups1] <- normnames
          tnames2[dups2] <- normnames
          tnames2[dups3] <- normnames
          cendf <- cendf[, !duplicated(tnames2)]
          names(cendf) <- tnames2[!duplicated(tnames2)]
      #save changes
          saveRDS(cendf, paste(City, "_cendf.rds", sep=""))
      #output as one big df
          if(i>1){
            cendf2 <- rbind(cendf2, cendf)
          }
    }
    ind <- Bus$id
    Bus <- merge(Bus, cendf2[, c("cluster", "GEOID")], by="GEOID")
    Bus <- Bus[match(ind, Bus$id), ]
    #remove duplicate ids
        a <- which(names(Bus)=="id")
        if(length(a)>1){
          a <- a[-1]
          Bus <- Bus[, -a]
        }


#4 - Create df with cluster info and hazard info
    City=prep$Cities[1]
    clustdf <- readRDS(paste(City, "_clustdf.rds", sep=""))
    cluststr <- readRDS(paste(City, "_cluststr.rds", sep=""))
    cHaz <- merge(clustdf, cluststr, by="cluster")
    for(i in 2:length(prep$Cities)){
      City <- prep$Cities[i]
      clustdf <- readRDS(paste(City, "_clustdf.rds", sep=""))
      cluststr <- readRDS(paste(City, "_cluststr.rds", sep=""))
      temp <- merge(clustdf, cluststr, by="cluster")
      cHaz <- rbind(cHaz, temp)
    }

#5 - sort data by GEOID, Interval, and Cluster
    cHaz$cluster2 <- cHaz$cluster+
       ifelse(cHaz$city_super=="Champaign", 1000, 
       ifelse(cHaz$city_super=="Charlotte", 2000, 
       ifelse(cHaz$city_super=="Cleveland", 3000, 
       ifelse(cHaz$city_super=="Las Vegas", 4000, 
       ifelse(cHaz$city_super=="Madison", 5000, 
       ifelse(cHaz$city_super=="Phoenix", 6000, 
       ifelse(cHaz$city_super=="Pittsburgh", 7000, NA)))))))
    cHaz <- cHaz[order(cHaz$cluster2, cHaz$Interval), ]

    Bus$cluster2 <- Bus$cluster+
        ifelse(Bus$city_super=="Champaign", 1000, 
        ifelse(Bus$city_super=="Charlotte", 2000, 
        ifelse(Bus$city_super=="Cleveland", 3000, 
        ifelse(Bus$city_super=="Las Vegas", 4000, 
        ifelse(Bus$city_super=="Madison", 5000, 
        ifelse(Bus$city_super=="Phoenix", 6000, 
        ifelse(Bus$city_super=="Pittsburgh", 7000, NA)))))))

#6 - remove na clusters
    Bus <- Bus[-which(is.na(Bus$cluster2)), ]

#7 - subset data to 2013 and 2016
    cHaz2 <- cHaz[cHaz$Interval==2013 | cHaz$Interval==2016, ]
    #a) remove clusters where alive<10
        badclus <- cHaz2$cluster2[cHaz2$alive<=0]
        cHaz2 <- cHaz2[!(cHaz2$cluster2 %in% badclus), ]
    #b) identify best clusters
        bestclus <- cHaz2$cluster2[cHaz2$alive>20 & cHaz2$alive<80]
        cHaz2$bestclus <- cHaz2$cluster2 %in% bestclus


#8 - prep atr info for each city
    atr <- list()
    range01  <-  function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
    for(i in 1:length(prep$Cities)){
        cit <- prep$Cities[i]
        atr3 <- range01(readRDS(paste(cit, "_atr.rds", sep="")))
        temp3 <- readRDS(paste(cit, "_revtopics.rds", sep=""))
        temp3 <- range01(temp3)
        atr3 <- rowMeans(cbind(atr3, temp3), na.rm = T)
        atr3 <- range01(atr3)
        atr3 <- matrix(atr3, nrow=sqrt(length(atr3)))
        atr[[i]] <- atr3
        print(i)
    }

#9 - Create threshold for minimum similarity to count as a change
    threshold <- quantile(unlist(atr), .1)
        #a place that is less similar to any of its neighbors than the 10th quantile of similarities
    # threshold <- quantile(unlist(sapply(atr, function(x){diag(x)=1; apply(x, 1, min)} )), .95)
    #     #if a place is less similar to its neighbor than the 5th quantile of minimum similarities.

#10 - For each cluster, find change info
    new.niche <- vector() #niches new
    new.niche.outlier <- vector() #new niches where business isn't just a weirdo
    new.niche.not.outlier <- vector() #new niches where business isn't just a weirdo
    dead.niche <- vector() #niches gone
    dead.niche.outlier <- vector() #niches gone where business wasn't just a weirdo
    dead.niche.not.outlier <- vector()
    sum.died <- vector()
    sum.new <- vector()
    new.to.old.mins <- list()
    new.to.new.mins <- list()
    new.to.old.means <- list()
    new.to.new.means <- list()
    old.to.old.mins <- list()
    old.to.new.mins <- list()
    old.to.old.means <- list()
    old.to.new.means <- list()
    dis.to.old.mins <- list()
    dis.to.old.means <- list()
    op.to.old.mins <- list()
    op.to.old.means <- list()
    
#11 - Get key stats for each cluster
    clusts <- unique(cHaz2$cluster2)
    for(q in 1:length(clusts)){
      #a) subset to cluster
          cHaz3 <- cHaz2[cHaz2$cluster2==clusts[q], ]
          clustids <- unique(cHaz3$cluster2)
      #b) get bus info for those in cluster
          Bus3 <- Bus[Bus$cluster2 %in% clustids & Bus$date_close>=2013, ]
      #c) subset atr to those businesses
          cit <- Bus3$city_super[1]
          citn <- match(cit, prep$Cities)
          ind <- (Bus$cluster2 %in% clustids & Bus$date_close>=2013)[Bus$city_super==cit]
          atr3 <- atr[[citn]][which(ind), which(ind)]
      #d) find businesses alive at 2016 but not 2013
          onlynew.ind <- Bus3$date_open>2013 & Bus3$date_open<=2016 & (Bus3$date_close>2016 | Bus3$is_open==1)
          onlynew.ind2 <- which(onlynew.ind)
      #f) find businesses alive at 2013 but not 2016
          onlyold.ind <- Bus3$date_open<=2013 & Bus3$date_close>2013 & Bus3$date_close<2016 & Bus3$is_open==0
          onlyold.ind2 <- which(onlyold.ind)
      #g) find businesses alive at 2013
          old.ind <- Bus3$date_open<=2013 & (Bus3$date_close>2013 | Bus3$is_open==1)
          old.ind2 <- which(old.ind)
      #h) find businesses alive at 2016
          new.ind <- Bus3$date_open<=2016 & (Bus3$date_close>2016 | Bus3$is_open==1)
          new.ind2 <- which(new.ind)
      #i) Summarize changes
          sum.new[q] <- sum(onlynew.ind)
          sum.died[q] <- sum(onlyold.ind)
      #j) Find businesses that opened and dissolved between 2013 and 2016
          onlydis.ind <- Bus3$date_open>2013 & Bus3$date_open<=2016 & Bus3$is_open==0
          onlydis.ind2 <- which(onlydis.ind)
      #k) Find businesses that were founded between 2013 and 2016
          onlyop.ind <- Bus3$date_open>2013 & Bus3$date_open<=2016
          onlyop.ind2 <- which(onlyop.ind)
      #l) find minimum similarity between old and new
          #new place similarity to old ones
            newminsim <- vector()
            oldminsim <- vector()
            newmeansim <- vector()
            oldmeansim <- vector()
            for(i in 1:length(onlynew.ind2)){
                tsims <- atr3[onlynew.ind2[i], ] #load similarities to all places for ego
                tsims2 <- tsims[old.ind2] #subset to just the old places
                tsims3 <- tsims[new.ind2] #subset to just new places
                oldminsim[i] <- min(tsims2) #find the minimum similarity to old
                newminsim[i] <- sort(tsims3)[2] #find min similarity to new
                oldmeansim[i] <- mean(tsims2)
                newmeansim[i] <- mean(tsims3[-i])

            }
      #m) save results to list
          new.niche.not.outlier[q] <- sum(oldminsim>threshold & newminsim<threshold)
          new.niche.outlier[q] <- sum(newminsim>threshold & newminsim>threshold)
          new.niche[q] <- sum(oldminsim>threshold)
          new.to.old.mins[[q]] <- oldminsim
          new.to.new.mins[[q]] <- newminsim
          new.to.old.means[[q]] <- oldmeansim
          new.to.new.means[[q]] <- newmeansim

      #n) find old place similarity to new ones
          newminsim <- vector()
          oldminsim <- vector()
          newmeansim <- vector()
          oldmeansim <- vector()
          for(i in 1:length(onlyold.ind2)){
            tsims <- atr3[onlyold.ind2[i], ] #load similarities to all places for ego
            tsims2 <- tsims[onlyold.ind2] #subset to just the old places
            tsims3 <- tsims[new.ind2] #subset to just new places
            oldminsim[i] <- sort(tsims2)[2] #find the minimum similarity to old
            newminsim[i] <- min(tsims3) #find min similarity to new
            oldmeansim[i] <- mean(tsims2[-i])
            newmeansim[i] <- mean(tsims3)
          }
          dead.niche.not.outlier[q] <- sum(oldminsim>threshold & newminsim<threshold)
          dead.niche.outlier[q] <- sum(newminsim>threshold & newminsim>threshold)
          dead.niche[q] <- sum(oldminsim>threshold)
          old.to.old.mins[[q]] <- oldminsim
          old.to.new.mins[[q]] <- newminsim
          old.to.old.means[[q]] <- oldmeansim
          old.to.new.means[[q]] <- newmeansim
          
      #o) find dissolved and opened similarity to old ones
          disminsim <- vector()
          opminsim <- vector()
          dismeansim <- vector()
          opmeansim <- vector()
          for(i in 1:length(onlydis.ind2)){
            tsims <- atr3[onlydis.ind2[i], ] #load similarities to all places for ego
            tsims2 <- tsims[old.ind2] #subset to just the old places
            temp <- min(tsims2)
            disminsim[i] <- ifelse(temp==0, sort(tsims2[2]), temp)
            dismeansim[i] <- mean(tsims2)
          }
          for(i in 1:length(onlyop.ind2)){
            tsims <- atr3[onlyop.ind2[i], ] #load similarities to all places for ego
            tsims2 <- tsims[old.ind2] #subset to just the old places
            temp <- min(tsims2)
            opminsim[i] <- ifelse(temp==0, sort(tsims2[2]), temp)
            opmeansim[i] <- mean(tsims2)
          }
          dis.to.old.mins[[q]] <- disminsim
          dis.to.old.means[[q]] <- dismeansim
          op.to.old.mins[[q]] <- opminsim
          op.to.old.means[[q]] <- opmeansim
          print(q)
    }

#12) Create new df of key info/differences in cHaz
    #a) subset cHaz4 into 2013 info
        cHaz4 <- cHaz2[cHaz2$Interval==2013, ]
        cHaz5 <- cHaz2[cHaz2$Interval==2016, ]
    #b) create variables indicating time changes
        a <- (sapply(cHaz4, class))
        a <- as.vector(a=="integer" | a=="numeric")
        cHaz6 <- cHaz5[, a]-cHaz4[, a]
    #c) merge dfs
        names(cHaz4) <- paste(names(cHaz4), "2013", sep = ".")
        names(cHaz5) <- paste(names(cHaz5), "2016", sep = ".")
        names(cHaz6) <- paste(names(cHaz6), "t", sep = ".")
        cHaz7 <- cbind(cHaz4, cHaz5, cHaz6)
    #d) merge useful info into cHaz4
        tempdf <- data.frame(cluster2=clusts, new.niche=new.niche, new.niche.outlier=new.niche.outlier, new.niche.not.outlier=new.niche.not.outlier, dead.niche=dead.niche, dead.niche.outlier=dead.niche.outlier, dead.niche.not.outlier=dead.niche.not.outlier, sum.died=sum.died, sum.new=sum.new)
        tempdf[is.na(tempdf)] <- 0
        cHaz7$cluster2 <- cHaz7$cluster2.2013
        cHaz7 <- merge(tempdf, cHaz7, by="cluster2")
        cHaz7 <- data.table(cHaz7)
        cHaz7$new.to.old.mins <- new.to.old.mins
        cHaz7$new.to.new.mins <- new.to.new.mins
        cHaz7$new.to.old.means <- new.to.old.means
        cHaz7$new.to.new.means <- new.to.new.means
        cHaz7$old.to.old.mins <- old.to.old.mins
        cHaz7$old.to.new.mins <- old.to.new.mins
        cHaz7$old.to.old.means <- old.to.old.means
        cHaz7$old.to.new.means <- old.to.new.means
        cHaz7$dis.to.old.mins <- dis.to.old.mins
        cHaz7$dis.to.old.means <- dis.to.old.means
        cHaz7$op.to.old.mins <- op.to.old.mins
        cHaz7$op.to.old.means <- op.to.old.means
        
#13) Save output
    saveRDS(cHaz7, "2013-2016_changes.rds")
}