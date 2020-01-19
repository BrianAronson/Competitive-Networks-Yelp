#' pap2 - 5 - Prep model variables
#' @export
#' @import data.table 
#' @import RCurl 
#' @import RJSONIO 
#' @import acs 
#' @import ggplot2 
#' @import tigris
#' @import sp 
#' @import raster 

yelp_tract_model_vars  <-  function(Directory, City){
#1) load data
    ch <- readRDS("2013-2016_changes.rds")

#2) #Rename variables and remove duplicates
    names(ch) <- gsub("2011.2013", "2011", names(ch))
    names(ch) <- gsub("2012.2013", "2012", names(ch))
    names(ch) <- gsub("2013.2013", "2013", names(ch))
    names(ch) <- gsub("2014.2013", "2014", names(ch))
    names(ch) <- gsub("2015.2013", "2015", names(ch))
    names(ch) <- gsub("2016.2013", "2016", names(ch))
    names(ch) <- gsub("2011.2016", "2011", names(ch))
    names(ch) <- gsub("2012.2016", "2012", names(ch))
    names(ch) <- gsub("2013.2016", "2013", names(ch))
    names(ch) <- gsub("2014.2016", "2014", names(ch))
    names(ch) <- gsub("2015.2016", "2015", names(ch))
    names(ch) <- gsub("2016.2016", "2016", names(ch))
    ch <- as.data.frame(ch)
    ch <- ch[, !(duplicated(names(ch)))]
    a <- sapply(ch, function(x) length(unique(x)))        
    ch <- ch[, a!=1]
    ch <- data.table(ch)
    
#3) #transform outliers
        ch$Income2013 <- ifelse(ch$Income2013<quantile(ch$Income2013, .05, na.rm=T), quantile(ch$Income2013, .05, na.rm=T), ch$Income2013)
        ch$Income2016 <- ifelse(ch$Income2016<quantile(ch$Income2016, .05, na.rm=T), quantile(ch$Income2016, .05, na.rm=T), ch$Income2016)
        
#4) Create popdens variable
    ch$popdens2013 <- ch$pop2013/ch$area.2013
    ch$popdens2016 <- ch$pop2016/ch$area.2016
    
#5) Create variable indicate percent of business founded in a new niche
    ch$p.new.niche <- ch$new.niche/ch$alive.2016
    ch$p.new.niche.outlier <- ch$new.niche.outlier/ch$alive.2016
    ch$p.new.niche.not.outlier <- ch$new.niche.not.outlier/ch$alive.2016
    ch$p.dead.niche <- ch$dead.niche/ch$alive.2013
    ch$p.dead.niche.outlier <- ch$dead.niche.outlier/ch$alive.2013
    ch$p.dead.niche.not.outlier <- ch$dead.niche.not.outlier/ch$alive.2013
    ch$p.sum.new <- ch$sum.new/ch$alive.2016
    ch$p.sum.died <- ch$sum.died/ch$alive.2013
    ch$p.N <- (ch$alive.2016+ch$alive.t)/ch$alive.2016
    
#6) create change variables
    ch$Income.t <- ch$Income2016-ch$Income2013
    ch$Migrants.t <- ch$Migrants2016-ch$Migrants2013
    ch$popdens.t <- ch$popdens2016-ch$popdens2013
    ch$White.t <- ch$White2016-ch$White2013
    ch$Vacant.t <- ch$Vacant2016-ch$Vacant2013
    ch$internationalMigrants.t <- ch$internationalMigrants2016-ch$internationalMigrants2013
    ch$Unemploy.t <- ch$Unemploy2016-ch$Unemploy2013
    ch$p.Income.t <- ch$Income2016/ch$Income2013
    ch$p.Migrants.t <- ch$Migrants2016/ch$Migrants2013
    ch$p.popdens.t <- ch$popdens2016/ch$popdens2013
    ch$p.White.t <- ch$White2016/ch$White2013
    ch$p.Vacant.t <- ch$Vacant2016/ch$Vacant2013
    ch$p.internationalMigrants.t <- ch$internationalMigrants2016/ch$internationalMigrants2013
    ch$p.Unemploy.t <- ch$Unemploy2016/ch$Unemploy2013
    ch$p.Migrants.t[is.infinite(ch$p.Migrants.t)] <- sort(unique(ch$p.Migrants.t), decreasing=T)[2]
    ch$p.internationalMigrants.t[is.infinite(ch$p.internationalMigrants.t)] <- sort(unique(ch$p.internationalMigrants.t), decreasing=T)[2]
    
#7) create positive and negative change variables
    ch$p.Income.t.p <- ch$p.Income.t-1
    ch$p.Income.t.p[ch$p.Income.t.p<0] <- 0
    ch$p.Income.t.n <- ch$p.Income.t-1
    ch$p.Income.t.n[ch$p.Income.t.p>0] <- 0
    ch$p.Income.t.n <- abs(ch$p.Income.t.n)
    ch$p.popdens.t.p <- ch$p.popdens.t-1
    ch$p.popdens.t.p[ch$p.popdens.t.p<0] <- 0
    ch$p.popdens.t.n <- ch$p.popdens.t-1
    ch$p.popdens.t.n[ch$p.popdens.t.p>0] <- 0
    ch$p.popdens.t.n <- abs(ch$p.popdens.t.n)
    ch$p.Migrants.t.p <- ch$p.Migrants.t-1
    ch$p.Migrants.t.p[ch$p.Migrants.t.p<0] <- 0
    ch$p.Migrants.t.n <- ch$p.Migrants.t-1
    ch$p.Migrants.t.n[ch$p.Migrants.t.p>0] <- 0
    ch$p.Migrants.t.n <- abs(ch$p.Migrants.t.n)
    ch$p.White.t.p <- ch$p.White.t-1
    ch$p.White.t.p[ch$p.White.t.p<0] <- 0
    ch$p.White.t.n <- ch$p.White.t-1
    ch$p.White.t.n[ch$p.White.t.p>0] <- 0
    ch$p.White.t.n <- abs(ch$p.White.t.n)
    ch$p.Vacant.t.p <- ch$p.Vacant.t-1
    ch$p.Vacant.t.p[ch$p.Vacant.t.p<0] <- 0
    ch$p.Vacant.t.n <- ch$p.Vacant.t-1
    ch$p.Vacant.t.n[ch$p.Vacant.t.p>0] <- 0
    ch$p.Vacant.t.n <- abs(ch$p.Vacant.t.n)
    ch$p.internationalMigrants.t.p <- ch$p.internationalMigrants.t-1
    ch$p.internationalMigrants.t.p[ch$p.internationalMigrants.t.p<0] <- 0
    ch$p.internationalMigrants.t.n <- ch$p.internationalMigrants.t-1
    ch$p.internationalMigrants.t.n[ch$p.internationalMigrants.t.p>0] <- 0
    ch$p.internationalMigrants.t.n <- abs(ch$p.internationalMigrants.t.n)
    ch$p.Unemploy.t.p <- ch$p.Unemploy.t-1
    ch$p.Unemploy.t.p[ch$p.Unemploy.t.p<0] <- 0
    ch$p.Unemploy.t.n <- ch$p.Unemploy.t-1
    ch$p.Unemploy.t.n[ch$p.Unemploy.t.p>0] <- 0
    ch$p.Unemploy.t.n <- abs(ch$p.Unemploy.t.n)
    
#8) Control variables
    ch$age.t <- ch$age.2016-ch$age.2013
    ch$chain.t <- ch$chain.2016-ch$chain.2013
    ch$chain_super.t <- ch$chain_super.2016-ch$chain_super.2013
    ch$dist_center.t <- ch$dist_center.2016-ch$dist_center.2013
    ch$Price.t <- ch$Price.2016-ch$Price.2013
    ch$rev_count.t <- ch$rev_count.2016-ch$rev_count.2013
    ch$stars.t <- ch$stars.2016-ch$stars.2013
    ch$p.chain.2013 <- ch$chain.2013/ch$alive.2013
    ch$p.chain.t <- ch$chain.t/ch$alive.2013
    ch$p.chain_super.2013 <- ch$chain_super.2013/ch$alive.2013
    ch$p.chain_super.t <- ch$chain_super.t/ch$alive.2013
    ch$maxDist <- ch$maxDist.2013
    ch$meanDist <- ch$meanDist.2013
    ch$area <- ch$area.2013
    ch$modules.2013 <- ch$clusters.2013
    ch$modules.2016 <- ch$clusters.2016
    ch$modules.t <- ch$clusters.t
    ch$Income2013 <- ch$Income2013/1000
    ch$Income2016 <- ch$Income2016/1000
    ch$White2013 <- ch$White2013*100
    ch$White2016 <- ch$White2016*100
    ch$Migrants2013 <- ch$Migrants2013*100
    ch$Migrants2016 <- ch$Migrants2016*100
    ch$popdens2013 <- ch$popdens2013/1000
    ch$popdens2016 <- ch$popdens2016/1000
        
#9) fix outliers
    ch$p.new.niche.not.outlier2 <- ch$p.new.niche.not.outlier
    ch$p.new.niche.not.outlier2[ch$p.new.niche.not.outlier>.075] <- .075
    ch$modularity.20132 <- ch$modularity.2013^2
    ch$modularity.20132 <- ch$modularity.2013^2

#10) create a variable that indicates whether a new restaurant is much more similar to any new places than any old
    a <- unlist(ch$new.to.old.mins)
    b <- unlist(ch$new.to.new.mins)
    d <- a-b
    d[d<0] <- 0
    d[d>.1] <- .1 #cases where new less similar to old than to new
    lens <- sapply(ch$new.to.old.mins, length)
    df1 <- vector()
    df2 <- vector()
    for(i in 1:length(lens)){
      df1 <- c(df1, rep(i, each=lens[i]))
      df2 <- c(df2, 1:lens[i])
    }
    a <- ch$new.to.old.mins
    for(i in 1:length(df1)){
      a[[df1[i]]][df2[i]] <- d[i]
    }
    ch$nc1b <- sapply(a, function(x) sum(x>0))
    ch$nc2b <- sapply(a, function(x) sum(x>0)/length(x))
    ch$nc3b <- sapply(a, function(x) mean(x[x>0], na.rm=T))
    ch$nc3b[is.nan(ch$nc3b)] <- 0

#11) create a variables that indicate whether an old restaurant is much more similar to any new places than any old
    a <- unlist(ch$old.to.old.mins)
    b <- unlist(ch$old.to.new.mins)
    d <- b-a
    d[d<0] <- 0
    d[d>.1] <- .1 #cases where new less similar to old than to new
    lens <- sapply(ch$old.to.new.mins, length)
    df1 <- vector()
    df2 <- vector()
    for(i in 1:length(lens)){
      df1 <- c(df1, rep(i, each=lens[i]))
      df2 <- c(df2, 1:lens[i])
    }
    a <- ch$old.to.new.mins
    for(i in 1:length(df1)){
      a[[df1[i]]][df2[i]] <- d[i]
    }
    ch$nc1a <- sapply(a, function(x) sum(x>0))
    ch$nc2a <- sapply(a, function(x) sum(x>0)/length(x))
    ch$nc3a <- sapply(a, function(x) mean(x[x>0], na.rm=T))
    ch$nc3a[is.nan(ch$nc3a)] <- 0
    
#12) create variables based both types of change variables
    ch$nc1c <- ch$nc1a+ch$nc1b
    ch$nc2c <- ch$nc2a+ch$nc2b
    ch$nc3c <- ch$nc3a+ch$nc3b

#13) Prepare key change variables
    ch$mean.new.to.old.mins <- sapply(ch$new.to.old.mins, mean, na.rm=T)
    ch$mean.new.to.new.mins <- sapply(ch$new.to.new.mins, mean, na.rm=T)
    ch$mean.new.to.old.means <- sapply(ch$new.to.old.means, mean, na.rm=T)
    ch$mean.new.to.new.means <- sapply(ch$new.to.new.means, mean, na.rm=T)
    ch$mean.old.to.old.mins <- sapply(ch$old.to.old.mins, mean, na.rm=T)
    ch$mean.old.to.new.mins <- sapply(ch$old.to.new.mins, mean, na.rm=T)
    ch$mean.old.to.old.means <- sapply(ch$old.to.old.means, mean, na.rm=T)
    ch$mean.old.to.new.means <- sapply(ch$old.to.new.means, mean, na.rm=T)
    ch$mean.dis.to.old.mins <- sapply(ch$dis.to.old.mins, mean, na.rm=T)
    ch$mean.dis.to.old.means <- sapply(ch$dis.to.old.means, mean, na.rm=T)
    ch$mean.op.to.old.mins <- sapply(ch$op.to.old.mins, mean, na.rm=T)
    ch$mean.op.to.old.means <- sapply(ch$op.to.old.means, mean, na.rm=T)
    ch$d.newmeans <- ch$mean.new.to.new.means-ch$mean.new.to.old.means
    ch$d.oldmeans <- ch$mean.old.to.new.means-ch$mean.old.to.old.means
    ch$nichechange <- ch$d.newmeans*(-1) + ch$d.oldmeans
        
#14) save changes
    saveRDS(ch, "2013-2016_changes.rds")
}        
 