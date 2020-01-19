#' 2 - Cluster census tracts
#' @export
#' @import data.table 
#' @import RCurl 
#' @import RJSONIO 
#' @import acs 
#' @import ggplot2 
#' @import tigris
#' @import sp 
#' @import raster 

yelp_cenclust <- function(Directory, City){
#1 - load data
    setwd(Directory)
    Bus <- readRDS("Restaurants.rds")
    Cen <- readRDS("Censusinfo.rds")
    Bus <- Bus[Bus$city_super==City, ]
    Cen <- Cen[Cen$id%in%Bus$id, ]
    Cust <- readRDS(paste(City, "_cust.rds", sep=""))
    Dist <- readRDS(paste(City, "_dist.rds", sep=""))
    cendf <- readRDS(paste(City, "_cendf.rds", sep=""))
     
#2 - reduce tie dataframe to census level
    #bring in business IDs
        Cust <- data.table(ID1=rep(Bus$id, each=length(Bus$id)), 
             ID2=rep(Bus$id, length(Bus$id)), 
             Cust=Cust, 
             Dist=Dist
        )
    #bring in GEOIDs
        temp <- Cen[, c("id", "GEOID")]
        names(temp) <- c("ID1", "GEOID1")
        Cust <- merge(Cust, temp, by="ID1")
        names(temp) <- c("ID2", "GEOID2")
        Cust <- merge(Cust, temp, by="ID2")
    #Create df where ties are by GEOID
    {  
        GEO <- Cust[, .(minDist=min(Dist), meanDist2=mean(Dist), Cust=sum(Cust)), by = c("GEOID1", "GEOID2")]
        GEO <- GEO[order(GEO$GEOID1, GEO$GEOID2), ]
    #find mean latitude and longitude of geoids
        cendf <- cendf[match(unique(GEO$GEOID1), cendf$GEOID), ]
        temp <- cendf[, c("GEOID", "latitude", "longitude", "N")]
        names(temp) <- paste(names(temp), "1", sep="")
        GEO <- merge(GEO, temp, by="GEOID1")
        temp <- cendf[, c("GEOID", "latitude", "longitude", "N")]
        names(temp) <- paste(names(temp), "2", sep="")
        GEO <- merge(GEO, temp, by="GEOID2")
    #Remove any missings
        cendf <- cendf[!is.na(cendf$GEOID), ]
        GEO <- GEO[!is.na(GEO$GEOID1) & !is.na(GEO$GEOID2), ]
    #Estimate distances between tract centers
        a <- cbind(cendf$longitude, cendf$latitude)
        temp <- c(distm(a))*0.000621371
        GEO$dist <- temp
    #get tie counts by GEOID
        temp <- GEO[, .(Sum=sum(Cust)), by="GEOID1"]
        GEO <- merge(GEO, temp, by="GEOID1")
    #get proportion by outdegree
        set.seed(1)
        GEO$cluster1 <- rep(1:sqrt(nrow(GEO)), each=sqrt(nrow(GEO)))
        GEO$cluster2 <- rep(1:sqrt(nrow(GEO)), sqrt(nrow(GEO)))
        GEO$Prop <- GEO$Cust/(GEO$N1* GEO$N2)
        GEO$Cust <- as.numeric(GEO$Cust)
      }
    #cluster by increasing thresholds iteratively, 
      for(i in 1:10){
        GEO$Prop <- 1
        GEO$Prop[GEO$dist>(i/8)] <- 0
        GEO$Prop[GEO$N1>80 | GEO$N2>80] <- 0
        if(i>1){
          #subtract i to lower the tendency for too distant places to converge
          GEO$Prop[(GEO$N11>80-i*4 | GEO$N21>80-i*4) & GEO$cluster1!=GEO$cluster2] <- 0
          GEO$Prop2 <- GEO$Cust1/(GEO$N11* GEO$N21)
          a <- GEO$N11>10 |GEO$N21>10
          GEO$Prop2[a] <- ifelse(GEO$Prop2[a]>quantile(GEO$Prop2[a], (.9-(i*(.9/10)))), 1, 0)
          # GEO$Prop[GEO$Prop2==0] <- 0
        }
        geomat <- matrix(GEO$Prop, nrow=sqrt(nrow(GEO)))
        g <- graph_from_adjacency_matrix(geomat)
      #Cluster
        cluster <- cluster_louvain(as.undirected(g))$membership
        GEO$cluster1 <- rep(cluster, each=length(cluster))
        GEO$cluster2 <- rep(cluster, length(cluster))
      #estimate N by cluster
        temp <- GEO[!duplicated(GEO$GEOID1), ]
        temp <- temp[, .(GEOID1=GEOID1, N1=sum(N1), Cust=sum(Cust)), by="cluster1"]
        temp <- temp[order(temp$GEOID1), ]
        GEO$N11 <- rep(temp$N1, each=nrow(temp))
        GEO$N21 <- rep(temp$N1, nrow(temp))
        GEO$Cust1 <- rep(temp$Cust, each=nrow(temp))
      }
      cendf$cluster <- cluster

        
#3 - Create df by census tract
        Bus2 <- cbind(Cen, Bus)
        temp <- cendf[, c("GEOID", "cluster")]
        Bus2 <- merge(Bus2, temp, by="GEOID")
        clustdf <- Bus2[!duplicated((Bus2$cluster)), c("cluster", "area", "Migrants2011", "Migrants2012", "Migrants2013", "Migrants2014", "Migrants2015", "Migrants2016", "Income2011", "Income2012", "Income2013", "Income2014", "Income2015", "Income2016", "Unemploy2011", "Unemploy2012", "Unemploy2013", "Unemploy2014", "Unemploy2015", "Unemploy2016", "Vacant2011", "Vacant2012", "Vacant2013", "Vacant2014", "Vacant2015", "Vacant2016", "White2011", "White2012", "White2013", "White2014", "White2015", "White2016", "popdens2011", "popdens2012", "popdens2013", "popdens2014", "popdens2015", "popdens2016", "internationalMigrants2011", "internationalMigrants2012", "internationalMigrants2013", "internationalMigrants2014", "internationalMigrants2015", "internationalMigrants2016", "city", "city_super")]
    # Create useful variables
        clustdf$N <- sapply(clustdf$cluster, function(x) sum(Bus2$cluster==x, na.rm=T))
        clustdf$is_open <- sapply(clustdf$cluster, function(x) sum(Bus2$is_open[Bus2$cluster==x], na.rm=T))
        clustdf$had_closed <- clustdf$N-clustdf$is_open
        clustdf$rev_count <- sapply(clustdf$cluster, function(x) sum(Bus2$rev_count[Bus2$cluster==x], na.rm=T))
        clustdf$rev_count_nonelites <- sapply(clustdf$cluster, function(x) sum(Bus2$rev_count_nonelites[Bus2$cluster==x], na.rm=T))
        clustdf$chain <- sapply(clustdf$cluster, function(x) sum(Bus2$chain[Bus2$cluster==x], na.rm=T))
        clustdf$chain_super <- sapply(clustdf$cluster, function(x) sum(Bus2$chain_super[Bus2$cluster==x], na.rm=T))
        clustdf$latitude <- sapply(clustdf$cluster, function(x) mean(Bus2$latitude[Bus2$cluster==x], na.rm=T))
        clustdf$longitude <- sapply(clustdf$cluster, function(x) mean(Bus2$longitude[Bus2$cluster==x], na.rm=T))
        clustdf$stars <- sapply(clustdf$cluster, function(x) mean(Bus2$stars[Bus2$cluster==x], na.rm=T))
        clustdf$age <- sapply(clustdf$cluster, function(x) mean(Bus2$age[Bus2$cluster==x], na.rm=T))
        clustdf$Price <- sapply(clustdf$cluster, function(x) mean(Bus2$Price[Bus2$cluster==x], na.rm=T))
        clustdf$Hour_Open <- sapply(clustdf$cluster, function(x) mean(Bus2$Hour_Open[Bus2$cluster==x], na.rm=T))
        clustdf$dist_center <- sapply(clustdf$cluster, function(x) mean(Bus2$dist_center[Bus2$cluster==x], na.rm=T))
        clustdf$Hour_Close <- sapply(clustdf$cluster, function(x) mean(Bus2$Hour_Close[Bus2$cluster==x], na.rm=T))
        clustdf$area <- NULL
        clustdf$area <- sapply(clustdf$cluster, function(x) sum(cendf$area[cendf$cluster==x], na.rm=T))
        row.names(clustdf) <- as.character(clustdf$cluster)
        
    # square areas of businesses
        area2 <- vector()
        for(i in 1:nrow(clustdf)){
          coords <- cbind(c(min(Bus2$longitude[Bus2$cluster==i]), min(Bus2$longitude[Bus2$cluster==i]), max(Bus2$longitude[Bus2$cluster==i]), max(Bus2$longitude[Bus2$cluster==i])), 
                        c(min(Bus2$latitude[Bus2$cluster==i]), max(Bus2$latitude[Bus2$cluster==i]), max(Bus2$latitude[Bus2$cluster==i]), min(Bus2$latitude[Bus2$cluster==i])))
          area2[i] <- areaPolygon(coords)*3.86102e-7  
        }
        clustdf$area2 <- area2
    
    #create more variables
        temp <- Bus2[, c("id", "cluster")]
        names(temp) <- c("ID1", "cluster1")
        Cust2 <- merge(Cust, temp, by="ID1")
        names(temp) <- c("ID2", "cluster2")
        Cust2 <- merge(Cust2, temp, by="ID2")
        
        Cust3 <- Cust2[GEOID1==GEOID2, ]
        GEO2 <- Cust3[, .(maxDist=max(Dist), meanDist=mean(Dist)), by = c("GEOID1")]
        cendf <- cendf[match(GEO2$GEOID1, cendf$GEOID), ]
        cendf$maxDist <- GEO2$maxDist
        cendf$meanDist <- GEO2$meanDist
        
        Cust3 <- Cust2[cluster1==cluster2, ]
        GEO2 <- Cust3[, .(maxDist=max(Dist), meanDist=mean(Dist)), by = c("cluster1")]
        clustdf <- clustdf[match(GEO2$cluster1, clustdf$cluster), ]
        clustdf$maxDist <- GEO2$maxDist
        clustdf$meanDist <- GEO2$meanDist
        rm(Cust2)
        gc()
        
    #Create 2013 and 2016 versions of the above
          datefun <- function(x) as.numeric(substr(x, 1, 4))+(as.numeric(substr(x, 6, 7))-1)/12 + (as.numeric(substr(x, 9, 10))-1)/365
          Bus2$date_open <- datefun(Bus2$date_open)
          Bus2$date_close <- datefun(Bus2$date_close)
          Bus.2013 <- Bus2[Bus2$date_open<=2013 & (Bus2$date_close>2013 | Bus2$is_open==1), ]
          Bus.2016 <- Bus2[Bus2$date_open<=2016 & (Bus2$date_close>2016 | Bus2$is_open==1), ]
          #2013
              clustdf$N.2013 <- sapply(clustdf$cluster, function(x) sum(Bus.2013$cluster==x, na.rm=T))
              clustdf$is_open.2013 <- sapply(clustdf$cluster, function(x) sum(Bus.2013$is_open[Bus.2013$cluster==x], na.rm=T))
              clustdf$had_closed.2013 <- clustdf$N-clustdf$is_open
              clustdf$rev_count.2013 <- sapply(clustdf$cluster, function(x) sum(Bus.2013$rev_count[Bus.2013$cluster==x], na.rm=T))
              clustdf$rev_count_nonelites.2013 <- sapply(clustdf$cluster, function(x) sum(Bus.2013$rev_count_nonelites[Bus.2013$cluster==x], na.rm=T))
              clustdf$chain.2013 <- sapply(clustdf$cluster, function(x) sum(Bus.2013$chain[Bus.2013$cluster==x], na.rm=T))
              clustdf$chain_super.2013 <- sapply(clustdf$cluster, function(x) sum(Bus.2013$chain_super[Bus.2013$cluster==x], na.rm=T))
              clustdf$latitude.2013 <- sapply(clustdf$cluster, function(x) mean(Bus.2013$latitude[Bus.2013$cluster==x], na.rm=T))
              clustdf$longitude.2013 <- sapply(clustdf$cluster, function(x) mean(Bus.2013$longitude[Bus.2013$cluster==x], na.rm=T))
              clustdf$stars.2013 <- sapply(clustdf$cluster, function(x) mean(Bus.2013$stars[Bus.2013$cluster==x], na.rm=T))
              Bus.2013$age <- 2013-Bus.2013$date_open
              clustdf$age.2013 <- sapply(clustdf$cluster, function(x) mean(Bus.2013$age[Bus.2013$cluster==x], na.rm=T))
              clustdf$Price.2013 <- sapply(clustdf$cluster, function(x) mean(Bus.2013$Price[Bus.2013$cluster==x], na.rm=T))
              clustdf$Hour_Open.2013 <- sapply(clustdf$cluster, function(x) mean(Bus.2013$Hour_Open[Bus.2013$cluster==x], na.rm=T))
              clustdf$dist_center.2013 <- sapply(clustdf$cluster, function(x) mean(Bus.2013$dist_center[Bus.2013$cluster==x], na.rm=T))
              clustdf$Hour_Close.2013 <- sapply(clustdf$cluster, function(x) mean(Bus.2013$Hour_Close[Bus.2013$cluster==x], na.rm=T))
          #2016
              clustdf$N.2016 <- sapply(clustdf$cluster, function(x) sum(Bus.2016$cluster==x, na.rm=T))
              clustdf$is_open.2016 <- sapply(clustdf$cluster, function(x) sum(Bus.2016$is_open[Bus.2016$cluster==x], na.rm=T))
              clustdf$had_closed.2016 <- clustdf$N-clustdf$is_open
              clustdf$rev_count.2016 <- sapply(clustdf$cluster, function(x) sum(Bus.2016$rev_count[Bus.2016$cluster==x], na.rm=T))
              clustdf$rev_count_nonelites.2016 <- sapply(clustdf$cluster, function(x) sum(Bus.2016$rev_count_nonelites[Bus.2016$cluster==x], na.rm=T))
              clustdf$chain.2016 <- sapply(clustdf$cluster, function(x) sum(Bus.2016$chain[Bus.2016$cluster==x], na.rm=T))
              clustdf$chain_super.2016 <- sapply(clustdf$cluster, function(x) sum(Bus.2016$chain_super[Bus.2016$cluster==x], na.rm=T))
              clustdf$latitude.2016 <- sapply(clustdf$cluster, function(x) mean(Bus.2016$latitude[Bus.2016$cluster==x], na.rm=T))
              clustdf$longitude.2016 <- sapply(clustdf$cluster, function(x) mean(Bus.2016$longitude[Bus.2016$cluster==x], na.rm=T))
              clustdf$stars.2016 <- sapply(clustdf$cluster, function(x) mean(Bus.2016$stars[Bus.2016$cluster==x], na.rm=T))
              Bus.2016$age <- 2016-Bus.2016$date_open
              clustdf$age.2016 <- sapply(clustdf$cluster, function(x) mean(Bus.2016$age[Bus.2016$cluster==x], na.rm=T))
              clustdf$Price.2016 <- sapply(clustdf$cluster, function(x) mean(Bus.2016$Price[Bus.2016$cluster==x], na.rm=T))
              clustdf$Hour_Open.2016 <- sapply(clustdf$cluster, function(x) mean(Bus.2016$Hour_Open[Bus.2016$cluster==x], na.rm=T))
              clustdf$dist_center.2016 <- sapply(clustdf$cluster, function(x) mean(Bus.2016$dist_center[Bus.2016$cluster==x], na.rm=T))
              clustdf$Hour_Close.2016 <- sapply(clustdf$cluster, function(x) mean(Bus.2016$Hour_Close[Bus.2016$cluster==x], na.rm=T))
                  
#4 - aggregate census variables for clustdf
    #a) create population variables
        cendf$pop2011 <-  cendf$popdens2011* cendf$area
        cendf$pop2012 <-  cendf$popdens2012* cendf$area
        cendf$pop2013 <-  cendf$popdens2013* cendf$area
        cendf$pop2014 <-  cendf$popdens2014* cendf$area
        cendf$pop2015 <-  cendf$popdens2015* cendf$area
        cendf$pop2016 <-  cendf$popdens2016* cendf$area
    #b) aggregate populations in clust
        clustdf$pop2011 <- sapply(clustdf$cluster, function(x) sum(cendf$pop2011[cendf$cluster==x], na.rm=T))
        clustdf$pop2012 <- sapply(clustdf$cluster, function(x) sum(cendf$pop2012[cendf$cluster==x], na.rm=T))
        clustdf$pop2013 <- sapply(clustdf$cluster, function(x) sum(cendf$pop2013[cendf$cluster==x], na.rm=T))
        clustdf$pop2014 <- sapply(clustdf$cluster, function(x) sum(cendf$pop2014[cendf$cluster==x], na.rm=T))
        clustdf$pop2015 <- sapply(clustdf$cluster, function(x) sum(cendf$pop2015[cendf$cluster==x], na.rm=T))
        clustdf$pop2016 <- sapply(clustdf$cluster, function(x) sum(cendf$pop2016[cendf$cluster==x], na.rm=T))
    #c) aggeregate census info into clusters
        clustdf$internationalMigrants2011 <- sapply(clustdf$cluster, function(x) sum(cendf$internationalMigrants2011[cendf$cluster==x]*cendf$pop2011[cendf$cluster==x])/sum(cendf$pop2011[cendf$cluster==x]))
        clustdf$internationalMigrants2012 <- sapply(clustdf$cluster, function(x) sum(cendf$internationalMigrants2012[cendf$cluster==x]*cendf$pop2012[cendf$cluster==x])/sum(cendf$pop2012[cendf$cluster==x]))
        clustdf$internationalMigrants2013 <- sapply(clustdf$cluster, function(x) sum(cendf$internationalMigrants2013[cendf$cluster==x]*cendf$pop2013[cendf$cluster==x])/sum(cendf$pop2013[cendf$cluster==x]))
        clustdf$internationalMigrants2014 <- sapply(clustdf$cluster, function(x) sum(cendf$internationalMigrants2014[cendf$cluster==x]*cendf$pop2014[cendf$cluster==x])/sum(cendf$pop2014[cendf$cluster==x]))
        clustdf$internationalMigrants2015 <- sapply(clustdf$cluster, function(x) sum(cendf$internationalMigrants2015[cendf$cluster==x]*cendf$pop2015[cendf$cluster==x])/sum(cendf$pop2015[cendf$cluster==x]))
        clustdf$internationalMigrants2016 <- sapply(clustdf$cluster, function(x) sum(cendf$internationalMigrants2016[cendf$cluster==x]*cendf$pop2016[cendf$cluster==x])/sum(cendf$pop2016[cendf$cluster==x]))
        
        clustdf$Migrants2011 <- sapply(clustdf$cluster, function(x) sum(cendf$Migrants2011[cendf$cluster==x]*cendf$pop2011[cendf$cluster==x])/sum(cendf$pop2011[cendf$cluster==x]))
        clustdf$Migrants2012 <- sapply(clustdf$cluster, function(x) sum(cendf$Migrants2012[cendf$cluster==x]*cendf$pop2012[cendf$cluster==x])/sum(cendf$pop2012[cendf$cluster==x]))
        clustdf$Migrants2013 <- sapply(clustdf$cluster, function(x) sum(cendf$Migrants2013[cendf$cluster==x]*cendf$pop2013[cendf$cluster==x])/sum(cendf$pop2013[cendf$cluster==x]))
        clustdf$Migrants2014 <- sapply(clustdf$cluster, function(x) sum(cendf$Migrants2014[cendf$cluster==x]*cendf$pop2014[cendf$cluster==x])/sum(cendf$pop2014[cendf$cluster==x]))
        clustdf$Migrants2015 <- sapply(clustdf$cluster, function(x) sum(cendf$Migrants2015[cendf$cluster==x]*cendf$pop2015[cendf$cluster==x])/sum(cendf$pop2015[cendf$cluster==x]))
        clustdf$Migrants2016 <- sapply(clustdf$cluster, function(x) sum(cendf$Migrants2016[cendf$cluster==x]*cendf$pop2016[cendf$cluster==x])/sum(cendf$pop2016[cendf$cluster==x]))
        
        
        clustdf$Income2011 <- sapply(clustdf$cluster, function(x) sum(cendf$Income2011[cendf$cluster==x]*cendf$pop2011[cendf$cluster==x])/sum(cendf$pop2011[cendf$cluster==x]))
        clustdf$Income2012 <- sapply(clustdf$cluster, function(x) sum(cendf$Income2012[cendf$cluster==x]*cendf$pop2012[cendf$cluster==x])/sum(cendf$pop2012[cendf$cluster==x]))
        clustdf$Income2013 <- sapply(clustdf$cluster, function(x) sum(cendf$Income2013[cendf$cluster==x]*cendf$pop2013[cendf$cluster==x])/sum(cendf$pop2013[cendf$cluster==x]))
        clustdf$Income2014 <- sapply(clustdf$cluster, function(x) sum(cendf$Income2014[cendf$cluster==x]*cendf$pop2014[cendf$cluster==x])/sum(cendf$pop2014[cendf$cluster==x]))
        clustdf$Income2015 <- sapply(clustdf$cluster, function(x) sum(cendf$Income2015[cendf$cluster==x]*cendf$pop2015[cendf$cluster==x])/sum(cendf$pop2015[cendf$cluster==x]))
        clustdf$Income2016 <- sapply(clustdf$cluster, function(x) sum(cendf$Income2016[cendf$cluster==x]*cendf$pop2016[cendf$cluster==x])/sum(cendf$pop2016[cendf$cluster==x]))
        clustdf$Unemploy2011 <- sapply(clustdf$cluster, function(x) sum(cendf$Unemploy2011[cendf$cluster==x]*cendf$pop2011[cendf$cluster==x])/sum(cendf$pop2011[cendf$cluster==x]))
        clustdf$Unemploy2012 <- sapply(clustdf$cluster, function(x) sum(cendf$Unemploy2012[cendf$cluster==x]*cendf$pop2012[cendf$cluster==x])/sum(cendf$pop2012[cendf$cluster==x]))
        clustdf$Unemploy2013 <- sapply(clustdf$cluster, function(x) sum(cendf$Unemploy2013[cendf$cluster==x]*cendf$pop2013[cendf$cluster==x])/sum(cendf$pop2013[cendf$cluster==x]))
        clustdf$Unemploy2014 <- sapply(clustdf$cluster, function(x) sum(cendf$Unemploy2014[cendf$cluster==x]*cendf$pop2014[cendf$cluster==x])/sum(cendf$pop2014[cendf$cluster==x]))
        clustdf$Unemploy2015 <- sapply(clustdf$cluster, function(x) sum(cendf$Unemploy2015[cendf$cluster==x]*cendf$pop2015[cendf$cluster==x])/sum(cendf$pop2015[cendf$cluster==x]))
        clustdf$Unemploy2016 <- sapply(clustdf$cluster, function(x) sum(cendf$Unemploy2016[cendf$cluster==x]*cendf$pop2016[cendf$cluster==x])/sum(cendf$pop2016[cendf$cluster==x]))
        clustdf$White2011 <- sapply(clustdf$cluster, function(x) sum(cendf$White2011[cendf$cluster==x]*cendf$pop2011[cendf$cluster==x])/sum(cendf$pop2011[cendf$cluster==x]))
        clustdf$White2012 <- sapply(clustdf$cluster, function(x) sum(cendf$White2012[cendf$cluster==x]*cendf$pop2012[cendf$cluster==x])/sum(cendf$pop2012[cendf$cluster==x]))
        clustdf$White2013 <- sapply(clustdf$cluster, function(x) sum(cendf$White2013[cendf$cluster==x]*cendf$pop2013[cendf$cluster==x])/sum(cendf$pop2013[cendf$cluster==x]))
        clustdf$White2014 <- sapply(clustdf$cluster, function(x) sum(cendf$White2014[cendf$cluster==x]*cendf$pop2014[cendf$cluster==x])/sum(cendf$pop2014[cendf$cluster==x]))
        clustdf$White2015 <- sapply(clustdf$cluster, function(x) sum(cendf$White2015[cendf$cluster==x]*cendf$pop2015[cendf$cluster==x])/sum(cendf$pop2015[cendf$cluster==x]))
        clustdf$White2016 <- sapply(clustdf$cluster, function(x) sum(cendf$White2016[cendf$cluster==x]*cendf$pop2016[cendf$cluster==x])/sum(cendf$pop2016[cendf$cluster==x]))
        clustdf$popdens2011 <- sapply(clustdf$cluster, function(x) sum(cendf$popdens2011[cendf$cluster==x]*cendf$pop2011[cendf$cluster==x])/sum(cendf$pop2011[cendf$cluster==x]))
        clustdf$popdens2012 <- sapply(clustdf$cluster, function(x) sum(cendf$popdens2012[cendf$cluster==x]*cendf$pop2012[cendf$cluster==x])/sum(cendf$pop2012[cendf$cluster==x]))
        clustdf$popdens2013 <- sapply(clustdf$cluster, function(x) sum(cendf$popdens2013[cendf$cluster==x]*cendf$pop2013[cendf$cluster==x])/sum(cendf$pop2013[cendf$cluster==x]))
        clustdf$popdens2014 <- sapply(clustdf$cluster, function(x) sum(cendf$popdens2014[cendf$cluster==x]*cendf$pop2014[cendf$cluster==x])/sum(cendf$pop2014[cendf$cluster==x]))
        clustdf$popdens2015 <- sapply(clustdf$cluster, function(x) sum(cendf$popdens2015[cendf$cluster==x]*cendf$pop2015[cendf$cluster==x])/sum(cendf$pop2015[cendf$cluster==x]))
        clustdf$popdens2016 <- sapply(clustdf$cluster, function(x) sum(cendf$popdens2016[cendf$cluster==x]*cendf$pop2016[cendf$cluster==x])/sum(cendf$pop2016[cendf$cluster==x]))

        clustdf$Vacant2011 <- sapply(clustdf$cluster, function(x) sum(cendf$Vacant2011[cendf$cluster==x]*cendf$pop2011[cendf$cluster==x])/sum(cendf$pop2011[cendf$cluster==x]))
        clustdf$Vacant2012 <- sapply(clustdf$cluster, function(x) sum(cendf$Vacant2012[cendf$cluster==x]*cendf$pop2012[cendf$cluster==x])/sum(cendf$pop2012[cendf$cluster==x]))
        clustdf$Vacant2013 <- sapply(clustdf$cluster, function(x) sum(cendf$Vacant2013[cendf$cluster==x]*cendf$pop2013[cendf$cluster==x])/sum(cendf$pop2013[cendf$cluster==x]))
        clustdf$Vacant2014 <- sapply(clustdf$cluster, function(x) sum(cendf$Vacant2014[cendf$cluster==x]*cendf$pop2014[cendf$cluster==x])/sum(cendf$pop2014[cendf$cluster==x]))
        clustdf$Vacant2015 <- sapply(clustdf$cluster, function(x) sum(cendf$Vacant2015[cendf$cluster==x]*cendf$pop2015[cendf$cluster==x])/sum(cendf$pop2015[cendf$cluster==x]))
        clustdf$Vacant2016 <- sapply(clustdf$cluster, function(x) sum(cendf$Vacant2016[cendf$cluster==x]*cendf$pop2016[cendf$cluster==x])/sum(cendf$pop2016[cendf$cluster==x]))
        
#5 - Put all cluster variables in cendf
    clustdf2 <- clustdf   
    names(clustdf2) <- paste("clust", names(clustdf2), sep = "")
    names(clustdf2)[1] <- "cluster"
    cendf <- merge(cendf, clustdf2, by="cluster")
    
#6 - saverds
    saveRDS(cendf, paste(Directory, City, "_cendf.rds", sep=""))
    saveRDS(clustdf, paste(Directory, City, "_clustdf.rds", sep=""))
}
