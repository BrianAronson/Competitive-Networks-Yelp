#' 1 - Create census tract data.frame
#' @export
#' @import data.table 
#' @import RCurl 
#' @import RJSONIO 
#' @import acs 
#' @import ggplot2 
#' @import tigris
#' @import sp 
#' @import raster 

yelp_cendf <- function(Directory, City){

#1 - load data
    setwd(Directory)
    Bus <- readRDS("Restaurants.rds")
    Cen <- readRDS("Censusinfo.rds")
    Bus <- Bus[Bus$city_super==City, ]
    Cen <- Cen[Cen$id%in%Bus$id, ]

#2 - Merge Census with Bus
  Bus <- cbind(Cen, Bus)
  
#3 - Create df by census tract
  cendf <- Bus[!duplicated((Bus$GEOID)), c("GEOID", "area", "Migrants2011", "Migrants2012", "Migrants2013", "Migrants2014", "Migrants2015", "Migrants2016", "Income2011", "Income2012", "Income2013", "Income2014", "Income2015", "Income2016", "Unemploy2011", "Unemploy2012", "Unemploy2013", "Unemploy2014", "Unemploy2015", "Unemploy2016", "Vacant2011", "Vacant2012", "Vacant2013", "Vacant2014", "Vacant2015", "Vacant2016", "White2011", "White2012", "White2013", "White2014", "White2015", "White2016", "popdens2011", "popdens2012", "popdens2013", "popdens2014", "popdens2015", "popdens2016", "internationalMigrants2011", "internationalMigrants2012", "internationalMigrants2013", "internationalMigrants2014", "internationalMigrants2015", "internationalMigrants2016", "city", "city_super")]
  #a - Create useful variables
      cendf$N <- sapply(cendf$GEOID, function(x) sum(Bus$GEOID==x, na.rm=T))
      cendf$is_open <- sapply(cendf$GEOID, function(x) sum(Bus$is_open[Bus$GEOID==x], na.rm=T))
      cendf$had_closed <- cendf$N-cendf$is_open
      cendf$rev_count <- sapply(cendf$GEOID, function(x) sum(Bus$rev_count[Bus$GEOID==x], na.rm=T))
      cendf$rev_count_nonelites <- sapply(cendf$GEOID, function(x) sum(Bus$rev_count_nonelites[Bus$GEOID==x], na.rm=T))
      cendf$chain <- sapply(cendf$GEOID, function(x) sum(Bus$chain[Bus$GEOID==x], na.rm=T))
      cendf$chain_super <- sapply(cendf$GEOID, function(x) sum(Bus$chain_super[Bus$GEOID==x], na.rm=T))
      cendf$latitude <- sapply(cendf$GEOID, function(x) mean(Bus$latitude[Bus$GEOID==x], na.rm=T))
      cendf$longitude <- sapply(cendf$GEOID, function(x) mean(Bus$longitude[Bus$GEOID==x], na.rm=T))
      cendf$stars <- sapply(cendf$GEOID, function(x) mean(Bus$stars[Bus$GEOID==x], na.rm=T))
      cendf$age <- sapply(cendf$GEOID, function(x) mean(Bus$age[Bus$GEOID==x], na.rm=T))
      cendf$Price <- sapply(cendf$GEOID, function(x) mean(Bus$Price[Bus$GEOID==x], na.rm=T))
      cendf$Hour_Open <- sapply(cendf$GEOID, function(x) mean(Bus$Hour_Open[Bus$GEOID==x], na.rm=T))
      cendf$dist_center <- sapply(cendf$GEOID, function(x) mean(Bus$dist_center[Bus$GEOID==x], na.rm=T))
      cendf$Hour_Close <- sapply(cendf$GEOID, function(x) mean(Bus$Hour_Close[Bus$GEOID==x], na.rm=T))
      
      
#4 - 2013 and 2016 versions of the above
  datefun <- function(x) as.numeric(substr(x, 1, 4))+(as.numeric(substr(x, 6, 7))-1)/12 + (as.numeric(substr(x, 9, 10))-1)/365
  Bus$date_open <- datefun(Bus$date_open)
  Bus$date_close <- datefun(Bus$date_close)
  Bus.2013 <- Bus[Bus$date_open<=2013 & (Bus$date_close>2013 | Bus$is_open==1), ]
  Bus.2016 <- Bus[Bus$date_open<=2016 & (Bus$date_close>2016 | Bus$is_open==1), ]
  #a) 2013
      cendf$N.2013 <- sapply(cendf$GEOID, function(x) sum(Bus.2013$GEOID==x, na.rm=T))
      cendf$is_open.2013 <- sapply(cendf$GEOID, function(x) sum(Bus.2013$is_open[Bus.2013$GEOID==x], na.rm=T))
      cendf$had_closed.2013 <- cendf$N-cendf$is_open
      cendf$rev_count.2013 <- sapply(cendf$GEOID, function(x) sum(Bus.2013$rev_count[Bus.2013$GEOID==x], na.rm=T))
      cendf$rev_count_nonelites.2013 <- sapply(cendf$GEOID, function(x) sum(Bus.2013$rev_count_nonelites[Bus.2013$GEOID==x], na.rm=T))
      cendf$chain.2013 <- sapply(cendf$GEOID, function(x) sum(Bus.2013$chain[Bus.2013$GEOID==x], na.rm=T))
      cendf$chain_super.2013 <- sapply(cendf$GEOID, function(x) sum(Bus.2013$chain_super[Bus.2013$GEOID==x], na.rm=T))
      cendf$latitude.2013 <- sapply(cendf$GEOID, function(x) mean(Bus.2013$latitude[Bus.2013$GEOID==x], na.rm=T))
      cendf$longitude.2013 <- sapply(cendf$GEOID, function(x) mean(Bus.2013$longitude[Bus.2013$GEOID==x], na.rm=T))
      cendf$stars.2013 <- sapply(cendf$GEOID, function(x) mean(Bus.2013$stars[Bus.2013$GEOID==x], na.rm=T))

      Bus.2013$age <- 2013-Bus.2013$date_open
      cendf$age.2013 <- sapply(cendf$GEOID, function(x) mean(Bus.2013$age[Bus.2013$GEOID==x], na.rm=T))
      cendf$Price.2013 <- sapply(cendf$GEOID, function(x) mean(Bus.2013$Price[Bus.2013$GEOID==x], na.rm=T))
      cendf$Hour_Open.2013 <- sapply(cendf$GEOID, function(x) mean(Bus.2013$Hour_Open[Bus.2013$GEOID==x], na.rm=T))
      cendf$dist_center.2013 <- sapply(cendf$GEOID, function(x) mean(Bus.2013$dist_center[Bus.2013$GEOID==x], na.rm=T))
      cendf$Hour_Close.2013 <- sapply(cendf$GEOID, function(x) mean(Bus.2013$Hour_Close[Bus.2013$GEOID==x], na.rm=T))
  #b) 2016
      cendf$N.2016 <- sapply(cendf$GEOID, function(x) sum(Bus.2016$GEOID==x, na.rm=T))
      cendf$is_open.2016 <- sapply(cendf$GEOID, function(x) sum(Bus.2016$is_open[Bus.2016$GEOID==x], na.rm=T))
      cendf$had_closed.2016 <- cendf$N-cendf$is_open
      cendf$rev_count.2016 <- sapply(cendf$GEOID, function(x) sum(Bus.2016$rev_count[Bus.2016$GEOID==x], na.rm=T))
      cendf$rev_count_nonelites.2016 <- sapply(cendf$GEOID, function(x) sum(Bus.2016$rev_count_nonelites[Bus.2016$GEOID==x], na.rm=T))
      cendf$chain.2016 <- sapply(cendf$GEOID, function(x) sum(Bus.2016$chain[Bus.2016$GEOID==x], na.rm=T))
      cendf$chain_super.2016 <- sapply(cendf$GEOID, function(x) sum(Bus.2016$chain_super[Bus.2016$GEOID==x], na.rm=T))
      cendf$latitude.2016 <- sapply(cendf$GEOID, function(x) mean(Bus.2016$latitude[Bus.2016$GEOID==x], na.rm=T))
      cendf$longitude.2016 <- sapply(cendf$GEOID, function(x) mean(Bus.2016$longitude[Bus.2016$GEOID==x], na.rm=T))
      cendf$stars.2016 <- sapply(cendf$GEOID, function(x) mean(Bus.2016$stars[Bus.2016$GEOID==x], na.rm=T))
      Bus.2016$age <- 2016-Bus.2016$date_open
      cendf$age.2016 <- sapply(cendf$GEOID, function(x) mean(Bus.2016$age[Bus.2016$GEOID==x], na.rm=T))
      cendf$Price.2016 <- sapply(cendf$GEOID, function(x) mean(Bus.2016$Price[Bus.2016$GEOID==x], na.rm=T))
      cendf$Hour_Open.2016 <- sapply(cendf$GEOID, function(x) mean(Bus.2016$Hour_Open[Bus.2016$GEOID==x], na.rm=T))
      cendf$dist_center.2016 <- sapply(cendf$GEOID, function(x) mean(Bus.2016$dist_center[Bus.2016$GEOID==x], na.rm=T))
      cendf$Hour_Close.2016 <- sapply(cendf$GEOID, function(x) mean(Bus.2016$Hour_Close[Bus.2016$GEOID==x], na.rm=T))

#5 - save results
      saveRDS(cendf, paste(Directory, City, "_cendf.rds", sep=""))
}      
