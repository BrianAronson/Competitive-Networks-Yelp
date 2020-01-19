#' 12 - Create Census Variables
#' @export
#' @import data.table 
#' @import RCurl 
#' @import RJSONIO 
#' @import acs 
#' @import ggplot2 
#' @import tigris
#' @import sp 
#' @import raster 

yelp_census <- function(Directory){
#0 - set api key
    api.key.install(key="")

#1 - load restaurant data
    setwd(Directory)
    Bus <- readRDS("Restaurants.rds")

#2 - set variable to fetch
    #a - Find the general "table" (variable) name of interest at https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t
        #Each table will have numerous variables within it. So the table for "age and sex", 
        #"B01001", will have numerous variables in it, including the total number of people, 
        #total number of males, total number of females age25-27, etc.
    #b - Fetch all variables within table of interest
        var1 <- acs.lookup(table.name = "B19013", endyear=2016)[1]
        var2 <- acs.lookup(table.name = "B23025", endyear=2016)[c(2, 5)] 
        var3 <- acs.lookup(table.name = "B25002", endyear=2016, span=5) #for census tracts, can't do smaller than 5 year span
        var4 <- acs.lookup(table.name = "B07201", endyear=2016, span=5)[c(1, 7)]
        var5 <- acs.lookup(table.name = "B25006", endyear=2016, span=5)[c(1, 2)]

#3 - set locations to fetch from
    #i. get all unique city and state pairs
        pairs <- unique(Bus[, c("city", "state")])
    #ii. split into list of pairs by state
        pairs <- split(pairs, pairs$state)
    #iii. save order of Bus id
        IDS <- Bus$id
    #iv. split Bus by state
        Busl <- split(Bus, Bus$state)
    #v. Get census location lookup info and shape info
        locations <- list()
        locations2 <- list()
        blocks <- list()
        ACSdf <- list()
        for(i in 1:length(pairs)){
          #a. find all county and city names in each state of interest
              cnames <- geo.lookup(state=pairs[[i]]$state[1], county="*", place="*")
          #b. grab row indices of all closest city name matches
              indices <- vector()
              for(j in 1:nrow(pairs[[i]])){
                  indices <- c(indices, agrep(pairs[[i]]$city[j], cnames$place.name))
              }
              indices <- sort(unique(indices))
          #c. find unique county names of each row index
              counties <- sort(unique(cnames$county.name[indices]))
          #d. Break out any county names with more than one name
              counties <- sort(unique(unlist(str_split(counties, ", "))))
          #e. find county fips codes
              cnames <- cnames[!is.na(cnames$county), ]
              cnames <- cnames[cnames$county.name %in% counties, ]
          #f. find census tracts and block groups in correct format to search for in census
              #locations[[i]] <- geo.make(state=as.numeric(cnames$state[1]), county=cnames$county, tract="*", block.group="*")
              locations[[i]] <- geo.make(state=as.numeric(cnames$state[1]), county=cnames$county, tract="*")
          #g. grab block group shape file from tigris
              #blocks[[i]]  <-  block_groups(state=as.numeric(cnames$state[1]), county=cnames$county, cb=TRUE)
              blocks[[i]]  <-  tracts(state=as.numeric(cnames$state[1]), county=cnames$county, cb=TRUE)
        
#4 - Fetch data from census based on locations and variables:
    #a. Fetch and format the data
        temp <- acs.fetch(geo=locations[[i]], variable=c(var1, var2, var3, var4, var5), endyear=2011, span=5)
        tempACSdf1 <- cbind(temp@geography, temp@estimate)
        temp <- acs.fetch(geo=locations[[i]], variable=c(var1, var2, var3, var4, var5), endyear=2012, span=5)
        tempACSdf2 <- cbind(temp@geography, temp@estimate)
        temp <- acs.fetch(geo=locations[[i]], variable=c(var1, var2, var3, var4, var5), endyear=2013, span=5)
        tempACSdf3 <- cbind(temp@geography, temp@estimate)
        temp <- acs.fetch(geo=locations[[i]], variable=c(var1, var2, var3, var4, var5), endyear=2014, span=5)
        tempACSdf4 <- cbind(temp@geography, temp@estimate)
        temp <- acs.fetch(geo=locations[[i]], variable=c(var1, var2, var3, var4, var5), endyear=2015, span=5)
        tempACSdf5 <- cbind(temp@geography, temp@estimate)
        temp <- acs.fetch(geo=locations[[i]], variable=c(var1, var2, var3, var4, var5), endyear=2016, span=5)
        tempACSdf6 <- cbind(temp@geography, temp@estimate)
    #b. bind the data
        names(tempACSdf1)[5:length(tempACSdf1)] <- paste(names(tempACSdf1)[5:length(tempACSdf1)], "_2011", sep="")
        names(tempACSdf2) <- paste(names(tempACSdf2), "_2012", sep="")
        names(tempACSdf3) <- paste(names(tempACSdf3), "_2013", sep="")
        names(tempACSdf4) <- paste(names(tempACSdf4), "_2014", sep="")
        names(tempACSdf5) <- paste(names(tempACSdf5), "_2015", sep="")
        names(tempACSdf6) <- paste(names(tempACSdf6), "_2016", sep="")
        ACSdf[[i]] <- cbind(tempACSdf1, tempACSdf2[5:length(tempACSdf1)], tempACSdf3[5:length(tempACSdf1)], tempACSdf4[5:length(tempACSdf1)], tempACSdf5[5:length(tempACSdf1)], tempACSdf6[5:length(tempACSdf1)])
        row.names(ACSdf[[i]]) <- NULL

#5 - use info in ACS df to construct fips keys of each tract and group
    #a. create fips geo_code for each blockgroup
        #ACSdf[[i]]$GEOID <- paste0(str_pad(ACSdf[[i]]$state, 2, "left", pad="0"), str_pad(ACSdf[[i]]$county, 3, "left", pad="0"), str_pad(ACSdf[[i]]$tract, 6, "left", pad="0"), ACSdf[[i]]$blockgroup)
        ACSdf[[i]]$GEOID <- paste0(str_pad(ACSdf[[i]]$state, 2, "left", pad="0"), str_pad(ACSdf[[i]]$county, 3, "left", pad="0"), str_pad(ACSdf[[i]]$tract, 6, "left", pad="0"))
    #b. find which Geoid each restaurant is in
        #create block-level data
            blocks[[i]]@data$area <- area(blocks[[i]])/1000000*0.386102
        #create spatial point object for longlat
            longlat <- Busl[[i]][, c("longitude", "latitude")]
            longlat <- SpatialPointsDataFrame(Busl[[i]][, c("longitude", "latitude")], as.data.frame(Busl[[i]][, "id"]), match.ID = F)
            proj4string(longlat) <- proj4string(blocks[[i]])
        #find which block spatial point is within
            returns <- over(longlat, blocks[[i]])
            Busl[[i]]$GEOID <- returns$GEOID
            Busl[[i]]$area <- returns$area
        #subset ACSdf[[i]] to blocks in restaurant data
            ACSdf[[i]] <- ACSdf[[i]][, c(5:ncol(ACSdf[[i]]))]
            ACSdf[[i]] <- ACSdf[[i]][ACSdf[[i]]$GEOID %in% returns$GEOID, ]
        #insert census info into Bus
            Busl[[i]] <- merge(Busl[[i]], ACSdf[[i]], by="GEOID", all=T)
            Busl[[i]] <- Busl[[i]][order(Busl[[i]]$id), ]
            }
            
#6 - Merge Bus back together
    temp <- do.call(rbind, Busl)
    temp <- temp[match(IDS, temp$id), ]

#7 - subset to just new vars
    newvars <- setdiff(names(temp), names(Bus))
    temp <- temp[, c("id", newvars)]
    
#8 - create key variables
    temp$Migrants2011 <- temp$B07201_007_2011/temp$B07201_001_2011
    temp$Migrants2012 <- temp$B07201_007_2012/temp$B07201_001_2012
    temp$Migrants2013 <- temp$B07201_007_2013/temp$B07201_001_2013
    temp$Migrants2014 <- temp$B07201_007_2014/temp$B07201_001_2014
    temp$Migrants2015 <- temp$B07201_007_2015/temp$B07201_001_2015
    temp$Migrants2016 <- temp$B07201_007_2016/temp$B07201_001_2016
    temp$Income2011 <- temp$B19013_001_2011
    temp$Income2012 <- temp$B19013_001_2012
    temp$Income2013 <- temp$B19013_001_2013
    temp$Income2014 <- temp$B19013_001_2014
    temp$Income2015 <- temp$B19013_001_2015
    temp$Income2016 <- temp$B19013_001_2016
    temp$Unemploy2011 <- temp$B23025_005_2011/temp$B23025_002_2011
    temp$Unemploy2012 <- temp$B23025_005_2012/temp$B23025_002_2012
    temp$Unemploy2013 <- temp$B23025_005_2013/temp$B23025_002_2013
    temp$Unemploy2014 <- temp$B23025_005_2014/temp$B23025_002_2014
    temp$Unemploy2015 <- temp$B23025_005_2015/temp$B23025_002_2015
    temp$Unemploy2016 <- temp$B23025_005_2016/temp$B23025_002_2016
    temp$Vacant2011 <- temp$B25002_001_2011/temp$B25002_003_2011
    temp$Vacant2012 <- temp$B25002_001_2012/temp$B25002_003_2012
    temp$Vacant2013 <- temp$B25002_001_2013/temp$B25002_003_2013
    temp$Vacant2014 <- temp$B25002_001_2014/temp$B25002_003_2014
    temp$Vacant2015 <- temp$B25002_001_2015/temp$B25002_003_2015
    temp$Vacant2016 <- temp$B25002_001_2016/temp$B25002_003_2016
    temp$White2011 <- temp$B25006_002_2011/temp$B25006_001_2011
    temp$White2012 <- temp$B25006_002_2012/temp$B25006_001_2012
    temp$White2013 <- temp$B25006_002_2013/temp$B25006_001_2013
    temp$White2014 <- temp$B25006_002_2014/temp$B25006_001_2014
    temp$White2015 <- temp$B25006_002_2015/temp$B25006_001_2015
    temp$White2016 <- temp$B25006_002_2016/temp$B25006_001_2016
    temp$popdens2011 <- temp$B07201_001_2011/temp$area
    temp$popdens2012 <- temp$B07201_001_2012/temp$area
    temp$popdens2013 <- temp$B07201_001_2013/temp$area
    temp$popdens2014 <- temp$B07201_001_2014/temp$area
    temp$popdens2015 <- temp$B07201_001_2015/temp$area
    temp$popdens2016 <- temp$B07201_001_2016/temp$area
    temp <- temp[, !grepl("_", names(temp))]

#9 - fix complete outliers
    for(i in 3:length(temp)){
      temptemp <- temp[, i]
      quants <- quantile(temp[, i], c(.01, .99), na.rm=T)
      temptemp <- ifelse(temptemp<quants[1], quants[1], temptemp)
      temptemp <- ifelse(temptemp>quants[2], quants[2], temptemp)
      temp[, i] <- temptemp
    }

#10 - Save RDS
    saveRDS(temp, "Censusinfo.rds")
        
}       
