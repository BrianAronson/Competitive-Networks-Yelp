#' 21 - Prep vars for model part 2
#' @export
#' @import data.table

Hazard_Model_Prep2  <-  function(Directory,  City){
#0 - Set Directory
    setwd(Directory)

#1 - Read Data
    Haz <- readRDS(paste(prep$Cities[1], "_runDensityVars.rds", sep=""))
    for(i in 2:length(prep$Cities)){
        Haz2 <- readRDS(paste(prep$Cities[i], "_runDensityVars.rds", sep=""))
        Haz <- Haz[, match(names(Haz2), names(Haz))[!is.na(match(names(Haz2), names(Haz)))]]
        Haz2 <- Haz2[, match(names(Haz), names(Haz2))[!is.na(match(names(Haz), names(Haz2)))]]
        Haz <- Haz[, match(names(Haz2), names(Haz))[!is.na(match(names(Haz2), names(Haz)))]]
        Haz <- rbind(Haz, Haz2)
    }
  
    
#2 - Create outlier function
    outlierfun <-  function(x){
      x[x<quantile(x, .05, na.rm=T)] <- quantile(x, .05, na.rm=T)
      x[x>quantile(x, .95, na.rm=T)] <- quantile(x, .95, na.rm=T)
      return(x)
    }
    
#3 - Fix densities where no alters
    Haz$density.1 <- ifelse(is.na(Haz$density.1), 0, Haz$density.1)
    Haz$density.2 <- ifelse(is.na(Haz$density.2), 0, Haz$density.2)
    
#4 - fix census variables
    Haz$Income <- Haz$Income2011
    Haz$Migrants <- Haz$Migrants2011
    Haz$Unemploy <- Haz$Unemploy2011
    Haz$Vacant <- Haz$Vacant2011
    Haz$White <- Haz$White2011
    Haz$popdens <- Haz$popdens2011

    Haz$Income[Haz$Interval>2012] <- Haz$Income2012[Haz$Interval>2012]
    Haz$Migrants[Haz$Interval>2012] <- Haz$Migrants2012[Haz$Interval>2012]
    Haz$Unemploy[Haz$Interval>2012] <- Haz$Unemploy2012[Haz$Interval>2012]
    Haz$Vacant[Haz$Interval>2012] <- Haz$Vacant2012[Haz$Interval>2012]
    Haz$White[Haz$Interval>2012] <- Haz$White2012[Haz$Interval>2012]
    Haz$popdens[Haz$Interval>2012] <- Haz$popdens2012[Haz$Interval>2012]

    Haz$Income[Haz$Interval>2013] <- Haz$Income2013[Haz$Interval>2013]
    Haz$Migrants[Haz$Interval>2013] <- Haz$Migrants2013[Haz$Interval>2013]
    Haz$Unemploy[Haz$Interval>2013] <- Haz$Unemploy2013[Haz$Interval>2013]
    Haz$Vacant[Haz$Interval>2013] <- Haz$Vacant2013[Haz$Interval>2013]
    Haz$White[Haz$Interval>2013] <- Haz$White2013[Haz$Interval>2013]
    Haz$popdens[Haz$Interval>2013] <- Haz$popdens2013[Haz$Interval>2013]

    Haz$Income[Haz$Interval>2014] <- Haz$Income2014[Haz$Interval>2014]
    Haz$Migrants[Haz$Interval>2014] <- Haz$Migrants2014[Haz$Interval>2014]
    Haz$Unemploy[Haz$Interval>2014] <- Haz$Unemploy2014[Haz$Interval>2014]
    Haz$Vacant[Haz$Interval>2014] <- Haz$Vacant2014[Haz$Interval>2014]
    Haz$White[Haz$Interval>2014] <- Haz$White2014[Haz$Interval>2014]
    Haz$popdens[Haz$Interval>2014] <- Haz$popdens2014[Haz$Interval>2014]

    Haz$Income[Haz$Interval>2015] <- Haz$Income2015[Haz$Interval>2015]
    Haz$Migrants[Haz$Interval>2015] <- Haz$Migrants2015[Haz$Interval>2015]
    Haz$Unemploy[Haz$Interval>2015] <- Haz$Unemploy2015[Haz$Interval>2015]
    Haz$Vacant[Haz$Interval>2015] <- Haz$Vacant2015[Haz$Interval>2015]
    Haz$White[Haz$Interval>2015] <- Haz$White2015[Haz$Interval>2015]
    Haz$popdens[Haz$Interval>2015] <- Haz$popdens2015[Haz$Interval>2015]

    Haz$Income[Haz$Interval>2016] <- Haz$Income2016[Haz$Interval>2016]
    Haz$Migrants[Haz$Interval>2016] <- Haz$Migrants2016[Haz$Interval>2016]
    Haz$Unemploy[Haz$Interval>2016] <- Haz$Unemploy2016[Haz$Interval>2016]
    Haz$Vacant[Haz$Interval>2016] <- Haz$Vacant2016[Haz$Interval>2016]
    Haz$White[Haz$Interval>2016] <- Haz$White2016[Haz$Interval>2016]
    Haz$popdens[Haz$Interval>2016] <- Haz$popdens2016[Haz$Interval>2016]

    Haz$Income <- (outlierfun(Haz$Income))
    Haz$Migrants <- (outlierfun(Haz$Migrants))
    Haz$Unemploy <- (outlierfun(Haz$Unemploy))
    Haz$Vacant <- (outlierfun(Haz$Vacant))
    Haz$White <- (outlierfun(Haz$White))
    Haz$popdens <- (outlierfun(Haz$popdens))


#5 - keep only useful variables
    varstokeep <- c("ID", "Dissolved", "density", "sim", "embed", "spec", "local", "dist_center", "chain", 
                  "age", "stars", "Interval", "date", "rev_count", "city", "atyp", "Migrants", "Income", 
                  "Unemploy", "Vacant", "White", "popdens")
    colstokeep <- vector()
    for(i in 1:length(varstokeep)){
      colstokeep <- c(colstokeep, which(grepl(varstokeep[i], names(Haz))))
    }
    colstokeep <- sort(unique(colstokeep))
    Haz <- Haz[, colstokeep]

#6 - Make variables inverse,  so that higher = greater degree of crwding/centrality
    Haz$localcentrality.1 <- 1-Haz$localcentrality.1
    Haz$localcrowd.0.1 <- 1-Haz$localcrowd.0.1

#7 - adjust outliers
    Haz$localcrowd.0.1 <- outlierfun(Haz$localcrowd.0.1)
    Haz$localcentrality.1 <- outlierfun(Haz$localcentrality.1)

#8 - transform variables
    #a) square all key variables
        # create reference of variables to transform
            colstoalterlookup <- (c("density|spec|sim|embed|gen|atyp|local|stars|Income|Migrants|Unemploy|Vacant|White|popdens"))
            colstoalter <- which(grepl(colstoalterlookup, names(Haz)))
        # create variables
            tempmat <- as.data.frame(matrix(0, nrow=nrow(Haz), ncol=length(colstoalter)))
            names(tempmat) <- paste(names(Haz)[colstoalter], "2", sep="")
            for(i in 1:length(colstoalter)){
              tempmat[, i] <- Haz[, colstoalter[i]]^2*sign(Haz[, colstoalter[i]])
            }
            Haz <- cbind(Haz, tempmat)
    #b) create time change variables
        # create function for doing the following: if id = id above,  trait=trait - previous trait,  else 0
            Haz <- Haz[order(Haz$ID), ]
            dups <- duplicated(Haz$ID)
            timechange <- function(x){
              temp <- c(0, x[-length(x)])
              return(ifelse(dups==F, 0, (x-temp)/temp))
            }
        # create variables
            tempmat <- as.data.frame(matrix(0, nrow=nrow(Haz), ncol=length(colstoalter)))
            names(tempmat) <- paste("t", names(Haz)[colstoalter], sep="")
            for(i in 1:length(colstoalter)){
              tempmat[, i] <- timechange(Haz[, colstoalter[i]])
            }
            Haz <- cbind(Haz, tempmat)
            
#9 - kill places with no local alters
    Haz$age[is.na(Haz$localcentrality.1) | is.na(Haz$localcrowd.0w.1)] <- NA

#10 - fix remaining outliers
    Haz$tlocalcrowd.3.1 <- outlierfun(Haz$tlocalcrowd.0.1)
    Haz$localwidth3.1 <- outlierfun(Haz$localwidth2.1)
    Haz$tlocalwidth3.1 <- outlierfun(Haz$tlocalwidth2.1)
    Haz$tlocalcentrality.3 <- outlierfun(Haz$tlocalcentrality.1)

#11 - Change variables to percentages
    Haz$tdensity.3.1 <- outlierfun(Haz$tdensity.1)
    Haz$tdensity.3.1 <- Haz$tdensity.3.1*100
    Haz$tlocalcrowd.3.1 <- Haz$tlocalcrowd.3.1*100
    Haz$tlocalwidth3.1 <- Haz$tlocalwidth3.1*100
    Haz$tlocalcentrality.3 <- Haz$tlocalcentrality.3*100
    Haz$localwidth3.12 <- Haz$localwidth3.1^2
    Haz$Interval2 <- Haz$Interval^2
      
#12 - Scale remaining variables
    Haz$Income2016 <- outlierfun(Haz$Income2016)
    Haz$Income2016 <- Haz$Income2016/1000
    Haz$popdens2016 <- Haz$popdens2016/1000
    Haz$Vacant2016 <- Haz$Vacant2016/100
    Haz$Income2016 <- Haz$Income2016/1000
    Haz$density.1 <- Haz$density.1/100
    Haz$tIncome <- (outlierfun(Haz$tIncome))
    Haz$tMigrants <- (outlierfun(Haz$tMigrants))
    Haz$tUnemploy <- (outlierfun(Haz$tUnemploy))
    Haz$tVacant <- (outlierfun(Haz$tVacant))
    Haz$tWhite <- (outlierfun(Haz$tWhite))
    Haz$tpopdens <- (outlierfun(Haz$tpopdens))
    Haz$tstars_int <- (outlierfun(Haz$tstars_int))
    Haz$tstars_cur <- (outlierfun(Haz$tstars_cur))
    
#13 - Age < 1 variable
    Haz$ageint2 <- ifelse(Haz$age<=4, 0, 1)
    
#14 - Prepare subset conditions for model
    dups <- duplicated(Haz$ID)
    temp <- Haz[!dups, ]
    ids <- temp$ID[temp$chain==0 &temp$date_open>=2011]
    ids <- as.character(ids)
    su <- Haz$ID %in% ids
    
#15 - Save key info
    saveRDS(Haz, "HazInfo.RDS")
    saveRDS(su, "suHazInfo.RDS")
}
