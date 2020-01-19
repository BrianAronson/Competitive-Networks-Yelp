#' 20 - Prep vars for model part 1
#' @export
#' @import data.table

Hazard_Model_Prep <- function(Directory, City, Intervals){
  #0 - Set Directory
      setwd(Directory)
  
  #1 - Read Data
      Bus <- readRDS("Restaurants.rds")
      Bus <- Bus[Bus$city_super==City, ]
      DF <- readRDS(paste(City, "_DensityVars.rds", sep=""))
      Stru <- readRDS(paste(City, "_StructuralVars.rds", sep=""))
      Dyn <- readRDS(paste(City, "_DynVars.rds", sep=""))
      Cen <- readRDS("Censusinfo.rds")
      Cen <- Cen[Cen$id%in%Bus$id, ]

  #2 Append Structural and census vars to DF
      DF <- cbind(DF, Stru)
      DF <- cbind(DF, Cen)
      
  #3 - Create Interv vector
      Interv2 <- (2018.5-2011)/Intervals
      Interv <- c(2011+(0:(Interv2)*Intervals))

  #4 - Create dataset for Haz models
      #Create DF for Haz model
          Haz <- Bus[rep(row.names(Bus), length(Interv)-1), ]
          Haz <- cbind(Haz, DF, row.names=NULL)
      #Merge dynamic variables
          Haz <- cbind(Haz, Dyn)

  #5 - Create useful variables for model
      #Interv
          Haz$Interval <- rep(Interv[-length(Interv)], each=nrow(Bus))
      #Stars
          Haz$stars_int <- ifelse(Haz$stars_int==0, Haz$stars_cur, Haz$stars_int)
      #Age variables
          Haz$date_close <- as.numeric(substr(Haz$date_close, 1, 4))+as.numeric(substr(Haz$date_close, 6, 7))/12
          Haz$date_open <- as.numeric(substr(Haz$date_open, 1, 4))+as.numeric(substr(Haz$date_open, 6, 7))/12
          Haz$age <- Haz$Interval-Haz$date_open+.5
          Haz$age2 <- Haz$age^2
          Haz$log_rev_cur <- log10(Haz$revs_cur+1)
      #Assume those that were closed in early january 2017 closed in late decemeber 2016
          Haz$date_close <- ifelse(Haz$Interval==2016.5 & Haz$is_open==0 & Haz$date_close>2017, 2016.5, Haz$date_close)
      #Create lagged Dissolved indicator (depending on quality of information and date of closure lag can be as low as .01 years)
          Haz$Dissolved <- ifelse(Haz$is_open==0 & Haz$date_close<=Haz$Interval+.5, 1, 0)

  #6 - Remove observations
      #unborn
          Haz <- Haz[Haz$age>0, ]
      #deceased prior to 2011
          Haz <- Haz[Haz$date_close>=2011, ]
      #already deceased
          temp <- duplicated(paste(Haz$Dissolved, Haz$id)) & Haz$Dissolved==1
          Haz <- Haz[temp==F, ]
              
  #7 - Save RDS
      saveRDS(Haz, paste(City, "_runDensityVars.rds", sep=""))

}



