#Prep data


#illustrate competitive crowding at the neighborhood level
    #make ties based on similarity. Size based on specialization, color based on specialization shift (crowding).

#illustrate centrality at the neighborhood level
    #make ties based on similarity. Size based on centrality, color based on centrality shift.


library(ggmap)

#0 - Set Directory
  setwd(Directory)

#1 - Read Data
  City="Charlotte"
  Haz<-readRDS(paste(City,"_runDensityVars.rds",sep=""))
  Busalt<-readRDS("Restaurants.rds")
  Busalt<-Busalt[Busalt$city_super==City,]
  Dist<-readRDS(paste(City,"_dist.rds",sep=""))
  Atr<-readRDS(paste(City,"_atr.rds",sep=""))
  Cust<-readRDS(paste(City,"_Cust.rds",sep=""))

#Create time change variables in Haz
    #Create function for doing the following: if id = id above, trait=trait - previous trait, else 0
        Haz<-Haz[order(Haz$ID),]
        dups<-duplicated(Haz$ID)
        timechange<-function(x){
          temp<-c(0,x[-length(x)])
          return(ifelse(dups==F,0,x-temp))
        }
        Haz$tspec<-timechange(Haz$spec)
        Haz$tlocalspec.1<-timechange(Haz$localspec.1)
        head(Haz$tlocalspec.1)
    #kill duplicates for rest of analyses
        Bus<-Haz
        Bus<-Bus[order(Bus$Interval,decreasing = T),]
        Bus<-Bus[!duplicated(Bus$id),]
  
#2 - Focus on a tiny section of Charlotte
  Bus2<-Bus[Bus$latitude>35.3025 & Bus$latitude<35.315 & Bus$longitude>(-80.7575) & Bus$longitude<(-80.746),]
  #don't allow for duplicate lon/lats
      Bus2<-Bus2[!duplicated(paste(Bus2$latitude,Bus2$longitude)),]
  
#3 - Create variables for visuals
  Died<-ifelse(Bus2$Dissolved==1,1,0)
  rev_count<-Bus2$rev_count
  
#4 - Create a sample data.frame with lat/lon points
  lon <- Bus2$longitude
  lat <- Bus2$latitude
  latlon <- as.data.frame(cbind(lon,lat))

#5 - Get the map from google maps
  charlottemap <- get_map(location = c(lon = mean(latlon$lon), lat = mean(latlon$lat)), zoom = 16,
                          maptype = "toner-lite", scale = "auto")

#6 - plotting the map with some points on it
  ggmap(charlottemap) +
    geom_point(data = latlon, aes(x = lon, y = lat), fill = ifelse(Died==1,"red","blue"), shape = 21,size=15)

#7 - Correct Atr
  #create functions
      range01 <- function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}
      perc.rank <- function(x) trunc(rank(x))/length(x)
  #expand to 0-1
      Atr2<-range01(Atr)
  #Alter to percentile ranks by row (i.e. find out which places ego is most and least similar to)
      temp<-matrix(Atr,nrow=sqrt(length(Atr)))
      Atr<-apply(temp, 1, perc.rank)
      Atr<-c(Atr)
  
#7 - Add ties to map
  # find ties among businesses
      df<-data.frame(Bus1=rep(Busalt$id,each=nrow(Busalt)),Bus2=rep(Busalt$id,nrow(Busalt)), Dist=Dist,Cust=Cust,Atr=Atr,Atr2=Atr2)
      df2<-df[df$Bus1 %in% Bus2$id & df$Bus2 %in% Bus2$id & df$Bus1!=df$Bus2,]
  # insert tie information into new Adjacency List
      # for Bus1s
          df2$id<-df2$Bus1
          df2<-merge(df2,Bus2,by="id")
          #change column names
              names(df2)<-paste("Bus1_",names(df2),sep="")
              df2$Bus1_id<-NULL
              names(df2)[names(df2)=="Bus1_Bus1"]<-"Bus1"
              names(df2)[names(df2)=="Bus1_Bus2"]<-"Bus2"
              names(df2)[names(df2)=="Bus1_Cust"]<-"Cust"
      # for Bus2s
          df2$id<-df2$Bus2
          df2<-merge(df2,Bus2,by="id")
          #change column names
              names(df2)[!(grepl("Bus1",names(df2)))]<-paste("Bus2_",names(df2)[!(grepl("Bus1",names(df2)))],sep="")
              df2$Bus2_id<-NULL
              names(df2)[names(df2)=="Bus2_Bus2"]<-"Bus2"
              names(df2)[names(df2)=="Bus2_Cust"]<-"Cust"
              names(df2)[names(df2)=="Bus1_Atr"]<-"Atr"
              names(df2)[names(df2)=="Bus1_Atr2"]<-"Atr2"
      # Create dataframe to base ties on
          ties<-df2[,c("Bus1_latitude","Bus1_longitude","Bus2_latitude","Bus2_longitude","Atr","Atr2","Bus1_rev_count","Bus2_rev_count")]
      # Create weighted ties based on similarity scores
          ties$Atr<-1-ties$Atr
          ties$Atr<-ifelse(ties$Atr<.95,0,ties$Atr)
          ties$Atr<-ties$Atr^20
          ties<-ties[ties$Atr!=0,]
#8 - prepare other variables
  #Create scale function
      range01 <- function(x){ (x - min(x))/(max(x)-min(x)) * (1 - .2) }
  #variables
      Centrality<-1-Bus2$localspec.1
      tCentrality<-ifelse(Bus2$tlocalspec.1<quantile(Bus2$tlocalspec.1,.1),.1,ifelse(Bus2$tlocalspec.1<quantile(Bus2$tlocalspec.1,.25),.25,ifelse(Bus2$tlocalspec.1>quantile(Bus2$tlocalspec.1,.9),.9,ifelse(Bus2$tlocalspec.1<quantile(Bus2$tlocalspec.1,.75),.75,.5))))
      Specialization<-(1-Bus2$spec)^9*20
      tSpecialization<-ifelse(Bus2$tspec<quantile(Bus2$tspec,.1),.1,ifelse(Bus2$tspec<quantile(Bus2$tspec,.25),.25,ifelse(Bus2$tspec>quantile(Bus2$tspec,.9),.9,ifelse(Bus2$tspec<quantile(Bus2$tspec,.75),.75,.5))))
  # Produce map

  
  
  
  ggmap(charlottemap) +
    geom_segment(data=ties, aes(
      y = Bus1_latitude,
      x = Bus1_longitude,
      yend = Bus2_latitude,
      xend = Bus2_longitude),
      size=ties$Atr) +
      geom_point(data = latlon, aes(x = lon, y = lat), fill = ifelse(Died==1,rgb(1,tCentrality,tCentrality),rgb(tCentrality,tCentrality,1)), shape = 21,size=5+20*Centrality)+
      scale_y_continuous( limits = c( 35.3025 , 35.312 ) , expand = c( 0 , 0 ) )+
      theme_void()
  #darker = greater shift towards central position
  #larger = more central location
  
  ggmap(charlottemap) +
    geom_segment(data=ties, aes(
      y = Bus1_latitude,
      x = Bus1_longitude,
      yend = Bus2_latitude,
      xend = Bus2_longitude),
      size=ties$Atr) +
      geom_point(data = latlon, aes(x = lon, y = lat), fill = ifelse(Died==1,rgb(1,tSpecialization,tSpecialization),rgb(tSpecialization,tSpecialization,1)), shape = 21,size=5+20*Specialization)+
      scale_y_continuous( limits = c( 35.3025 , 35.312 ) , expand = c( 0 , 0 ) )+
      theme_void()
  #darker = more crowding
  #larger = more generalized
  
  
