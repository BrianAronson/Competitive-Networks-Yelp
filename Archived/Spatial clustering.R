
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)
library(dbscan)

prep$City<-"Madison"
setwd(prep$Directory)
#Whole city
  Bus<-readRDS("Restaurants.rds")
  Bus<-Bus[Bus$city_super==prep$City,]
  #Bus<-Bus[Bus$city==Bus$city_super,]
# create a data.frame with lat/lon points
  lon <- Bus$longitude
  lat <- Bus$latitude
  df <- as.data.frame(cbind(lon,lat))

# Get the map from google maps
  charlottemap <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 11,
                          maptype = "toner-lite", scale = "auto")
# plot the maps with points
  df <- as.data.frame(cbind(lon,lat))
  clusters<-dbscan(df[,1:2], eps = .005)
  df$cluster<-clusters$cluster
  clusters2<-dbscan(df[df$cluster==1,1:2], eps = .01)
  df$cluster<-ifelse(df$cluster==0,73,df$cluster)
  df$cluster<-as.factor(df$cluster)

  # Clusters - Nice visualization
      ggmap(charlottemap) +
        scale_fill_brewer(palette="Paired")+
        geom_density2d(data = df, aes(x = lon, y = lat),bins=50)  
      
      
  #Best geospatial clustering method  
      library(geosphere)
      geo.dist = function(df) {
        require(geosphere)
        d <- function(i,z){         # z[1:2] contain long, lat
          dist <- rep(0,nrow(z))
          dist[i:nrow(z)] <- distHaversine(z[i:nrow(z),1:2],z[i,1:2])
          return(dist)
        }
        dm <- do.call(cbind,lapply(1:nrow(df),d,df))
        return(as.dist(dm))
      }
      df <- as.data.frame(cbind(lon,lat))
      km <- kmeans(geo.dist(df),centers=12)
      df$km<-as.factor(km$cluster)
      
      #visualization
      ggmap(charlottemap) +
        scale_fill_brewer(palette="Paired")+
        geom_point(data = df, aes(x = lon, y = lat, fill = km), shape = 21,size=3)
      
  
      
#Determine how well these clusters map onto ties.
    #load data
        Cust<-readRDS(paste(prep$City,"_cust.rds",sep=""))
        Dist<-readRDS(paste(prep$City,"_dist.rds",sep=""))
        Atr<-readRDS(paste(prep$City,"_atr.rds",sep=""))
        Results2<-readRDS(paste(prep$City,"_nullties.rds",sep=""))
        
        cor(Results2$Proportion,Dist)

    #bring in spatial cluster info
        DT<-data.table(Cust,Cluster1=rep(df$km,each=nrow(df)),Cluster2=rep(df$km,nrow(df)),Prop=Results2$Proportion)
    #reduce DT to spatial clusters
        DT2<-DT[,.(Prop=mean(Prop),Cust=sum(Cust)),by=list(Cluster1,Cluster2)]
    #for creating polgons around each cluster
        df<-data.table(df)
        df2<-df[,.(hulls=chull(.SD)),by=km]
        df4<-df[,.(lon=lon,lat=lat,hulls=1:.N),by=km]
        df3<-merge(df2,df4,by=c("km","hulls"),sort=F)
    #find centerpoint within each cluster
        centerpoints<-df[,.(lon=mean(lon),lat=mean(lat)),by=km]
        names(centerpoints)<-c("Cluster1","lon1","lat1")
        DT2<-merge(DT2,centerpoints,by="Cluster1")
        names(centerpoints)<-c("Cluster2","lon2","lat2")
        DT2<-merge(DT2,centerpoints,by="Cluster2")
    #keep edges between spatial clusters that are relatively large
        DT2$Prop2<-DT2$Prop*2
        DT3<-DT2[DT2$Prop2>1.05]
        DT3$Prop2<-DT3$Prop2^2

        #map it
          ggmap(charlottemap) +
              scale_fill_brewer(palette="Paired")+
              geom_point(data = df, aes(x = lon, y = lat, fill = km), shape = 21,size=3)+
              geom_polygon(data = df3, aes(x = lon, y = lat, fill = km),alpha=.6)+
              geom_segment(data=DT3, aes(
              y = lat1,
              x = lon1,
              yend = lat2,
              xend = lon2
              ),
              size=DT3$Prop2*3,
              alpha=.5
              #arrow = arrow(length = unit(0.03, "npc"))
          )
        
        #next questions are: 
            #a) can I test/ model these distance clusters/connections?
            #b) can I track changes in these connections?
            #c) can I include a parameter for tie propensity now?
   
        
        
        
        
        
        
        
        
        #Try mapping spatial connections before an after
          #create data open and closed variables
          #2 - Change dates to numeric. Assume date_close only true if business marked as closed; assign all other dates closed as 2100;
              Bus$date_close<-ifelse(Bus$is_open==0,as.numeric(substr(Bus$date_close,1,4))+as.numeric(substr(Bus$date_close,6,7))/12,2100)
              Bus$date_open<-as.numeric(substr(Bus$date_open,1,4))+as.numeric(substr(Bus$date_open,6,7))/12
              #round down to .5s
                  Bus$date_close<-.5*floor(Bus$date_close/.5)
                  Bus$date_open<-.5*floor(Bus$date_open/.5)

          
                  
          #bring in spatial cluster info
              DT<-data.table(Cust,Prop=Results2$Proportion, open1=rep(Bus$date_open,each=nrow(Bus)),close1=rep(Bus$date_close,each=nrow(Bus)),open2=rep(Bus$date_open,nrow(Bus)),close2=rep(Bus$date_close,nrow(Bus)))
          #Find geospatial clusters
              df$km<-NULL
              km <- kmeans((df),centers=10,iter.max=10000)
              df$km<-as.factor(km$cluster)
              #dbscan(cbind(df$lat, df$long), eps = .001)
              df<-data.table(df)
              table(df$km)
          #append spatial cluster info to DT
              DT$Cluster1<-rep(df$km,each=nrow(df))
              DT$Cluster2=rep(df$km,nrow(df))
              
              
              
              year=2015
          #reduce DT to just places that are open at year
              DT1<-DT[DT$close1>=year & DT$close2>=year & DT$open1<=year & DT$open2<=year,]
          #reduce years in data
              df1<-df[Bus$date_close>=year & Bus$date_open<=year,] 
              
          #reduce DT to spatial clusters
              DT2<-DT1[,.(Prop=mean(Prop),Cust=sum(Cust)),by=list(Cluster1,Cluster2)]
          #creating polgons around each cluster
              df2<-df1[,.(hulls=chull(.SD)),by=km]
              df4<-df1[,.(lon=lon,lat=lat,hulls=1:.N),by=km]
              df3<-merge(df2,df4,by=c("km","hulls"),sort=F)
          #find centerpoint within each cluster
              centerpoints<-df1[,.(lon=mean(lon),lat=mean(lat)),by=km]
              names(centerpoints)<-c("Cluster1","lon1","lat1")
              DT2<-merge(DT2,centerpoints,by="Cluster1")
              names(centerpoints)<-c("Cluster2","lon2","lat2")
              DT2<-merge(DT2,centerpoints,by="Cluster2")
          #keep edges between spatial clusters that are relatively large
              DT3<-DT2[DT2$Prop>(.07)]
              mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 7))
              
              #map it
                ggmap(charlottemap) +
                    #scale_fill_brewer(palette="Paired")+
                    geom_point(data = df1, aes(x = lon, y = lat, fill = km), shape = 21,size=3)+
                    geom_polygon(data = df3, aes(x = lon, y = lat, fill = km),alpha=.6)+
                    geom_segment(data=DT3, aes(
                        y = lat1,
                        x = lon1,
                        yend = lat2,
                        xend = lon2
                        ),
                        size=DT3$Prop*20,
                        alpha=.5)+
                  scale_color_manual(values = mycolors)
          
          

                
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)
library(dbscan)

#1 - Read Data
    Haz<-readRDS(paste("Pittsburgh","_runDensityVars.rds",sep=""))
    Bus<-readRDS("Restaurants.rds")
    Bus<-Bus[Bus$city_super=="Pittsburgh",]
    
    # Get the map from google maps
    lon <- Bus$longitude
    lat <- Bus$latitude
    df <- as.data.frame(cbind(lon,lat))
    charlottemap <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 10,
                            maptype = "toner-lite", scale = "auto")
    
    
#2 - fix densities where no alters
    Haz$density.1<-ifelse(is.na(Haz$density.1),0,Haz$density.1)
    Haz$density.2<-ifelse(is.na(Haz$density.2),0,Haz$density.2)
  
#subset to 1 obs per city; just keep data for model
    Haz<-Haz[order(Haz$Interval,decreasing = T),]
    dups<-duplicated(Haz$ID)
    temp<-Haz[!dups,]

    ggmap(charlottemap) +
        geom_point(data = temp, aes(x = longitude, y = latitude),size=3, fill = ifelse(temp$density.1>=10 ,"blue","green"), shape = 21)
    
Bus$chain

summary(temp$density.1>=5& temp$density.1<30&temp$chain==0 &temp$Dissolved==1)

summary(temp$density.1>50&temp$chain==0 &temp$Dissolved==1)


table(temp$density.1>=10 & temp$density.1<30 )
#&temp$Dissolved==1
#    temp$chain==1 |



temp$density.4
