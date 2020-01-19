#' 11- Create Distance adjacency list
#' @description This function returns a vector of the distances between each business, ordered id1, id2
#' @param Directory Stuff1
#' @param City Stuff2
#' @export
#' @import data.table
#' @import geosphere
#' @import XML
#' @import RCurl
yelp_Distance_Adj_Function2<-function(Directory,City){
  
    #Overview: This script does the following:
        #0 - Set Directory  
        #1 - Read Data
        #2 - Create temporary dataframe consisting of just the businesses in the specified city.
        #3 - Estimate physical distance between each business
        #4 - Save
  
    #0 - Set Directory
        setwd(Directory)
    
    #1 - Read Data
        Bus<-readRDS("Restaurants.rds")
    
    #2 - Create temporary dataframe consisting of just the businesses in the specified city.
        Bus2<-Bus[Bus$city_super==City,]
    
    #3 - Estimate physical distance between each business (super efficient method)
        a <- cbind(Bus2$longitude,Bus2$latitude)
        temp<-c(distm(a))
        
    #4 - convert to miles
        Dist<-temp*0.000621371
        
    #5 -estimate driving distance and time
        #create search function (edit api as necessary)
            latlon2ft <- function(x,y){
                    origin=x
                    destination=y
                    xml.url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false&key=HIDDEN')
                    xmlfile <- xmlTreeParse(getURL(xml.url))
                    duration<-xmlfile$doc$children$DistanceMatrixResponse[['row']][[2]][2][1][['duration']][['value']][[1]]
                    distance<-xmlfile$doc$children$DistanceMatrixResponse[['row']][[1]][3][1][['distance']][['value']][[1]]
                    duration <- as.numeric(unclass(duration)[['value']])
                    distance <- as.numeric(unclass(distance)[['value']])
                    duration <- duration/60 #seconds to minutes
                    distance <- distance*0.000621371 # FROM METER TO MILES
                    return(c(duration,distance))
            }
        #using distancematrixapi (title is a lie!!)
            #prep search parameters
                loc1=paste(Bus2$latitude,Bus2$longitude,sep=",")
                loc1<-loc1
                origin=paste(loc1,collapse="|")
                origin<-loc1[1]
                destination=paste(head(loc1,100),collapse="|")
            #search
                xml.url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false&key=')
                
            #parse
                xmlfile <- getURL(xml.url)
                xmlfile <- xmlTreeParse(xmlfile)
                responses<-xmlfile$doc$children$DistanceMatrixResponse
                durations<-getNodeSet(responses,"//row/element/duration/value")
                distances<-getNodeSet(responses,"//row/element/distance/value")
                durations<-as.numeric(sapply(durations,xmlValue))
                distances<-as.numeric(sapply(distances,xmlValue))
                    #tada!
                duration <- duration/60 #seconds to minutes
                distance <- distance*0.000621371 # FROM METER TO MILES
                
        #create table of variables to search for
            df<-data.table(Dist,Bus1=rep(Bus2$id,each=nrow(Bus2)),Bus2=rep(Bus2$id,nrow(Bus2)),loc1=rep(paste(Bus2$latitude,Bus2$longitude,sep=","),each=nrow(Bus2)),loc2=rep(paste(Bus2$latitude,Bus2$longitude,sep=","),nrow(Bus2)))
            #create variable indicating rank closeness of each bus to bus[i]
                Dist2<-matrix(Dist,nrow=sqrt(length(Dist)))
                Dist2<-apply(Dist2, 1, rank)
                Dist2<-c(Dist2)
                df$Dist2<-Dist2
            #create identifier of lower and upper triangles for vector
                temp<-matrix(0,nrow=sqrt(length(Dist)),ncol=sqrt(length(Dist)))
                temp[upper.tri(temp)]<-1
                temp<-c(temp)
                df$temp<-temp
            #Keep 30 closest places to each ego; remove duplicates
                df2<-df[df$Dist2<=51 &df$temp==1 &df$Dist<.3,]
                
        #Search for the time and driving distance by pairs
            loc1<-df2$loc1
            loc2<-df2$loc2
            results<-matrix(nrow=nrow(df2),ncol=2)
            for(i in 1:nrow(results)){
              z=0
              while(z==0){
                try({
                    results[i,]<-latlon2ft(loc1[i],loc2[i])
                    print(i/nrow(results)*100)
                    z=z+1
                })
              }
            }
            
            
            
            
            
        #create and save variables in larger dataframe
            #put results in df
                df3<-df2[,c("Bus1","Bus2")]
                df3$duration<-results[,1]
                df3$distance<-results[,2]
            #duplicate results by sender/receiver
                df4<-df3
                names(df4)[1:2]<-c("Bus2","Bus1")    
                df3<-rbind(df3,df4)
            #merge into bigger df
                df<-merge(df,df3,by=c("Bus1","Bus2"),all=T,sort=F)
                duration<-df$duration
                distance<-df$distance
                saveRDS(duration,paste(City, "_dist_dur.rds",sep=""))
                saveRDS(distance,paste(City, "_dist_dri.rds",sep=""))
}


