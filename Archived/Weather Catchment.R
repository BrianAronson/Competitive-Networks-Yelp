#1)
    #load weather data
        setwd("C:/Users/bda13/Desktop/Sociology/Statistics/Weather")
        weather<-read.csv("weather_description.csv")
    #subset weather data
        names(weather)
        cities<-"Pittsburgh"
        weather<-weather[,c("datetime",cities)]
    #find weather years
        temp<-as.character(weather$datetime)
        datefun<-function(x) as.numeric(substr(x,1,4))+(as.numeric(substr(x,6,7))-1)/12 + (as.numeric(substr(x,9,10))-1)/365
        weather$datetime<-datefun(temp)
        daterange<-c(min(weather$datetime),max(weather$datetime))
        #just extract weather at noon.
        rows<-1:floor(nrow(weather)/24)
        rows<-(rows-1)*24+1
        weather<-weather[rows,]
    #find unique weather types
        weathertypes<-as.data.frame(table(unlist(weather[,c(cities)])))
        weathertypes<-weathertypes[order(weathertypes$Freq),]
    
#2) 
    #load Yelp data
        setwd(prep$baseDir)
        Rev<-readRDS("ReviewData.rds")
        # User<-readRDS("UserData.rds")
        setwd(prep$derDir)
        Bus<-readRDS("Restaurants.rds")        
    #subset to useful info
        Bus<-Bus[Bus$city_super %in% c(cities),]
        Rev<-Rev[Rev$business_id %in% Bus$id,]
    #subset to years that match weather
        datefun<-function(x) as.numeric(substr(x,1,4))+(as.numeric(substr(x,6,7))-1)/12 + (as.numeric(substr(x,9,10))-1)/365
        Rev$date2<-datefun(Rev$date)
        Rev<-Rev[Rev$date2>daterange[1] & Rev$date2<daterange[2],]
        
    #find users who left 20-40 reviews (i.e. moderate users)
        # User<-User[User$user_id %in% Rev2$user_id,]
        users<-as.data.frame(table(Rev$user_id))
        users<-users[users$Freq>=20 & users$Freq<=40,]
        names(users)<-c("id","freq")
    #subset rev to those users
        Rev<-Rev[Rev$user_id %in% users$id,]
    #subset Bus to Bus in rev
        Bus<-Bus[Bus$id %in% Rev$business_id,]
    #find distance between each restaurant
        a <- cbind(Bus$longitude,Bus$latitude)
        temp<-c(distm(a))
        Dist<-temp*0.000621371
        
#3) get info about places each user reviewed; For each user: 
    #Find the latitude and longitude of places they reviewed.
        geos<-list()
        geofun<-function(x){
          buses<-Rev$business_id[Rev$user_id==x]
          geos<-data.frame(lat= Bus$latitude[Bus$id%in% buses],lon= Bus$longitude[Bus$id%in% buses])
          return(geos)
        }
        geos<-lapply(users$id,geofun)
    #Find the mean location they reviewed from
        meanlat<-sapply(geos,function(x) mean(x[,1]))
        meanlon<-sapply(geos,function(x) mean(x[,2]))
    #Find the median location they reviewed from
        medianlat<-sapply(geos,function(x) median(x[,1]))
        medianlon<-sapply(geos,function(x) median(x[,2]))
    #Find the distance between the places they reviewed.
        distf<-function(x){
          lon<-x[,2]
          lat<-x[,1]
          a <- cbind(lon,lat)
          Dist<-(distm(a))*0.000621371
          diag(Dist)<-max(Dist)
          return(Dist)
        }
        geosdist<-lapply(geos,distf)
    #Calculate min distance for each restaurant they reviewed
        mindists<-lapply(geosdist, function(x) apply(x,1, min))
    #calculate dist differences from user medians and means
        #distance function from user
            library(geosphere)
            distfun<-function(l1,l2,geo){
              dist<-vector()
              for(i in 1:nrow(geo)){
                dist[i] <- distm(c(l1,l2),c(geo[,2][i],geo[,1][i]), fun = distHaversine)*0.000621371
              }
              return(dist)
            }
        #mean dists
            meandists<-list()
            for(i in 1:length(geos)){
              dists[[i]]<-distfun(l1=meanlon[i],l2=meanlat[i],geo=geos[[i]])
              print(i)
            }
        #median dists
            mediandists<-list()
            for(i in 1:length(geos)){
              dists[[i]]<-distfun(l1=medianlon[i],l2=medianlat[i],geo=geos[[i]])
              print(i)
            }
    #Put this all into users df
        users<-data.table(users)
        users$lgeos<-geos
        users$lmindists<-mindists
        users$lmeandists<-meandists
        users$lmediandists<-mediandists
        users$meanlat<-meanlat
        users$meanlon<-meanlon
        users$medianlat<-medianlat
        users$medianlon<-medianlon
  
#4) get stats based on weather.
    #get weather into reviews df
        Rev$datetime<-datefun(Rev$date)
        con.order<-Rev$review_id
        weather<-data.table(weather)
        Rev2<-data.table(Rev)
        Rev2<-merge(Rev,weather,by="datetime")
        Rev2<-Rev2[match(con.order,Rev2$review_id),]
        Rev2$weather<-Rev2$Pittsburgh
        Rev2$Pittsburgh<-NULL
    #For each user, created index of which reviews were left on clear and rainy days
        sort(table(Rev2$weather))
        indexfun<-function(x,weath){
          revs<-Rev2[Rev$user_id==x,]
          temp<-(revs$weather==weath)
          return(temp)
        }
        clear<-lapply(users$id,function(x) indexfun(x,"sky is clear"))
        rain<-lapply(users$id,function(x) indexfun(x,"light rain"))
        
        "moderate rain"
        "sky is clear"
        
        # temp<-(as.data.frame(table(Rev$Pittsburgh)))
        # temp[order(temp$Freq),]
        # rainrev<-Rev[Rev$Pittsburgh=="moderate rain",]
        # clearrev<-Rev[Rev$Pittsburgh=="sky is clear",]
        
    #create a dataframe of consumer habits around median distance traveled 
        temp<-geos
        distsf<-function(a){
            a[,1]<-a[,1]-mean(a[,1])
            a[,2]<-a[,2]-mean(a[,2])
            a<-a*69
        return(a)
        }
        temp2<-lapply(temp,distsf)
        temp3<-do.call(rbind,temp2)
        df<-data.frame(x=temp3[,1],y=temp3[,2])
        

    #example of one person
        ggplot(temp2[[32]],aes(x=lon,y=lat)) + geom_point()
        ggplot(temp2[[56]],aes(x=lon,y=lat)) + geom_point(size=5)
        ggplot(temp2[[42]],aes(x=lon,y=lat)) + geom_point()
        
        png(file="C:/Users/bda13/Desktop/Single consumer distance habits.png", height=10, width=14,units = "in",res = 300)
        ggplot(temp2[[41]],aes(x=lon,y=lat)) + geom_point(size=5)+
        labs(x="Longitude", y="Latitude")+
        scale_x_continuous(limits=c(-2,2),breaks=(-2:2))+
        scale_y_continuous(limits=c(-2,2),breaks=(-2:2))+
        theme_bw() +
        theme(panel.border = element_blank(),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text=element_text(size=22),
          axis.title=element_text(size=24),
          title =element_text(size=32, face='bold'),
          legend.position="none")
        dev.off()
        
     
        temp<-geos
        distsf<-function(a){
          a[,1]<-a[,1]-median(a[,1])
          a[,2]<-a[,2]-median(a[,2])
          a<-a*69
          return(a)
        }
        temp2<-lapply(temp,distsf)
        temp3<-do.call(rbind,temp2)
        df<-data.frame(x=temp3[,1],y=temp3[,2])
        
    # Prep info for a contour map
        df2 = kde2d(df$x, df$y, n = 1000)
        df2 = data.frame(expand.grid(x=df2$x, y=df2$y), z=as.vector(df2$z))
        df2$z2<-df2$z^(1/2)
        # df2$z2<-df2$z^1
        
        
        # df3<-df[df$x<12 & df$x>(-12) &df$y<12 & df$y>(-12),]
        # df3 = kde2d(df3$x, df3$y, n = 100)
        # df3 = data.frame(expand.grid(x=df3$x, y=df3$y), z=as.vector(df3$z))
        # df3$z2<-df3$z

    #4 - Create
        #plot and color a particular attribute
        png(file="C:/Users/bda13/Desktop/Consumer distance habits.png", height=10, width=14,units = "in",res = 300)
            ggplot() +
              scale_fill_gradient2(low = "white", mid="white", high="#1bc3e5",midpoint = .001)+
              labs(x="Longitude", y="Latitude")+
              geom_raster(data=df2,aes(x=x,y=y,fill = z2))+
              geom_contour(data=df2,aes(x=x,y=y,z=z2, colour = ..level..),breaks=seq(min(df2$z2), max(df2$z2), length.out=30),size=.5)+
              scale_x_continuous(limits=c(-12,12))+
              scale_y_continuous(limits=c(-12,12))+
              theme_bw() +
              theme(panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text=element_text(size=22),
                axis.title=element_text(size=24),
                title =element_text(size=32, face='bold'),
                legend.position="none")
        dev.off()
    #grab raw statistics on distance habits
      temp1<-vector()
        for(x in (1:10)/2) temp1[x*2]<-(nrow(df[abs(df$x)<x & abs(df$y)<x,])/nrow(df))
      tempdf<-data.frame(dist=(1:10)/2,percent=round(temp1,2))
    #how about when it's raining?
      sort(table(Rev2$weather))
      df2<-df[unlist(lapply(users$id,function(x) indexfun(x,"heavy intensity rain"))),]
      # df2<-df[unlist(lapply(users$id,function(x) indexfun(x,"light snow"))),]
      # df2<-df[unlist(lapply(users$id,function(x) indexfun(x,"light rain"))),]
      # df2<-df[unlist(lapply(users$id,function(x) indexfun(x,"moderate rain"))),]
      df3<-df[unlist(clear),]
      temp2<-vector()
      temp3<-vector()
      for(x in 1:10) {
        temp2[x]<-(nrow(df2[abs(df2$x)<x/2 & abs(df2$y)<x/2 &  (abs(df2$x)>=(x/2-.5) | abs(df2$y)>=(x/2-.5)),])/nrow(df2))
        temp3[x]<-(nrow(df3[abs(df3$x)<x/2 & abs(df3$y)<x/2 &  (abs(df3$x)>=(x/2-.5) | abs(df3$y)>=(x/2-.5)),])/nrow(df3))
      }
      tempdf2<-data.frame(dist=(1:10)/2,rain=round(temp2,2),clear=round(temp3,2))
      tempdf2
      
  #How about min distances?
      temp<-geos
      distsf<-function(a){
        a[,1]<-a[,1]-median(a[,1])
        a[,2]<-a[,2]-median(a[,2])
        a<-a*69
        return(a)
      }
      temp2<-lapply(temp,distsf)
      temp3<-do.call(c,mindists)
      df<-data.frame(x=temp3)

      # df2<-df[unlist(lapply(users$id,function(x) indexfun(x,"heavy intensity rain"))),]
      # df2<-df[unlist(lapply(users$id,function(x) indexfun(x,"light snow"))),]
      # df2<-df[unlist(lapply(users$id,function(x) indexfun(x,"light rain"))),]
      df2<-df[unlist(lapply(users$id,function(x) indexfun(x,"moderate rain"))),]
      df3<-df[unlist(clear),]
      temp2<-vector()
      temp3<-vector()
      for(x in 1:10) {
        temp2[x]<-(length(df2[df2<x/2 & df2>=(x/2-.5)])/length(df2))
        temp3[x]<-(length(df3[df3<x/2 & df3>=(x/2-.5)])/length(df3))
      }
      tempdf2<-data.frame(dist=(1:10)/2,rain=round(temp2,2),clear=round(temp3,2))
      tempdf2
      
      
  #null model correlation
      City="Pittsburgh"
      null<-readRDS(paste(Directory,City, "_nullties.rds",sep=""))
      dist<-readRDS(paste(Directory,City, "_dist.rds",sep=""))
      weighted.mean(null$Proportion[dist<=2.5 & dist>2],w=null$Expected[dist<=2.5 & dist>2])
      mean(null$Proportion[dist<=2.5 & dist>2])
      null$Proportion<-null$Observed/null$Expected
      #Now I remember. I added 1 to everything to make it less skewed...
      null$Observed<-null$Observed-1
      null$Expected<-null$Expected-1
      
      

      temp<-vector()
      for(x in 1:10){
        temp[x]<-sum(null$Observed[dist<(x/2) & dist>(x/2-.5)])/sum(null$Expected[dist<(x/2) & dist>(x/2-.5)])
      }
      tempdf2<-data.frame(dist=(1:10)/2,prop=round(temp,2))
      tempdf2
      sum(null$Observed[dist<.1])/sum(null$Expected[dist<.1])
      
      
  #just for the sake of comparison, look at alcohol expected vs prop...
      temp<-vector()
      for( i in 1:ncol(Bus)){
        tryCatch({
          a<-rep(Bus[,i],each=nrow(Bus))
          b<-rep(Bus[,i],nrow(Bus))
          c<-a & b
          temp[i]<-sum(null$Observed[c & dist>10])/sum(null$Expected[c & dist>10])
          print(i)
        }, error = function(e) e)
      }
      tdf<-data.frame(name=names(Bus),prop=temp)
      tdf[order(tdf$prop),]
      
  # #I think i did it wrong... I didn't control for possible number of distances <.5 and customer overlaps
  #     dist<-readRDS(paste(Directory,City, "_dist.rds",sep=""))
  #     ties<-readRDS(paste(Directory,City, "_cust.rds",sep=""))
  #     dt<-data.table(Bus1=rep(1:nrow(Bus),each=nrow(Bus)),Bus2=rep(1:nrow(Bus),nrow(Bus)),ties=ties,dist=dist)
  #     dt2<-dt[dt$Bus1!=dt$Bus2 & dt$ties!=0,]
  #     #this is almost correct, except ties are supposed to be reciprocal...
  #     temp<-vector()
  #     for(i in 1:50){
  #     dt2$Bus3<-sample(dt2$Bus2,nrow(dt2))
  #     dt2$dist3<-dt$dist[match(paste(dt2$Bus1,dt2$Bus3),paste(dt$Bus1,dt$Bus2))]
  #     temp[i]<-sum(dt2$dist3<=.5)
  #     print(i)
  #     }
  #     sum(dt2$dist<=.5)
  #     expected=sum(ties[dist<.5])
  #     ties<-as.matrix(ties,nrow=sqrt())
      
      
      
      summary(users$Freq)
      
      a<-data.frame(table(users$Freq))
      # a<-a[as.numeric(as.character(a$Var1))>5,]
      quantile(a$Freq,(c(1:20)/20))      
      
      a$Freq2<-a$Freq*as.numeric(as.character(a$Var1))
      a$prop1<-cumsum(a$Freq)
      a$prop1<-a$prop1/sum(a$Freq)
      a$prop2<-cumsum(a$Freq2)
      a$prop2<-a$prop2/sum(a$Freq2)
      
      a[1:50,]
      
      #Only 4% of the sample left more than 50 restaurant reviews, But these reviews accounted for 26% of all reviews left.