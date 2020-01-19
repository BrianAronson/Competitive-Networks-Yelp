rm(list = ls())
gc()

  #Determine reviews by interval    
    # Read data
      Review<-readRDS("ReviewData.rds")
    # Create count variable
      Review$count<-1
    # Create intervals
      Interval=c(2009+(0:28)/4)
    # Convert dates to numbers
      Review$date<-year(Review$date)+round(month(Review$date))/12
    # Remove reviews prior to 2009
      Review<-Review[Review$date>=2009,]
    # Find which interval each date falls into
      Review$Interval<-Interval[findInterval(Review$date, Interval)]
    # Count number of reviews per interval by business
      temp<-aggregate(count ~ business_id + Interval, data = Review, FUN=sum)
    # Merge interval data with uniquebusines
      temp2<-uniquebus
      for(i in 1:length(Interval)){
        names(temp)[3]<-paste("RI",Interval[i],sep="")
        temp2<-merge(temp[temp$Interval==Interval[i],c("business_id",names(temp)[3])],temp2,all.y=T)
        print(i)
      }
    # Find frequency of reviews by time after founded
      uniquebus<-uniquebus[uniquebus$DateFounded>=2009,]
      Reviews<-data.frame(I1=vector(length=length(uniquebus[,1])))
      for(i in 1:length(uniquebus[,1])){
        Reviews$I1[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i], Interval)],sep=""))]
        Reviews$I2[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+.25, Interval)],sep=""))]
        Reviews$I3[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+.5, Interval)],sep=""))]
        Reviews$I4[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+.75, Interval)],sep=""))]
        Reviews$I5[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+1.00, Interval)],sep=""))]
        Reviews$I6[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+1.25, Interval)],sep=""))]
        Reviews$I7[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+1.5, Interval)],sep=""))]
        Reviews$I8[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+1.75, Interval)],sep=""))]
        Reviews$I9[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+2.00, Interval)],sep=""))]
        Reviews$I10[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+2.25, Interval)],sep=""))]
        Reviews$I11[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+2.5, Interval)],sep=""))]
        Reviews$I12[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+2.75, Interval)],sep=""))]
        Reviews$I13[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+3.00, Interval)],sep=""))]
        Reviews$I14[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+3.25, Interval)],sep=""))]
        Reviews$I15[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+3.5, Interval)],sep=""))]
        Reviews$I16[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+3.75, Interval)],sep=""))]
        Reviews$I17[i]<-temp2[i,names(temp2)==(paste("RI",Interval[findInterval(uniquebus$DateFounded[i]+4.00, Interval)],sep=""))]
        print(i)
      }
    
      Reviews<-Reviews[!is.na(Reviews$I17),]
      
    # plot review count 
      # create data frame of count by interval
      Count<-c(Reviews$I1,Reviews$I2,Reviews$I3,Reviews$I4,Reviews$I5,Reviews$I6,Reviews$I7,Reviews$I8,Reviews$I9,Reviews$I10,Reviews$I11,Reviews$I12,Reviews$I13,Reviews$I14,Reviews$I15,Reviews$I16,Reviews$I7)
      Interval<-c(rep(1:17,each=length(Reviews[,1])))
      temp<-data.frame(Count,Interval)
      pdf(file="Reviews by Interval.pdf", height=8, width=12)
      ggplot(NULL, aes(Interval,y=Count)) + 
        #geom_jitter(data = temp[!is.na(temp$Count),],color="grey70")+
        geom_smooth(data=temp[!is.na(temp$Count),],size=2,color="firebrick")
        ggtitle("Reviews by Interval") +
        labs(x="Interval", y="Reviews")
      dev.off()
      
   