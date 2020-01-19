#0 - Set Directory
    setwd(Directory)

#1 - Read Data
    Haz<-readRDS("HazInfo.RDS")
    su<-readRDS("suHazInfo.RDS")

#2 - Prepare data
    #create a version of Haz With the subset used in my analyses
        Haz2<-Haz[su,]
    #sort data
        Haz2<-Haz2[order(Haz2$ID,Haz2$Interval),]
    #add an age-based interval variable
        temp<-ifelse(duplicated(Haz2$ID),0,1)
        for(i in 1:length(temp)){
          if(temp[i]==0){
            temp[i]<-(temp[i-1]+1)
          }
        }
        Haz2$interage<-temp
        Hdens<-Haz2[Haz2$t]
    #prep summary variables
        d<-data.frame(age=1:12)
        d$count<-0
        d$tot<-0
        crowd<-d
        uncrowd<-d
        compr<-d
        uncompr<-d
        centr<-d
        uncentr<-d
        countfun<-function(x){
          sum(Haz2$interage==d$age[i] & x>median(x[x!=0],na.rm=T) & x>0 & Haz2$Dissolved==1,na.rm=T)
        }
        totfun<-function(x){
          sum(Haz2$interage==d$age[i] & x>median(x[x!=0],na.rm=T) & x>0,na.rm=T)
        }
    #Gather summaries of relevant info
        for(i in 1:nrow(d)){
          d$count[i]<-sum(Haz2$Dissolved[Haz2$interage<(d$age[i]+1) & Haz2$interage>=(d$age[i])],na.rm=T)
          d$tot[i]<-sum(Haz2$interage==d$age[i],na.rm=T)
          crowd$count[i]<-countfun(Haz2$tlocalcrowd.3.1)
          crowd$tot[i]<-totfun(Haz2$tlocalcrowd.3.1)
          compr$count[i]<-countfun(Haz2$tlocalwidth3.1)
          compr$tot[i]<-totfun(Haz2$tlocalwidth3.1)
          centr$count[i]<-countfun(Haz2$tlocalcentrality.3)
          centr$tot[i]<-totfun(Haz2$tlocalcentrality.3)
       }
    #correct labels
        d$age<-d$age/2
        crowd$age<-crowd$age/2
        compr$age<-compr$age/2
        centr$age<-centr$age/2
        d$perc<-d$count/d$tot
        crowd$perc<-crowd$count/crowd$tot
        compr$perc<-compr$count/compr$tot
        centr$perc<-centr$count/centr$tot
        d$Type<-" All Restaurants"
        crowd$Type<-" Crowded"
        compr$Type<-" Compressed"
        centr$Type<-" Centralized"
        tdf<-rbind(d,crowd,compr,centr)
    # Select palette:
        cbPalette <- c("#000000", "#1bc3e5", "#60d5ed","#4c4c4c", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        tdf$talpha<-rep(c(1,1,.99,.99),each=nrow(tdf)/4)

#3 - Make more variables
    #find death count per week
      Bus<-readRDS("Restaurants.rds")
      Bus$death<-round((as.numeric(Bus$date_close)-as.numeric(Bus$date_open))/7)
      temp<-Bus[,c("id","death")]
      temp<-temp[temp$id%in% Haz2$ID[Haz2$Dissolved==1],]
      death<-as.data.frame(table(temp$death))
      death<-as.data.frame(sapply(death,function(x) as.numeric(as.character(x))))
      names(death)<-c("age","count")
    #fill in empty days
      death<-rbind(data.frame(age=setdiff((0:max(death)),death$age),count=0),death)
      death<-death[order(death$age),]
    #find alive count
      temp<-Bus[,c("id","death")]
      temp<-temp[temp$id%in% Haz2$ID,]
      alive<-death
      for (i in 1:nrow(alive)){
        alive$count[i]<-sum(temp$death>=alive$age[i])
      }
    #determine percent died in a single day
      death$perc<-death$count/alive$count*100
      death$age<-death$age/52.1
      
#4 - Plot
  png(file="C:/Users/bda13/Desktop/Fig 1 - Baseline Hazard Plot.png", height=8, width=8*1.8,units = "in",res = 72)
  ggplot(death[death$age<5,],aes(x=age,y=perc))+
    theme_bw()+
    geom_smooth(size=4,se=T,color="#1bc3e5")+
    coord_cartesian(ylim = c(0.1, .3))+
    labs(x="Years Since Founded", y="% Dissolved (per week)")+
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text=element_text(size=24),
      title =element_text(size=28), #, face='bold'
      legend.position="none",
      legend.text=element_text(size=24),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      axis.title.y = element_text(size=28, margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(size=26, margin = margin(t = 20, r = 0, b = 0, l = 0))
    )
    dev.off()
        
    
