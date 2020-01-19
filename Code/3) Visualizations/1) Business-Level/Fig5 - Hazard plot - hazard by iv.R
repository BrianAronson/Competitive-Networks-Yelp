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
    #Prop summary variables
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

#3 - Plot
    png(file="C:/Users/bda13/Desktop/Fig 5 - Hazard Plots by Niche Changes.png", height=9, width=14.5,units = "in",res = 72)
      gg<-ggplot(data=tdf[tdf$age>=1,], aes(age,y=perc,group=Type,color=Type)) +
      geom_smooth(size=4,se=F)+
      labs(x="Years Since Founded", y="% Dissolved (per half year)")+
      scale_y_continuous(limits=c(0,.1),labels=percent)+
      scale_x_continuous(limits=c(1,7.4),breaks=c(1,2,3,4,5,6))+
      theme_bw() +
      theme(
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text=element_text(size=31),
          title =element_text(size=32), #, face='bold'
          plot.title = element_text(hjust = 1),
          legend.position="none",
          legend.text=element_text(size=31),
          plot.margin = margin(1, 1, 1, 1, "cm"),
          axis.title.y = element_text(size=33, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size=31, margin = margin(t = 20, r = 0, b = 0, l = 0))
          )+
      scale_colour_manual(values=cbPalette)
      print(direct.label(gg,list("last.points",cex=2.3)))
    dev.off()
    