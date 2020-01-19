#0 - Set Directory
    setwd(Directory)

#1 - Read Data
    ch<-readRDS("2013-2016_changes.rds")
    ch<-ch[ch$alive.2013>15 & ch$alive.2013<80 & ch$p.N<1.2 & ch$p.N>.9,]
    ch<-ch[,c("nichechange","mean.new.to.old.mins","p.sum.died","p.sum.new","p.N","modularity.2013","modules.2013","Income2013","Migrants2013","pop2013","popdens2013","Price.2013","Unemploy2013","Vacant2013","White2013","p.Income.t","p.popdens.t","p.White.t","compression.2013","dist_center.2013","p.chain.2013","age.2013","alive.2013","area","rev_count.2013","stars.2013")]
    ch<-as.data.frame(ch)

#2 - Descriptives
    descr<-data.frame(Variable=names(ch),
                      Mean=0,
                      SD=0,
                      Min=0,
                      Max=0)
    for(i in 1:ncol(ch)){
      descr$Mean[i]=mean(ch[,i],na.rm = T)
      descr$SD[i]=sd(ch[,i],na.rm = T)
      descr$Min[i]=min(ch[,i],na.rm = T)
      descr$Max[i]=max(ch[,i],na.rm = T)
    }
  