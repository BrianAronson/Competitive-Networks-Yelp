#1) load data
    setwd(Directory)
    ch<-readRDS("2013-2016_changes.rds")
    
    
#3) plot subset criteria
    quantile(ch$alive.2013,(0:20)/20)
    #a) prep main data
        ldf2<-data.frame(Statistic=rep(c("N","Growth Rate"),each=nrow(ch)),
                         Estimate=c(ch$alive.2013,ch$p.N),
                         Included=rep(ch$alive.2013>15 & ch$alive.2013<80 & ch$p.N<1.2 & ch$p.N>.9,2),
                         threshold=rep(15,1.2,each=nrow(ch)),
                         temp=1
                         )
    #b) prep thresholds
        temp=data.frame(Statistic = c("Growth Rate","N"), threshold = c(1.2,15))
        temp2=data.frame(Statistic = c("Growth Rate","N"), threshold = c(.9,80))
    #c) plot and save
        png(file="C:/Users/bda13/Desktop/Subset Criteria and Thresholds.png", height=5, width=7,units = "in",res = 300)
        ggplot(data=ldf2, aes(x=temp, y=Estimate)) +
          geom_hline(data=temp,aes(yintercept=threshold), color="red", alpha=0.6)+
          geom_hline(data=temp2,aes(yintercept=threshold), color="red", alpha=0.6)+
          # scale_color_grey(start = .8, end = 0)+
          geom_jitter(color=ifelse(ldf2$Included,"blue1","grey"))+
          facet_wrap(~Statistic,scales="free",nrow=1)+
          # geom_violin(fill=rgb(1,1,1,alpha=0.6), color="red")+
          # scale_color_discrete(name = "type")+
          theme_bw()+
          ggtitle("Subset Criteria and Thresholds")+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"),
                legend.text=element_text(size=12),
                text=element_text(family="A"),
                legend.title=element_text(size=12,face="bold"),
                title = element_text(size=12,face="bold"),
                # legend.position="bottom",
                axis.text.x = element_blank(),
                # axis.text.y = element_blank(),
                axis.text.y = element_text(size=11.5),
                # axis.title.x= element_text(size=12,face="bold",margin = margin(t = 10, r = 0, b = 0, l = 0)),
                # axis.title.y= element_text(size=13.5,face="bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
                axis.title.x= element_blank(),
                axis.title.y= element_blank(),
                strip.text.x = element_text(size = 14),
                axis.ticks.x = element_blank())
        dev.off()
