#0 - Set Directory
    setwd(Directory)

#1 - Read Data
    ch<-readRDS("2013-2016_changes.rds")
    ch<-ch[ch$alive.2013>15 & ch$alive.2013<80 & ch$p.N<1.2 & ch$p.N>.9,]
    varnames<-c("nichechange","mean.new.to.old.mins","p.sum.died","p.N","modularity.2013","modules.2013","pop2013","alive.2013","area","popdens2013","Unemploy2013","White2013","Income2013","Migrants2013","p.Income.t","p.popdens.t","p.White.t","compression.2013","dist_center.2013","p.chain.2013","age.2013","stars.2013","Price.2013")
    ch<-as.data.frame(ch)
    ch<-ch[,varnames]

#for the sake of simplicity, median impute nas
    for(i in 1:ncol(ch)){
      ch[,i][is.na(ch[,i])]<-median(ch[,i],na.rm=T)
    }

#2 - Correlation tables
    #create correlation matrix
        cormat<-cor(ch,use="complete")
        temp<-c("Niche Change","Niche Change (alt)","Proportion Died","Growth Rate","Modularity","# Modules","Population","# Restaurants","Area","Population density","Unemployment","White","Income","Migrants",expression(Delta~Income),expression(Delta~Population),expression(Delta~White),"Compression","Distance from City Center","Proportion Chains","Average Age","Average Stars","Average Price")

    #reduce number of vars
        cormat<-cormat[c(1,3:14),c(1,3:14)]
        temp<-temp[c(1,3:14)]

    #reformat for graphing
        cormat<-round(cormat,2)
        get_upper_tri <- function(cormat){
          cormat[lower.tri(cormat)]<- NA
          return(cormat)
        }
        upper_tri <- get_upper_tri(cormat)
        library(reshape2)
        melted_cormat <- melt(upper_tri, na.rm = TRUE)
        melted_cormat$val2<-melted_cormat$value
        melted_cormat$val2<-sprintf("%.2f", melted_cormat$val2)
        melted_cormat$val2[melted_cormat$value==1]<-""
        melted_cormat[melted_cormat$Var1==melted_cormat$Var2,]$value<-0
        melted_cormat$val2<-ifelse(sign(as.numeric(melted_cormat$val2))==-1,paste("(",(str_sub(melted_cormat$val2,2,5)),")",sep=""),melted_cormat$val2)

    #create basic heatmap
        ggheatmap<-ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
          geom_tile(color = "white")+
          scale_fill_gradient2(low = "#1bc3e5", high = "#e53d1b",
                               midpoint = 0, limit = c(-1,1), space = "Lab",
                               name="Pearson\nCorrelation") +
          theme_minimal()+
          theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 20, hjust = 1),
                axis.text.y = element_text(size = 20))+
          coord_fixed()

    ##Graph heatmap
        png("C:/Users/bda13/Desktop/Fig 2 - Correlations2.png", height=12, width=18,units = "in",res = 72)
        ggheatmap +
          geom_text(aes(Var2, Var1, label = val2), color = "black", size = 4.8,hjust=.5) +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            plot.title = element_text(size=28, hjust=.38),
            plot.margin = margin(.5, 4, 0, 0, "cm"))+
          guides(fill = guide_colorbar(barwidth = 4, barheight = 15))+
          ggtitle("Figure 2: Correlations Among All Variables")+
          scale_x_discrete(labels= temp)+
          scale_y_discrete(labels= temp)
        dev.off()
