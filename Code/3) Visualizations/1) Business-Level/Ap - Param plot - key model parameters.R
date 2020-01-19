#if using pam
    a<-summary(pam3)$p.table
    a<-cbind(row.names(a),a)
    mod1<-as.data.frame(a)
    mod1$coef<-as.numeric(as.character(mod1$Estimate))
    mod1$`se(coef)`<-as.numeric(as.character(mod1$`Std. Error`))
    mod1$`Pr(>|z|)`<-as.numeric(as.character(mod1$`Pr(>|z|)`))
    
#create variables variable
    df$Variable<-row.names(df)
    
#create CI variable
    df$CIlower<-df$coef-df$`se(coef)`*1.96
    df$CIupper<-df$coef+df$`se(coef)`*1.96
    
#create color variable
    df$Significant<-factor(ifelse(df$`Pr(>|z|)`<.05,1,0))
    
#exponentiate
    df$coef<-exp(df$coef)
    df$CIlower<-exp(df$CIlower)
    df$CIupper<-exp(df$CIupper)
    df$Estimate<-df$coef

#keep variables of interest
    df<-df[df$Variable %in% c("tlocalcentrality.3","tlocalwidth3.1", "tlocalcrowd.3.1","tstars_cur", "tIncome","tUnemploy"),]
    df<-df[c(6,5,4,1,2,3),]
    df$Variable<-c("Centrality","Compression",  "Crowding", "Stars", "Income","Unemployment")
    df<-df[c(6,5,4,3,2,1),]
    df$Variable<-factor(df$Variable,levels = df$Variable)
            
#Plot
    cbPalette <- c("#000000","#000000","#000000","#000000", "#1bc3e5","#1bc3e5")
    png(file="C:/Users/bda13/Desktop/Param Estimates.png", height=9, width=12.5,units = "in",res = 72)
          p <- ggplot(df, aes(y=Estimate,x=(Variable)))
                  p + geom_pointrange(aes(ymin=CIlower,ymax=CIupper),size=2,shape=73,fatten=6,color=cbPalette) + coord_flip()+
                  labs(x="Parameter", y="Hazard Ratio")+
                  geom_hline(yintercept = 1)+
                  theme_bw() + 
                  theme(panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black"),
                      axis.text=element_text(size=22),
                      axis.title=element_text(size=24),
                      title =element_text(size=32, face='bold'),
                      legend.position="none")
                  dev.off()


#change Variables of interest
    df<-df[df$Variable %in% c("tlocalcentrality.3","tlocalwidth3.1", "tlocalcrowd.3.1","tstars_cur", "tatyp"),]
    df<-df[c(4,5,3,1,2),]
    df$Variable<-c("Centrality","Compression",  "Crowding", "Stars", "Atypicality")
    df<-df[c(4,5,3,1,2),]
    df$Variable<-factor(df$Variable,levels = df$Variable)

#Plot
    cbPalette <- c("#000000","#000000","#000000", "#1bc3e5","#1bc3e5")
    png(file="C:/Users/bda13/Desktop/Param Estimates.png", height=9, width=12.5,units = "in",res = 72)
    p <- ggplot(df, aes(y=Estimate,x=(Variable)))
    p + geom_pointrange(aes(ymin=CIlower,ymax=CIupper),size=2,shape=73,fatten=6,color=cbPalette) + coord_flip()+
      labs(x="Parameter", y="Hazard Ratio")+
      geom_hline(yintercept = 1)+
      theme_bw() + 
      theme(
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=22),
        axis.title=element_text(size=24),
        title =element_text(size=32, face='bold'),
        legend.position="none")
    dev.off()

