#1) read data
  setwd(Directory)
  ch<-readRDS("2013-2016_changes.rds")
  
#2) make last minute changes to data
  ch$rev_count.2013<-ch$rev_count.2013/1000
  ch$compression.20132<-ch$compression.2013^2
  ch$compression.20162<-ch$compression.2016^2
  ch$alive.20132<-ch$alive.2013^2
  ch$nc3c
  ch$nichechange2<-ch$mean.new.to.old.mins/ch$compression.2013
  ch$Income20132<-ch$Income2013^2
  ch$Unemploy20132<-ch$Unemploy2013^2
  ch$internationalMigrants20162<-ch$internationalMigrants2016^2
  ch$Migrants20162<-ch$Migrants2016^2
  ch$popdens20132<-ch$popdens2013^2
  ch$Vacant20132<-ch$Vacant2013^2
  ch$p.White.tb<-ifelse(ch$p.White.t<.8,1,0)
  ch$popdens2013.2<-ch$popdens2013
  ch$popdens2013.2[ch$popdens2013.2>7.5]<-7.5
  ch$Income2013.2<-ch$Income2013
  ch$Income2013.2[ch$Income2013.2>80]<-80
  ch$Vacant2013.2<-ch$Vacant2013
  ch$Vacant2013.2[ch$Vacant2013.2>.2]<-.2
  ch$Migrants2016.2<-ch$Migrants2016
  ch$Migrants2016.2[ch$Migrants2016.2>10]<-10
  ch$p.Unemploy.t.2<-ch$p.Unemploy.t
  ch$p.Unemploy.t.2[ch$p.Unemploy.t.2>2]<-2
  
#3) run models
  #1) simplest
      round(summary(glm(nichechange2~
          Income2013+Income20132+Vacant2013+Vacant20132+popdens2013+popdens20132+internationalMigrants2016+internationalMigrants20162+
          alive.2013+alive.20132,
          data=ch[ch$alive.2013>20 & ch$alive.2013<80 & ch$p.N<1.2 & ch$p.N>.9,]))$coefficients,3)[,3:4]

  #2) with changes (don't help)
      round(summary(glm(nichechange2~
          Income2013+Income20132+Vacant2013+Vacant20132+popdens2013+popdens20132+internationalMigrants2016+internationalMigrants20162+
          p.White.t+p.Income.t+p.Vacant.t+p.Unemploy.t+p.popdens.t+
          alive.2013+alive.20132,
          data=ch[ch$alive.2013>15 & ch$alive.2013<80 & ch$p.N<1.2 & ch$p.N>.9,]))$coefficients,3)[,3:4]
      
  #3) with business controls (don't help)
      round(summary(glm(nichechange2~
          Income2013+Income20132+Vacant2013+Vacant20132+popdens2013+popdens20132+internationalMigrants2016+internationalMigrants20162+
          alive.2013+alive.20132+
          p.chain.2013+age.2013+Price.2013+Hour_Open.2013,
          data=ch[ch$alive.2013>15 & ch$alive.2013<80 & ch$p.N<1.2 & ch$p.N>.9,]))$coefficients,3)[,3:4]
      
          
  #4) Full model 
      round(summary(glm(nichechange2~
          Income2013.2+Vacant2013.2+popdens2013.2+internationalMigrants2016+
          alive.2013+
          modules.2013*modularity.2013,
          data=ch[ch$alive.2013>20 & ch$alive.2013<80 & ch$p.N<1.2 & ch$p.N>.9,]))$coefficients,3)[,c(3:4)]

