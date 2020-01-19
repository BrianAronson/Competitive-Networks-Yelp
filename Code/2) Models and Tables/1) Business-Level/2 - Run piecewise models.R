library(pammtools)
library(survival)
library(ggplot2)
library(mgcv)
theme_set(theme_bw())

#0 - Set Directory
    setwd(Directory)

#1 - Read Data
    Haz<-readRDS("HazInfo2.RDS")
    su<-readRDS("suHazInfo2.RDS")

#2 - Prepare subset conditions for models
    dups<-duplicated(Haz$ID)
    temp<-Haz[!dups,]
    ids<-temp$ID[temp$chain==0 &temp$date_open>=2011]
    ids<-as.character(ids)
    su<-Haz$ID %in% ids

#3 - Make last minute data transformations
    Haz$tlocalcentrality.3[Haz$tlocalcentrality.3<0]<-0
    Haz$tlocalwidth3.1[Haz$tlocalwidth3.1<0]<-0
    Haz$tlocalcrowd.3.1[Haz$tlocalcrowd.3.1<0]<-0
    I$popularity<-I$rev_count*I$age
    I$popularity2<-I$popularity^2
    I$rev_count2<-I$rev_count^2
    

#4 - Subset data
    I <- Haz[su,]
    I <- I[,c("Dissolved","Interval","age","age2","tstars_cur","tstars_int","tdensity.3.1","tatyp","tMigrants","tIncome","tUnemploy","tVacant","tWhite","tpopdens","stars_cur","stars_int","city_super","density.1","atyp","Migrants2016","Income2016","Unemploy2016","Vacant2016","White2016","popdens2016","localcrowd.0.1","tlocalcrowd.3.1","localwidth3.1","tlocalwidth3.1","localcentrality.1","tlocalcentrality.3","rev_count")]
    I <- I[rowSums(is.na(I))== 0,]


#5 - Prepare formulas
    f1<-formula(Dissolved ~ s(Interval) +age+age2 + tstars_cur +  + tdensity.3.1 + 
                  tatyp + tMigrants + tIncome + tUnemploy + tVacant + tWhite + 
                  tpopdens + stars_cur +  + city_super + density.1 + 
                  atyp + Migrants2016 + Income2016 + Unemploy2016 + Vacant2016 + 
                  White2016 + popdens2016 + localcrowd.0.1 + tlocalcrowd.3.1)
    
    f2<-formula(Dissolved ~ s(Interval) +age+age2 + tstars_cur +  + tdensity.3.1 + 
                  tatyp + tMigrants + tIncome + tUnemploy + tVacant + tWhite + 
                  tpopdens + stars_cur +  + city_super + density.1 + 
                  atyp + Migrants2016 + Income2016 + Unemploy2016 + Vacant2016 + 
                  White2016 + popdens2016 + localcrowd.0.1 + tlocalcrowd.3.1 + 
                  localcentrality.1 + tlocalcentrality.3)
    
    f3<-formula(Dissolved ~ s(Interval) +age+age2 + tstars_cur +  + tdensity.3.1 + 
                  tatyp + tMigrants + tIncome + tUnemploy + tVacant + tWhite + 
                  tpopdens + stars_cur +  + city_super + density.1 + 
                  atyp + Migrants2016 + Income2016 + Unemploy2016 + Vacant2016 + 
                  White2016 + popdens2016 + localcrowd.0.1 + tlocalcrowd.3.1 + 
                  localcentrality.1 + tlocalcentrality.3 + localwidth3.1 + tlocalwidth3.1)
    
    
    f4<-formula(Dissolved ~ s(Interval) +age+age2 + tstars_cur +  + tdensity.3.1 + 
                  tatyp + tMigrants + tIncome + tUnemploy + tVacant + tWhite + 
                  tpopdens + stars_cur +  + city_super + density.1 + 
                  atyp + Migrants2016 + Income2016 + Unemploy2016 + Vacant2016 + 
                  White2016 + popdens2016 + localcrowd.0.1 + tlocalcrowd.3.1 + 
                  localcentrality.1 + tlocalcentrality.3 + localwidth3.1 + tlocalwidth3.1+rev_count+rev_count2+popularity+popularity2)

#6 - run models
    pam1<-gam(f1,data =  I,family = poisson())
    pam2<-gam(f2,data =  I,family = poisson())
    pam3<-gam(f3,data =  I,family = poisson())
    pam4<-gam(f4,data =  I,family = poisson())

#7 - save results
    saveRDS(pam1, "pam1.rds")
    saveRDS(pam2, "pam2.rds")
    saveRDS(pam3, "pam3.rds")
    saveRDS(pam4, "pam4.rds")
    saveRDS(I, "I.rds")
    