#Table 3:
    #0 - Set Directory
        setwd(Directory)
    
    #1 - Read Data
        Haz <- readRDS("HazInfo2.RDS")
        su <- readRDS("suHazInfo2.RDS")
    
    #2 - Prepare subset conditions for models
        dups <- duplicated(Haz$ID)
        temp <- Haz[!dups, ]
        ids <- temp$ID[temp$chain==0 &temp$date_open>=2011]
        ids <- as.character(ids)
        su <- Haz$ID %in% ids
    
    #3 - libraries
        library(pammtools)
        library(survival)
        library(ggplot2)
        theme_set(theme_bw())
        library(mgcv)
    
    #4- remove negative changes
        Haz$tlocalcentrality.3[Haz$tlocalcentrality.3<0] <- 0
        Haz$tlocalwidth3.1[Haz$tlocalwidth3.1<0] <- 0
        Haz$tlocalcrowd.3.1[Haz$tlocalcrowd.3.1<0] <- 0
    
    #5 - subset data
        I <- Haz[su, ]
        I <- I[, c("ID", "Dissolved", "Interval", "age", "age2", "tstars_cur", "tstars_int", "tdensity.3.1", "tatyp", "tMigrants", "tIncome", "tUnemploy", "tVacant", "tWhite", "tpopdens", "stars_cur", "stars_int", "city_super", "density.1", "atyp", "Migrants2016", "Income2016", "Unemploy2016", "Vacant2016", "White2016", "popdens2016", "localcrowd.0.1", "tlocalcrowd.3.1", "localwidth3.1", "tlocalwidth3.1", "localcentrality.1", "tlocalcentrality.3", "rev_count", "date_close", "date_open")]
        I  <-  I[rowSums(is.na(I))== 0, ]
    
    #6 - add other variables
        I$popularity <- I$rev_count*I$age
        I$popularity2 <- I$popularity^2
        I$rev_count2 <- I$rev_count^2
        #age factors
            I$agef <- factor(ifelse(I$age<.5, "0-.5", 
            ifelse(I$age<2.5, ".5-2.5", 
            ifelse(I$age<4.5, "2.5-4.5", "4.5+"))))
            I$agef  <-  relevel(I$agef, ref="0-.5")
        #interval factors
            I$Intervalf <- ifelse(I$Interval<2013, "2011-2013", 
            ifelse(I$Interval<2015, "2013-2015", "2015-2017"))
        #squared versions of things
            I$density.12 <- I$density.1^2
            I$localcrowd.0.12 <- I$localcrowd.0.1^2
            I$localcentrality.12 <- I$localcentrality.1^2
            I$localwidth3.12 <- I$localwidth3.1^2
            I$Dissolved2 <- c(I$Dissolved[-1], F)==1 & c(I$ID[-1], F)==c(I$ID)
            I$Dissolved2 <- I$Dissolved2*1
            I$sub2 <- I$Dissolved!=1
            I$sub3 <- I$city_super=="Phoenix" | I$city_super=="Las Vegas"
            I$sub4 <- I$Interval<=2016
            I$duration <- .5

    #7 - add models
        f1 <- formula(Dissolved ~ Intervalf +agef + scale(tstars_cur) +  scale(tdensity.3.1) + 
                      scale(tatyp) + scale(tMigrants) + scale(tIncome) + scale(tUnemploy) + scale(tVacant) + scale(tWhite) + 
                      scale(tpopdens) + scale(stars_cur) +  + city_super + scale(density.1) + 
                      scale(atyp) + scale(Migrants2016) + scale(Income2016) + scale(Unemploy2016) + scale(Vacant2016) + 
                      scale(White2016) + scale(popdens2016) + scale(localcrowd.0.1) + scale(tlocalcrowd.3.1)  + offset(log(duration)))
        
        f2 <- formula(Dissolved ~ Intervalf +agef + scale(tstars_cur) +  scale(tdensity.3.1) + 
                      scale(tatyp) + scale(tMigrants) + scale(tIncome) + scale(tUnemploy) + scale(tVacant) + scale(tWhite) + 
                      scale(tpopdens) + scale(stars_cur) +  + city_super + scale(density.1) + 
                      scale(atyp) + scale(Migrants2016) + scale(Income2016) + scale(Unemploy2016) + scale(Vacant2016) + 
                      scale(White2016) + scale(popdens2016) + scale(localcrowd.0.1) + scale(tlocalcrowd.3.1) + 
                      scale(localcentrality.1) + scale(tlocalcentrality.3) + offset(log(duration)))
        
        f3 <- formula(Dissolved ~ Intervalf +agef + scale(tstars_cur) +  scale(tdensity.3.1) + 
                      scale(tatyp) + scale(tMigrants) + scale(tIncome) + scale(tUnemploy) + scale(tVacant) + scale(tWhite) + 
                      scale(tpopdens) + scale(stars_cur) +  +city_super  + scale(density.1) + 
                      scale(atyp) + scale(Migrants2016) + scale(Income2016) + scale(Unemploy2016) + scale(Vacant2016) + 
                      scale(White2016) + scale(popdens2016) + scale(localcrowd.0.1) + scale(tlocalcrowd.3.1) + 
                      scale(localcentrality.1) + scale(tlocalcentrality.3) + 
                      scale(localwidth3.1) + scale(tlocalwidth3.1) + offset(log(duration)))
    
        f4 <- formula(Dissolved ~ s(Interval) +age+age2 + tstars_cur +  + tdensity.3.1 + 
                  tatyp + tMigrants + tIncome + tUnemploy + tVacant + tWhite + 
                  tpopdens + stars_cur +  + city_super + density.1 + 
                  atyp + Migrants2016 + Income2016 + Unemploy2016 + Vacant2016 + 
                  White2016 + popdens2016 + localcrowd.0.1 + tlocalcrowd.3.1 + 
                  localcentrality.1 + tlocalcentrality.3 + localwidth3.1 + tlocalwidth3.1+rev_count+rev_count2+popularity+popularity2)
 
    #8 - run models
        pam1 <- gam(f1, data =  I, family = poisson())
        pam2 <- gam(f2, data =  I, family = poisson())
        pam3 <- gam(f3, data =  I, family = poisson())
      
        summary(pam1)
        summary(pam2)
        summary(pam3)
      
    #9 - pull AIC
        AIC(pam1)
        AIC(pam2)
        AIC(pam3)
      
    #10 - format and save results
        #a) prep functions
            prstars <- function(x){
              ifelse(x<.001, "***", ifelse(x<.01, "**", ifelse(x<.05, "*", "")))
            }
            modfun <- function(model){
              b <- row.names(model)
              model <- data.table(model)
              model$var <- b
              # model <- model[match(c("density.1", "tdensity.3.1", "localcrowd.0.1", "tlocalcrowd.3.1", "localcentrality.1", "tlocalcentrality.3", "localwidth3.1", "tlocalwidth3.1", "age", "age2", "stars_cur", "tstars_cur", "atyp", "tatyp", "Income2016", "tIncome", "popdens2016", "tpopdens", "Vacant2016", "tVacant", "White2016", "tWhite", "Migrants2016", "tMigrants", "Unemploy2016", "tUnemploy", "city_superCharlotte", "city_superCleveland", "city_superLas Vegas", "city_superMadison", "city_superPhoenix", "city_superPittsburgh", "(Intercept)"), model$var), ]
              model <- model[match(c("scale(tdensity.3.1)", "scale(tlocalcrowd.3.1)", "scale(tlocalcentrality.3)", "scale(tlocalwidth3.1)", "", "scale(tatyp)", "scale(tstars_cur)", "", "scale(tIncome)", "scale(tpopdens)", "scale(tVacant)", "scale(tWhite)", "scale(tMigrants)", "scale(tUnemploy)", "", "scale(density.1)", "scale(localcrowd.0.1)", "scale(localcentrality.1)", "scale(localwidth3.1)", "", "agef.5-2.5", "agef2.5-4.5", "agef4.5+", "", "Intervalf2013-2015", "Intervalf2015-2017", "", "scale(atyp)", "scale(stars_cur)", "", "scale(Income2016)", "scale(popdens2016)", "scale(Vacant2016)", "scale(White2016)", "scale(Migrants2016)", "scale(Unemploy2016)", "", "city_superCharlotte", "city_superCleveland", "city_superLas Vegas", "city_superMadison", "city_superPhoenix", "city_superPittsburgh", "(Intercept)"), model$var), ]
              model <- model[, -3]
              model[, 3] <- prstars(model[, 3])
              model[, 1] <- sapply(model[, 1], function(x) sprintf("%.2f", x))
              model[, 2] <- sapply(model[, 2], function(x) paste("(", sprintf("%.2f", x), ")", sep=""))
              return(model[, 1:3])
            }
        #b - run and format models
            m1 <- round(summary(pam1)$p.table, 2)
            m2 <- round(summary(pam2)$p.table, 2)
            m3 <- round(summary(pam3)$p.table, 2)
            rnames <- c("scale(tdensity.3.1)", "scale(tlocalcrowd.3.1)", "scale(tlocalcentrality.3)", "scale(tlocalwidth3.1)", "", "scale(tatyp)", "scale(tstars_cur)", "", "scale(tIncome)", "scale(tpopdens)", "scale(tVacant)", "scale(tWhite)", "scale(tMigrants)", "scale(tUnemploy)", "", "scale(density.1)", "scale(localcrowd.0.1)", "scale(localcentrality.1)", "scale(localwidth3.1)", "", "agef.5-2.5", "agef2.5-4.5", "agef4.5+", "", "Intervalf2013-2015", "Intervalf2015-2017", "", "scale(atyp)", "scale(stars_cur)", "", "scale(Income2016)", "scale(popdens2016)", "scale(Vacant2016)", "scale(White2016)", "scale(Migrants2016)", "scale(Unemploy2016)", "", "city_superCharlotte", "city_superCleveland", "city_superLas Vegas", "city_superMadison", "city_superPhoenix", "city_superPittsburgh", "(Intercept)")
            a <- data.table(cbind(rnames, modfun(m1), modfun(m2), modfun(m3)))
            a[is.na(a) | a=="NA" | a=="(NA)"] <- ""
            
        #c - save models
            write.csv(sapply(a, function(x)paste0('="', x, '"')), "C:/Users/bda13/Desktop/pap1models.csv", row.names = F, quote = F)
    
    
#Table 2 - Correlations
        varnames <- c("tdensity.3.1", "tlocalcrowd.3.1", "tlocalcentrality.3", "tlocalwidth3.1", "tatyp", "tstars_cur", "tIncome", "tpopdens", "tVacant", "tWhite", "tMigrants", "tUnemploy", "density.1", "localcrowd.0.1", "localcentrality.1", "localwidth3.1", "age", "Interval")
        ch2 <- as.data.frame(I)
        ch2 <- ch2[, varnames]

    #1 - for the sake of simplicity, median impute nas
        for(i in 1:ncol(ch2)){
          ch2[, i][is.na(ch2[, i])] <- median(ch2[, i], na.rm=T)
        }

    #2 - Correlation tables
        #create correlation matrix
            cormat <- cor(ch2, use="complete")
            temp <- c(expression(Delta~Density), expression(Delta~Crowding), expression(Delta~Centrality), expression(Delta~Compression), 
                    expression(Delta~Atypicality), expression(Delta~Stars), expression(Delta~Income), expression(Delta~Pop.Density), expression(Delta~Vacancy), expression(Delta~White), expression(Delta~Migrants), expression(Delta~Unemployed), "Density", "Crowding", "Compression", "Centrality", "Age", "Interval")
        #reformat for graphing
            cormat <- round(cormat, 2)
            get_upper_tri  <-  function(cormat){
              cormat[lower.tri(cormat)] <-  NA
              return(cormat)
            }
            upper_tri  <-  get_upper_tri(cormat)
            library(reshape2)
            melted_cormat  <-  melt(upper_tri, na.rm = TRUE)
            melted_cormat$val2 <- melted_cormat$value
            melted_cormat$val2 <- sprintf("%.2f", melted_cormat$val2)
            melted_cormat$val2[melted_cormat$value==1] <- ""
            melted_cormat[melted_cormat$Var1==melted_cormat$Var2, ]$value <- 0
            melted_cormat$val2 <- ifelse(sign(as.numeric(melted_cormat$val2))==-1, paste("(", (str_sub(melted_cormat$val2, 2, 5)), ")", sep=""), melted_cormat$val2)

         #create basic heatmap
            ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
              geom_tile(color = "white")+
              scale_fill_gradient2(low = "#1bc3e5", high = "#e53d1b", 
                                   midpoint = 0, limit = c(-1, 1), space = "Lab", 
                                   name="Pearson\nCorrelation") +
              theme_minimal()+
              theme(text=element_text(family="serif"), 
                    axis.text.x = element_text(angle = 45, vjust = 1, size = 20, hjust = 1), 
                    axis.text.y = element_text(size = 20))+
              coord_fixed()

         ##Graph heatmap
            png("C:/Users/bda13/Desktop/p1.Fig 2 - Correlations2.png", height=12, width=16, units = "in", res = 72)
            ggheatmap +
              geom_text(aes(Var2, Var1, label = val2), color = "black", size = 5.3, hjust=.5) +
              theme(
                text=element_text(family="serif"), 
                axis.title.x = element_blank(), 
                axis.title.y = element_blank(), 
                panel.grid.major = element_blank(), 
                panel.border = element_blank(), 
                panel.background = element_blank(), 
                axis.ticks = element_blank(), 
                legend.text = element_text(size = 18), 
                legend.title = element_text(size = 20), 
                plot.title = element_text(size=28, hjust=.38), 
                plot.margin = margin(.5, 4, 0, 0, "cm"))+
              guides(fill = guide_colorbar(barwidth = 4, barheight = 15))+
              scale_x_discrete(labels= temp)+
              scale_y_discrete(labels= temp)
            dev.off()
            
            
#Table 1 - descriptives            
    #1 - prepare data
        ch3 <- as.data.frame(ch2)
        varnames <- c("ID", "tdensity.3.1", "tlocalcrowd.3.1", "tlocalcentrality.3", "tlocalwidth3.1", "tatyp", "tstars_cur", "tIncome", "tpopdens", "tVacant", "tWhite", "tMigrants", "tUnemploy", "density.1", "localcrowd.0.1", "localcentrality.1", "localwidth3.1", "age", "Interval")
        ch3 <- as.data.frame(I)
        ch3 <- ch3[, varnames]
        ch3 <- ch3[order(ch3$Interval, decreasing = T), ]
        ch3 <- ch3[!duplicated(ch3$ID), ]
        ch3$ID <- NULL
        
    #2 - Generate table
        descr <- data.frame(Variable=names(ch3), 
                          Mean=0, 
                          SD=0, 
                          Min=0, 
                          Max=0)
        for(i in 1:ncol(ch3)){
          descr$Mean[i]=mean(ch3[, i], na.rm = T)
          descr$SD[i]=sd(ch3[, i], na.rm = T)
          descr$Min[i]=min(ch3[, i], na.rm = T)
          descr$Max[i]=max(ch3[, i], na.rm = T)
        }
    #3 - save
        write.csv(descr, "C:/Users/bda13/Desktop/desc.csv", row.names = F, quote = F)
                