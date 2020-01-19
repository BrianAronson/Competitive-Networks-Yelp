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

#3 - create formulas
    #A) Interval - Mostly random effects
        a.baseform <- formula(Surv(Interval, Dissolved) ~ age + age2 + stars_cur + stars_int + city_super + density.1 + tdensity.3.1 + atyp + Migrants2016 + Income2016 + Unemploy2016 + Vacant2016 + White2016 + popdens2016)
        a.form1 <- update(a.baseform, ~. + localcrowd.0.1 + tlocalcrowd.3.1)
        a.form2 <- update(a.form1, ~. + localcentrality.1 + tlocalcentrality.3)
        a.form3 <- update(a.form2, ~. + localwidth3.1 + tlocalwidth3.1)
    #B) Age - Mostly random effects (probably best model)
        b.baseform <- formula(Surv(age, Dissolved) ~ stars_cur + stars_int + city_super + density.1 + tdensity.3.1 + atyp + Migrants2016 + Income2016 + Unemploy2016 + Vacant2016 + White2016 + popdens2016)
        b.form1 <- update(b.baseform, ~. + localcrowd.0.1 + tlocalcrowd.3.1)
        b.form2 <- update(b.form1, ~. + localcentrality.1 + tlocalcentrality.3)
        b.form3 <- update(b.form2, ~. + localwidth3.1 + tlocalwidth3.1)
    #C) Age with time interactions - Mostly random 
        c.baseform <- formula(Surv(age, Dissolved) ~ stars_cur + stars_int + city_super + ageint2*density.1 + ageint2*tdensity.3.1 + ageint2*atyp + Migrants2016 + Income2016 + Unemploy2016 + Vacant2016 + White2016 + popdens2016)
        c.form1 <- update(c.baseform, ~. + ageint2*localcrowd.0.1 + ageint2*tlocalcrowd.3.1)
        c.form2 <- update(c.form1, ~. + ageint2*localcentrality.1 + ageint2*tlocalcentrality.3)
        c.form3 <- update(c.form2, ~. + ageint2*localwidth3.1 + ageint2*tlocalwidth3.1-ageint2)
    #D) Interval - Just fixed effects
        d.baseform <- formula(Surv(Interval, Dissolved) ~ tstars_cur + tstars_int + tdensity.3.1 + tatyp + tMigrants + tIncome + tUnemploy + tVacant + tWhite + tpopdens)
        d.form1 <- update(d.baseform, ~. + tlocalcrowd.3.1)
        d.form2 <- update(d.form1, ~. + tlocalcentrality.3)
        d.form3 <- update(d.form2, ~. + tlocalwidth3.1)
    #E) Age - Just fixed effects (too small AIC differences)
        e.baseform <- formula(Surv(age, Dissolved) ~ tstars_cur + tstars_int + tdensity.3.1 + tatyp + tMigrants + tIncome + tUnemploy + tVacant + tWhite + tpopdens)
        e.form1 <- update(e.baseform, ~. + tlocalcrowd.3.1)
        e.form2 <- update(e.form1, ~. + tlocalcentrality.3)
        e.form3 <- update(e.form2, ~. + tlocalwidth3.1)
    #F) Interval - all fixed and random effects
        f.baseform <- formula(Surv(Interval, Dissolved) ~ tstars_cur + tstars_int + tdensity.3.1 + tatyp + tMigrants + tIncome + tUnemploy + tVacant + tWhite + tpopdens + age + age2 + stars_cur + stars_int + city_super + density.1 + tdensity.3.1 + atyp + Migrants2016 + Income2016 + Unemploy2016 + Vacant2016 + White2016 + popdens2016)
        f.form1 <- update(f.baseform, ~. + localcrowd.0.1 + tlocalcrowd.3.1)
        f.form2 <- update(f.form1, ~. + localwidth3.1 + tlocalwidth3.1)
        f.form3 <- update(f.form2, ~. + localcentrality.1 + tlocalcentrality.3)
    #G) Age - all fixed and random effects - FINAL
        g.baseform <- formula(Surv(age, Dissolved) ~ tstars_cur + tstars_int + tdensity.3.1 + tatyp + tMigrants + tIncome + tUnemploy + tVacant + tWhite + tpopdens + stars_cur + stars_int + city_super + density.1 + tdensity.3.1 + atyp + Migrants2016 + Income2016 + Unemploy2016 + Vacant2016 + White2016 + popdens2016)
        g.form1 <- update(g.baseform, ~. + localcrowd.0.1 + tlocalcrowd.3.1)
        g.form2 <- update(g.form1, ~. + localwidth3.1 + tlocalwidth3.1)
        g.form3 <- update(g.form2, ~. + localcentrality.1 + tlocalcentrality.3)
    #H) Age with time interactions
        h.baseform <- formula(Surv(age, Dissolved) ~ tstars_cur + tstars_int + tdensity.3.1 + tatyp + tMigrants + tIncome + tUnemploy + tVacant + tWhite + tpopdens + stars_cur + stars_int + city_super + density.1 + tdensity.3.1 + atyp + Migrants2016 + Income2016 + Unemploy2016 + Vacant2016 + White2016 + popdens2016)
        h.baseform <- update(h.baseform, ~. + ageint2*density.1 + ageint2*tdensity.3.1 + ageint2*atyp)
        h.form1 <- update(h.baseform, ~. + ageint2*localcrowd.0.1 + ageint2*tlocalcrowd.3.1)
        h.form2 <- update(h.form1, ~. + ageint2*localwidth3.1 + ageint2*tlocalwidth3.1)
        h.form3 <- update(h.form2, ~. + ageint2*localcentrality.1 + ageint2*tlocalcentrality.3)
        
#4 - run models
    (fit0 <- coxph(g.baseform, data =  Haz[su, ]))
    (fit1 <- coxph(g.form1, data =  Haz[su, ]))
    (fit2 <- coxph(g.form2, data =  Haz[su, ]))
    (fit3 <- coxph(g.form3, data =  Haz[su, ]))

#5 - shoenfield tests
   (shoen <- cox.zph(fit3))
    ggcoxzph(shoen, var=c("tlocalcrowd.3.1", "tlocalwidth3.1", "tlocalcentrality.3"), resid=F)
    ggcoxzph(shoen, var=c("tdensity.3.1"), resid=F)

#6 - Format results
    #convert to df
        mod1 <- as.data.frame(summary(fit1)$coefficients)
    #stars function
        prstars <- function(x){
          ifelse(x<.001, "***", ifelse(x<.01, "**", ifelse(x<.05, "*", "")))
        }
    #remove unecessary info
        mod1 <- as.data.frame(summary(fit1)$coefficient)[, c(1, 3, 4, 5)]
        mod2 <- as.data.frame(summary(fit2)$coefficient)[, c(1, 3, 4, 5)]
        mod3 <- as.data.frame(summary(fit3)$coefficient)[, c(1, 3, 4, 5)]
        df1 <- data.frame(var=row.names(mod1), coef1=round(mod1[, 1], 2), se1=paste("(", round(mod1[, 2], 2), ")", sep=""), pr1=prstars(mod1[, 4]))
        df2 <- data.frame(var=row.names(mod2), coef2=round(mod2[, 1], 2), se2=paste("(", round(mod2[, 2], 2), ")", sep=""), pr2=prstars(mod2[, 4]))
        df3 <- data.frame(var=row.names(mod3), coef3=round(mod3[, 1], 2), se3=paste("(", round(mod3[, 2], 2), ")", sep=""), pr3=prstars(mod3[, 4]))
        df <- merge(df1, df2, by="var", all=T)
        df <- merge(df, df3, by="var", all=T)

#7 - Find other important info
    #Grab AICs
        extractAIC(fit1)[2]
        extractAIC(fit2)[2]
        extractAIC(fit3)[2]
    #Grab sample sizes
        nrow(Haz[!is.na(Haz$age), ])
        length(unique(Haz$ID[!is.na(Haz$age)]))

#8 - Save Key Info
    write.xlsx(df, "C:/Users/bda13/Desktop/Table 1.xlsx")
  