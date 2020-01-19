rm(list = ls())
gc()

#library(gender)

setwd('/misc/utopia3/bda13/lanhome/Data/RData')
uniquebus<-readRDS("BusinessData.rds")
User<-readRDS("UserData.rds")

str(User)
library(gender)
#Intro
  #Show that the quality of reviews is mostly unrelated to survival
    starsbysurvival<-data.frame(survival=c("survived","died"),stars=c(mean(uniquebus[uniquebus$open==1,]$stars),mean(uniquebus[uniquebus$open==0,]$stars)))

#Reviewer information
  #List number of reviewers and information about how many reviews they left
    #limit list to most common names()
        temp<-as.data.frame(table(User$name))
        temp<-temp[temp$Freq>9,]      
      #percent of all names that occurred at least 10 times
        sum(temp$Freq)/length(User[,1])
      #look up gender of most common names  
        temp<-gender(as.character(temp$Var1))
      #Just keep name and proportion male
        temp<-temp[,1:2]
        names(temp)<-c("name","male")
      #merge into Userdata
        User<-merge(User,temp,by="name",all=TRUE)
      #determine how many reviews were by men
        User$malereviewcount<-User$male*User$review_count
        sum(User$malereviewcount,na.rm=TRUE)/sum(User$review_count[is.na(User$malereviewcount)==FALSE])
#Use this info and other to describe users
    Userinfo<-data.frame(MeanReviewCountPerUser=mean(User$review_count),MedianReviewCountPerUser=median(User$review_count),PercentEliteUsers=sum(User$elite!="[]")/length(User[,1]),PercentReviewsByElites=1-(sum(User[User$elite=="[]",]$review_count)/sum(User$review_count)),PercentMaleUsers=sum(User$malereviewcount,na.rm=TRUE)/sum(User$review_count[is.na(User$malereviewcount)==FALSE]))  
        
#City information
  #Describe that supercities are a collection of adjacent cities
    cityinfo<-as.data.frame(table(uniquebus$city,uniquebus$supercity))
    names(cityinfo)<-c("city","supercity","frequency")
  #create dataframe of largest and second largest cities within a supercity
    largestcities<-data.frame(index=1:12,supercity=NA,city=NA,BusinessCount=NA)
    supercities=unique(uniquebus$supercity)
    for (i in 1:length(supercities)){
      #super city
        largestcities[i*2-1,]$supercity<-as.character(supercities[i])
        largestcities[i*2,]$supercity<-as.character(supercities[i])
      #largest city within supercity
        largestcities[i*2-1,]$city<-as.character(cityinfo$city[cityinfo$supercity==as.character(supercities[i]) & cityinfo$frequency==max(cityinfo$frequency[cityinfo$supercity==as.character(supercities[i])])])
      #largest city within supercity business count
        largestcities[i*2-1,]$BusinessCount<-max(cityinfo$frequency[cityinfo$supercity==as.character(supercities[i])])        
      #Second largest city within supercity
        largestcities[i*2,]$city<-as.character(cityinfo$city[cityinfo$supercity==as.character(supercities[i]) & cityinfo$frequency==max(cityinfo[cityinfo$frequency!=max(cityinfo$frequency[cityinfo$supercity==as.character(supercities[i])]),]$frequency[cityinfo$supercity==as.character(supercities[i])])])
        largestcities[i*2,]$BusinessCount<-max(cityinfo[cityinfo$frequency!=max(cityinfo$frequency[cityinfo$supercity==as.character(supercities[i])]),]$frequency[cityinfo$supercity==as.character(supercities[i])])
    }
  #Estimate City Growth to determine which years to examine
    #Create dataframe
      growth<-data.frame(city=unique(uniquebus$supercity), A2010=0, A2011=0, A2012=0, A2013=0, A2014=0, A2015=0)
    #Estimate alive businesses in each city by year
      for(i in 1:length(unique(uniquebus$supercity))){
        growth[i,2]<-sum(uniquebus$Alive2010[uniquebus$supercity==growth[i,1]])
        growth[i,3]<-sum(uniquebus$Alive2011[uniquebus$supercity==growth[i,1]])
        growth[i,4]<-sum(uniquebus$Alive2012[uniquebus$supercity==growth[i,1]])
        growth[i,5]<-sum(uniquebus$Alive2013[uniquebus$supercity==growth[i,1]])
        growth[i,6]<-sum(uniquebus$Alive2014[uniquebus$supercity==growth[i,1]])
        growth[i,7]<-sum(uniquebus$Alive2015[uniquebus$supercity==growth[i,1]])
      }
  #show that this is consistent with user saturation
      usergrowth<-as.data.frame(table(substr(User$yelping_since,1,4)))
      usergrowth<-usergrowth[as.numeric(as.character(usergrowth$Var1))>2009 & as.numeric(as.character(usergrowth$Var1))<2016,]

#Business information      

  #Categories:
    #List supercategories and most common categories within them
        #Determine most common subcategories by supercategory
          temp<-as.data.frame(table(unlist(uniquebus[uniquebus$supercategory=="Food",]$categories)))
          temp<-temp[order(temp$Freq,decreasing=T),]
          names(temp)<-c("SubCategory","Frequency")
          temp$SuperCategory<-"Food"
          CategoryBySuperCategory<-head(temp)[-1,]
          
          temp<-as.data.frame(table(unlist(uniquebus[uniquebus$supercategory=="Nightlife",]$categories)))
          temp<-temp[order(temp$Freq,decreasing=T),]
          names(temp)<-c("SubCategory","Frequency")
          temp$SuperCategory<-"Nightlife"
          CategoryBySuperCategory<-rbind(CategoryBySuperCategory,head(temp)[-1,])
          
          temp<-as.data.frame(table(unlist(uniquebus[uniquebus$supercategory=="Restaurants",]$categories)))
          temp<-temp[order(temp$Freq,decreasing=T),]
          names(temp)<-c("SubCategory","Frequency")
          temp$SuperCategory<-"Restaurants"
          CategoryBySuperCategory<-rbind(CategoryBySuperCategory,head(temp)[-1,])
          
          CategoryBySuperCategory$ReviewCount<-NA
          uniquebus$temp<-NA
          for(i in 1:length(CategoryBySuperCategory[,1])){
            for (h in 1:length(uniquebus[,1])){
              uniquebus$temp[h]<-ifelse(as.character(CategoryBySuperCategory$SubCategory[i]) %in% uniquebus$categories[[h]],1,0)
            }
            CategoryBySuperCategory$ReviewCount[i]<-mean(uniquebus[uniquebus$temp==1,]$reviewcount,na.rm=T)
            print(i)
          }
    
  #These tables require grabbing a version of the business dataset with all supercategories in it and defined.
      #Find most frequently reviewed non restaurants    
        #Determine most frequent categories
          uniquebus2<-uniquebus
          uniquebus<-uniquebus[uniquebus$city=="Charlotte",]
          uniquebus<-uniquebus[uniquebus$supercategory=="Shopping",]
          categorieslist<-as.data.frame(table(unlist(uniquebus$categories[uniquebus$supercategory!="Restaurants" & uniquebus$supercategory!="Nightlife"])))
          categorieslist<-categorieslist[categorieslist$Freq>10,]
          categorieslist<-as.vector(categorieslist$Var1)
          categoriesdf<-matrix(0,ncol=length(categorieslist),nrow=nrow(uniquebus))
          for(i in 1:nrow(categoriesdf)){
            categoriesdf[i,match(uniquebus$categories[[i]],categorieslist)]<-1
            print(i)
          }
          categoriesdf<-as.data.frame(categoriesdf)
          names(categoriesdf)<-categorieslist
          categoriesdf$review_count<-uniquebus$review_count
        #calculate medians
          means<-vector(length=length(categoriesdf)-1)
          counts<-vector(length=length(categoriesdf)-1)
          for(i in 1:(length(categoriesdf)-1)){
            means[i]<-median(categoriesdf$review_count[categoriesdf[,i]==1])
            counts[i]<-sum(categoriesdf[,i])
            print(i)
          }
        #create table of results
          results<-data.frame(Category=categorieslist,review_count=means)
          results[order(results$review_count),]
          
          results2<-data.frame(Category=categorieslist,Freq=counts)
          results2[order(results2$Freq),]
        #Supercategory counts
          temp<-aggregate(review_count ~ supercategory,data=uniquebus,FUN=mean)

          
          
          table(uniquebus$supercategory)
          uniquebus$review_count      
    #Show how categories were clustered
        #Group the following:
          #Create more meaningful business categories - #SuperCategory has too few types and categories2 has too many organizational types that might not even be distinct; try to make meaningful and distinct categories of organizations
          uniquebus$categories2<-unlist(lapply(uniquebus$categories, `[[`, 1))
          for(i in 1:length(uniquebus$categories2)){ #fix vague "food" category & nightlife category
            NumberCategories<-length(uniquebus$categories[[i]])}
          #original algorithm too influenced by "restaurant" category for clustering. Remove "restaurant"
          UniqueCategories<-unique(unlist(uniquebus$categories)) #get list of all categories
          #Remove nonuseful categories
          UniqueCategories<-UniqueCategories[UniqueCategories!="Food" & UniqueCategories!="Nightlife" & UniqueCategories!="Restaurants"]
          temp<-as.data.frame(table(unlist(uniquebus$categories)))
          temp<-temp[temp$Freq>10,]
          UniqueCategories<-UniqueCategories[UniqueCategories %in% as.character(temp$Var1)]
          #empty category matrix
          CategoryMatrix<-matrix(0,ncol=length(UniqueCategories), nrow=length(UniqueCategories)) 
          #fill category matrix by category overlap
          for(i in 1:length(uniquebus$categories)){
            CategoryPosition<-match(uniquebus$categories[[i]],UniqueCategories) #return all row/column numbers of business's categories
            CategoryPosition2<-data.frame(sender=rep(CategoryPosition,length(CategoryPosition)),receiver=rep(CategoryPosition,each=length(CategoryPosition))) #Create list of positions that categories overlap
            for(j in 1:length(CategoryPosition2[,1])){ #Add 1 to category matrix in each position categories overlap
              CategoryMatrix[CategoryPosition2$sender[j],CategoryPosition2$receiver[j]]<-CategoryMatrix[CategoryPosition2$sender[j],CategoryPosition2$receiver[j]]+1}
          }
          #convert to proportions
          for(i in 1:length(CategoryMatrix[,1])){
            CategoryMatrix[i,]<-CategoryMatrix[i,]/CategoryMatrix[i,i]}
          #delete diagonal of Category Matrix
          for(i in 1:length(CategoryMatrix[,1])){
            CategoryMatrix[i,i]<-0
          }
          d <- dist(CategoryMatrix, method = "euclidean") # distance matrix
          fit <- hclust(d, method="ward.D")
          restaurantclusters <- cutree(fit, k=40) # cut tree into 5 clusters
#          restaurantclusters<-kmeans(CategoryMatrix,40) #Most clusters are meaningful, but large ones are not
          #Append typology clusters into restaurant data
          uniquebus$categoryclusterall<-NA
          UniqueCategories2<-data.frame(UniqueCategories,restaurantclusters)
          #UniqueCategories2<-UniqueCategories2[order(UniqueCategories2$restaurantclusters.cluster), ] #order categories by clusters
          for(i in 1:length(uniquebus[,1])){
            tempcluster<-ifelse(length(uniquebus$categories[[i]])>1,match(uniquebus$categories[[i]][uniquebus$categories[[i]]!="Food" & uniquebus$categories[[i]]!="Nightlife"], UniqueCategories2$UniqueCategories),match(uniquebus$categories[[i]],UniqueCategories2$UniqueCategories)) #find clusters, but don't return clusters for braod "food" and "restaurant" categories
            uniquebus$categoryclusterall[[i]]<-list(UniqueCategories2$restaurantclusters[c(tempcluster)]) #create list variable of all clusters
            uniquebus$categoryclusterprimary[i]<-ifelse(is.na(uniquebus$categoryclusterall[[i]]),NA,as.numeric(names(which.max(table(uniquebus$categoryclusterall[[i]])))))} #create numeric variable of most common cluster - biased towards lower values
          #Create info to show
          temp<-as.data.frame(table(uniquebus$categoryclusterprimary))
          temp<-temp[order(temp$Freq,decreasing=T),]
          temp<-head(temp,10)
          CommonCategoryClusters<-UniqueCategories2[UniqueCategories2$restaurantclusters %in% as.numeric(as.character(temp$Var1)),]
          CommonCategoryClusters<-CommonCategoryClusters[order(CommonCategoryClusters$restaurantclusters),]
          CommonCategoryClusters<-data.frame(Group1 = head(CommonCategoryClusters[CommonCategoryClusters$restaurantclusters==11,]$UniqueCategories),Group2 = head(CommonCategoryClusters[CommonCategoryClusters$restaurantclusters==10,]$UniqueCategories),Group3 = head(CommonCategoryClusters[CommonCategoryClusters$restaurantclusters==4,]$UniqueCategories),Group4 = head(CommonCategoryClusters[CommonCategoryClusters$restaurantclusters==9,]$UniqueCategories),Group5 = head(CommonCategoryClusters[CommonCategoryClusters$restaurantclusters==8,]$UniqueCategories))
        #Remove non useful info
          rm(list = c('CategoryPosition2','CategoryMatrix','CategoryPosition','i','j','NumberCategories','restaurantclusters','tempcluster','UniqueCategories','d','fit'))
          
#A BETTER WAY TO MODEL THIS MIGHT BE TO JUST INCORPORATE A CATEGORY OVERLAP SEARCH INTO THE CODE

          
#Model/tie information    
    #Show rate of business closure by half year
      #limit data frame to businesses opened after 2010 and before 2015
        temp<-uniquebus[uniquebus$dateopenedproxy>"2010-01-01" & uniquebus$dateopenedproxy<"2015-01-01",]
        temp$lifespan<-temp$lifespan/182.5
        temp$lifespan2<-ceiling(temp$lifespan)
      #limit data frame to only businesses that I know died (i.e. no reviews in at least half a year)
        temp2<-temp[temp$dateclosedproxy<"2015-07-01" | temp$open==0,]
      #Percent Businesses with certain deaths within first 5 years
        PercentBusinessesThatDied<-length(temp2[,1])/length(temp[,1])
      #Percent Businesses that died by lifespan
        lifespantable<-as.data.frame(table(temp2$lifespan2))
        names(lifespantable)<-c("Years","PercentDeceased")
        lifespantable$Years=as.numeric(as.character(lifespantable$Years))/2
        #add those that died at birth to those died at half a year, and do the same for those who died at 6 years
          lifespantable$PercentDeceased[2]<-lifespantable$PercentDeceased[2]+lifespantable$PercentDeceased[1]
          lifespantable$PercentDeceased[12]<-lifespantable$PercentDeceased[12]+lifespantable$PercentDeceased[13]
          lifespantable<-lifespantable[-13,]
          lifespantable<-lifespantable[-1,]
        #Convert count to percentage
          lifespantable$PercentDeceased<-lifespantable$PercentDeceased/sum(lifespantable$PercentDeceased)

    #Show bias in review count for businesses that survived to explain that it is not a perfect way to measure niche space
      uniquebus$lifespan<-uniquebus$lifespan/182.5
      uniquebus$lifespan2<-ceiling(uniquebus$lifespan)
      temp<-uniquebus[uniquebus$lifespan2<17,]
      ReviewsByTime<-data.frame(YearsOpen=(1:16)/2,reviewcount=NA)
      ReviewsByTime2<-ReviewsByTime
      for(i in 1:16){
        ReviewsByTime$reviewcount[i]<-median(uniquebus[uniquebus$lifespan2==i,]$reviewcount)
        ReviewsByTime2$reviewcount[i]<-mean(uniquebus[uniquebus$lifespan2==i,]$reviewcount)
      }
  table(uniquebus$lifespan2)
    #Show predictors for being tied
      temp<-readRDS("AllTieModel.rds")
      ModelInfo<-data.frame(Charlotte=readRDS("CharlotteTieModel.rds")[,1],LasVegas=readRDS("LasVegasTieModel.rds")
,Pittsburgh=readRDS("PittsburghTieModel.rds"),Madison=readRDS("MadisonTieModel.rds"),Champaign=readRDS("ChampaignTieModel.rds"),Allties=readRDS("AllTieModel.rds")[,1])
    #Read model outputs and add a column for variable name 
      temp1<-as.data.frame(cbind(readRDS("AllTieModel.rds"),Variable=row.names(readRDS("AllTieModel.rds"))))
      temp2<-as.data.frame(cbind(readRDS("CharlotteTieModel.rds"),Variable=row.names(readRDS("CharlotteTieModel.rds"))))
      temp3<-as.data.frame(cbind(readRDS("LasVegasTieModel.rds"),Variable=row.names(readRDS("LasVegasTieModel.rds"))))
      temp4<-as.data.frame(cbind(readRDS("PhoenixTieModel.rds"),Variable=row.names(readRDS("PhoenixTieModel.rds"))))
      temp5<-as.data.frame(cbind(readRDS("PittsburghTieModel.rds"),Variable=row.names(readRDS("PittsburghTieModel.rds"))))
      temp6<-as.data.frame(cbind(readRDS("MadisonTieModel.rds"),Variable=row.names(readRDS("MadisonTieModel.rds"))))
      temp7<-as.data.frame(cbind(readRDS("ChampaignTieModel.rds"),Variable=row.names(readRDS("ChampaignTieModel.rds"))))
    #Reduce Datasets to just estimate info 
      temp1<-data.frame(Variable=temp1$Variable,All=temp1$Estimate)
      temp2<-data.frame(Variable=temp2$Variable,Charlotte=temp2$Estimate)
      temp3<-data.frame(Variable=temp3$Variable,LasVegas=temp3$Estimate)
      temp4<-data.frame(Variable=temp4$Variable,Phoenix=temp4$Estimate)
      temp5<-data.frame(Variable=temp5$Variable,Pittsburgh=temp5$Estimate)
      temp6<-data.frame(Variable=temp6$Variable,Madison=temp6$Estimate)
      temp7<-data.frame(Variable=temp7$Variable,Champaign=temp7$Estimate)
    #merge
      TiePropensityByCity <- Reduce(function(x, y) merge(x, y, all=T, by=c("Variable")), list(temp1,temp2,temp3,temp4,temp5,temp6,temp7))
    #R2 info
      readRDS("R2.rds")
      readRDS("AllTieR2.rds")

  #Predictors of business death
    

  
      
      
      
      
      
  #Delete non-esssential data
      rm(cityinfo,supercities,User,uniquebus,temp,temp1,temp2,temp3,temp4,temp5,temp6,temp7)
      


      
#Figures
    library(ggplot2)
        
    #growth figures
      growth2<-data.frame(city=rep(growth$city,5),year=rep(c(2010,2011,2012,2013,2014),each=6),count=append(append(append(append(growth$A2010,growth$A2011),growth$A2012),growth$A2013),growth$A2014))
        growth2$count2<-0
      for(i in 7:30){
        growth2$count2[i]<-(growth2$count[i]-growth2$count[i-6])/growth2$count[i-6]*100
      }
      pdf(file="Growth Rate by City Over Time.pdf", height=8, width=12)
      p<-ggplot(growth2[growth2$year!=2010,], aes(x=year, y=count2,group=city))
        p+geom_line(size=1,aes(group=city,color=city))+
        ggtitle("Growth Rate by City Over Time") +
        labs(x="Year", y="Percent Growth", group="City")
      dev.off()    
    #Category figures
      CategoryBySuperCategory #just paste into excel
      CommonCategoryClusters #just paste into excel
    #largestcities
      pdf(file="Supercity Composition.pdf", height=8, width=12)
      p<-ggplot(uniquebus[uniquebus$city %in% largestcities$city,], aes(x=city,fill=supercity))
      p+geom_bar()+scale_x_discrete(limits=c(unique(largestcities$city)))+
        ggtitle("Supercity Composition") +
        labs(x="City", y="Business Count", fill="Supercity")
      dev.off()
    #lifespantable
      lifespantable$lifespan<-lifespantable$Years
      lifespantable$count<-lifespantable$PercentDeceased*100
      temp<-uniquebus[uniquebus$dateopenedproxy>"2010-01-01" & uniquebus$dateopenedproxy<"2015-01-01",]
      temp$lifespan<-(temp$lifespan/365)
      temp$lifespan<-ifelse(temp$lifespan==0,1,ifelse(temp$lifespan==12,11,temp$lifespan/2))
      temp<-temp[temp$dateclosedproxy<"2015-07-01" | temp$open==0,]
      temp2<-as.data.frame(table(temp$lifespan))
      names(temp2)<-c("lifespan","count")
      temp<-merge(temp,temp2,by="lifespan")

      pdf(file="Percent Died by Lifespan.pdf", height=8, width=12)
        ggplot(NULL, aes(lifespan,y=count)) +
        geom_jitter(data = temp[temp$lifespan>.4999 &temp$lifespan<5.51,],color="grey70")+
        geom_smooth(data=lifespantable,size=2,color="firebrick")+
          ggtitle("Percent Died by Lifespan (for those that died)") +
          labs(x="Years Alive", y="Percent Died (per half year)")
      dev.off()
    #ReviewsByTime
      temp<-uniquebus
      #temp$lifespan<-(temp$lifespan/365)
      temp$review_count<-ifelse(temp$review_count>100,NA,temp$review_count)
      ReviewsByTime$lifespan<-ReviewsByTime$YearsOpen
      ReviewsByTime$review_count<-ReviewsByTime$reviewcount
      ReviewsByTime2$lifespan<-ReviewsByTime2$YearsOpen
      ReviewsByTime2$review_count<-ReviewsByTime2$reviewcount
      
      pdf(file="Reviews by Lifespan.pdf", height=8, width=12)
      ggplot(NULL, aes(lifespan,y=review_count))+
        geom_jitter(data = temp[temp$lifespan>.4999 & temp$lifespan<8.0001 & !is.na(temp$review_count),],color="grey70",size=.4)+
        geom_line(data=ReviewsByTime,color="black",size=3)+
        geom_line(data=ReviewsByTime2,color="firebrick",size=3)+
        ggtitle("Reviews by Lifespan") +
        labs(x="Years Alive", y="Review Count")
      dev.off()
      
    #User info and growth info
      usergrowth #copy into excel
      Userinfo #copy into excel
      
    #TiePropensityByCity
      #MUST RUN 5.5
    #Survival Model
      RestaurantToGroceryStore
      #MUST RUN 8.1
      
    #facet line graphs
      #1
        p<-ggplot(P99, aes(ParGen, Estimate,group=Parameter)) 
        p+geom_line(size=1) + facet_wrap(~Parameter, scales="free") + geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit),alpha=0.3)
      #2      
        p+geom_line(size=1) + facet_wrap(~ParQuantile, scales="free",nrow=3) + geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit),alpha=0.3) + scale_x_discrete(limits=c("89-95","98-04","07-13"))
    #bar graph
      p<-ggplot(data, aes(x=(Quantile), y=HeadWorkHours))
      p+geom_bar(stat = "identity")
      
      
    #loess curve with scatter plot
      p<-ggplot(Cscf1, aes(x=(jitter(year)), y=RHoursPerWeek, color=NWPercentile))
      print(  p + geom_point(aes(group=NWPercentile, color=(NWPercentile=="99"))) + geom_smooth(aes(group=NWPercentile, color=(NWPercentile=="99"))) + scale_color_manual(values=c("grey70", "firebrick"), labels=c("Other Percentiles", "99th Percentile")))
      
    #histogram  
      geom_histogram()+theme_minimal()
    #boxplot
      geom_boxplot()
    
    #Good figure for regression models
      p <- ggplot(out.tidy, aes(y=estimate,x=reorder(term, estimate)))
      p + geom_pointrange(aes(ymin=conf.low,
          ymax=conf.high)) +
          geom_hline() +
          coord_flip()
      
    #Other features    
      #to save it, put first line before, second line after      
        pdf(file="Loess and Scatter.pdf", height=10, width=15)
        dev.off()
      #add titles
        ggtitle("Retirement Rate of 1% under age 60")
      #add labels
        labs(x="GDP per Capita", y="Life Expectancy", color="Continent") +
      #set order of variables
        y=reorder()
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      




########### ARCHIVE ################
Business<-readRDS("BusinessData.rds")
Checkin<-readRDS("CheckinData.rds")
Tip<-readRDS("TipData.rds")
User<-readRDS("UserData.rds")
Review<-readRDS("ReviewData.rds")
uniquebus<-readRDS("Uniquebus.rds")
Charlotteuniquebus<-readRDS("Charlotteuniquebus.rds")
revieweroverlap<-readRDS("Revieweroverlap.rds")


table(uniquebus$supercategory)

#descriptives of all variables in datasets
  str(Business)
  str(Checkin) #useless
  str(Tip) #useless
  str(User) 
  str(Review) 
  str(uniquebus)
#Descriptives of Data
  #Business info
    #skewed distribution of reviews at the upper end. Dataset only contains places with at least 1 review
      summary(uniquebus$categories)
    #15 cities with over 1000 businesses
      head( sort(table(uniquebus$city),decreasing = TRUE),30)
    #Categories - Only a 3rd are restaurants. There are a ton of other categories
      a<-head(sort(table(unlist(uniquebus$categories)),decreasing = TRUE),15)
    #Which categories are most reviewed - Pretty even distribution across categories
      for (i in 1:15){
      print(c(names(a[i]),summary(uniquebus[unlist(uniquebus$categories)==names(a[i]),]$review_count)))
        }
  #Create a table with  all categories
      a<-as.data.frame(sort(table(unlist(uniquebus$categories)),decreasing = TRUE))
      d <- a
      names <- rownames(d)
      rownames(d) <- NULL
      data <- cbind(names,d)
      colnames(data)<-c("Category","Reviews")
    #keep only core categories
      data1<-data[data$Category=="Restaurants", ]
      write.csv(data,file="t1.csv")
      head(length(User$elite[1]))
      
      User$elite[1]=="[]"
      User$elite[4]=="[]"
      
  #User info  
    #skewed distribution of user reviews. People with 2 or less reviews are probably useless. People with more than 100 reviews might be unreliable too.
      summary(User$review_count)
    #Percent of reviewers who are elite users
      sum(User$elite!="[]")/length(User[,1])
    #Percent of total reviews by elite users
      1-(sum(User[User$elite=="[]",]$review_count)/sum(User$review_count))
    #skewed distribution of users using yelp (i.e. market or preferences may have changed)
      table(substr(User$yelping_since,1,4))
    #Mostly men seem to use yelp
      head( sort(table(User$name),decreasing = TRUE),15)
    #Not sure this matters but here is review distribution
      summary(User$average_stars)

      
      
#limit uniquebus to charlotte to get a sense of business survival
#      uniquebus<-uniquebus[uniquebus$city=="Charlotte",]
#Estimate number of businesses alive in a given year
      for(i in 27:31){
        print(names(uniquebus[i]))
        print(table(uniquebus[i])) #about 2010 is as early before the inflation seems unrealistic
      }
      i=1
#Estimate City Growth
  #Create dataframe
      growth<-data.frame(city=unique(uniquebus$supercity), A2010=0, A2011=0, A2012=0, A2013=0, A2014=0, A2015=0)
  #Estimate alive businesses in each city by year
      for(i in 1:length(unique(uniquebus$supercity))){
      growth[i,2]<-sum(uniquebus$Alive2010[uniquebus$supercity==growth[i,1]])
      growth[i,3]<-sum(uniquebus$Alive2011[uniquebus$supercity==growth[i,1]])
      growth[i,4]<-sum(uniquebus$Alive2012[uniquebus$supercity==growth[i,1]])
      growth[i,5]<-sum(uniquebus$Alive2013[uniquebus$supercity==growth[i,1]])
      growth[i,6]<-sum(uniquebus$Alive2014[uniquebus$supercity==growth[i,1]])
      growth[i,7]<-sum(uniquebus$Alive2015[uniquebus$supercity==growth[i,1]])
          }

      
sum(uniquebus$Alive2011)
  #Explore businesses that are closed vs opened
      mean(uniquebus[uniquebus$open==1,]$stars)
      mean(uniquebus[uniquebus$open==0,]$stars)
      #study lifespan of closed businesses vs open businesses
      summary(uniquebus[uniquebus$open==0,]$dateclosedproxy)      
      summary(uniquebus[uniquebus$open==1,]$dateclosedproxy)  #vast majority of open places have had recent reviews
      summary(uniquebus[uniquebus$open==0,]$dateopenedproxy)      
      summary(uniquebus[uniquebus$open==1,]$dateopenedproxy)  #places that are closed were opened earlier; i.e. the longer something is open, the greater their chance of ever closing.
      summary(uniquebus[uniquebus$open==0,]$lifespan)
      summary(uniquebus[uniquebus$open==1,]$lifespan)

    #What about restaurants?
      summary(uniquebus[uniquebus$open==0 & uniquebus$supercategory=="Restaurant",]$dateclosedproxy)
      summary(uniquebus[uniquebus$open==1 & uniquebus$supercategory=="Restaurant",]$dateclosedproxy)
      summary(uniquebus[uniquebus$open==0 & uniquebus$supercategory=="Restaurant",]$dateopenedproxy)
      summary(uniquebus[uniquebus$open==1 & uniquebus$supercategory=="Restaurant",]$dateopenedproxy)
      summary(uniquebus[uniquebus$open==0 & uniquebus$supercategory=="Restaurant",]$lifespan)
      summary(uniquebus[uniquebus$open==1 & uniquebus$supercategory=="Restaurant",]$lifespan) #Restaurants are pretty similar to rest of sample. seem to have a solid half year more data
      
      
      
      #Estimate number of restaurants alive in a given year
      #for(i in 27:31){
      #  print(names(uniquebus[uniquebus$supercategory=="Restaurant",][i]))
      #  print(table(uniquebus[uniquebus$supercategory=="Restaurant",][i])) #about 2010 is as early before the inflation seems unrealistic
      #}
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      #Check descriptive stats and look for error
      
      
      #try running in parallel
      # start.time <- Sys.time()
      # x<-foreach(j=1:4222, .combine=rbind) %dopar%{
      #       b<-unlist(strsplit(Pinevilleuniquebus[j,17],", "))
      #       c<-as.numeric(length(intersect(a,b)))
      #       d<-data.frame(sender=Pinevilleuniquebus[i,1], receiver=Pinevilleuniquebus[j,1], overlap=c)
      #             }
      # end.time <- Sys.time()
      # time.taken <- end.time - start.time
      # time.taken
      #Super slow
      
      
      
      
      
      #library(doSNOW)
      #library(foreach)
      #set up parallel computing for speed
      #registerDoSNOW(makeCluster(4, type = "SOCK"))
      
      
  DissolvedStats<-as.data.frame(table(Hazardbus$Interval,Hazardbus$Dissolved2)[,2])
  names(DissolvedStats)<-"Count"
  DissolvedStats$Date<-as.numeric(row.names(DissolvedStats))
  row.names(DissolvedStats)<-NULL
  
  
  
#Death by date
    p<-ggplot(DissolvedStats, aes(x=Date,y=Count))
    p+geom_bar(stat = "identity")+
    scale_x_discrete(limits=c(DissolvedStats$Date))+
    ggtitle("Death by Date")+
    theme(plot.title = element_text(face="bold", size=32), #Title size
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=18),
    axis.title.x= element_text(size=20),
    axis.title.y= element_text(size=20))
    ggsave(file="Death by Date.png")

#Survival curves
    mround <- function(x,base){ 
      base*round(x/base) 
    } 
    
    Hazardbus$Age2<-mround(Hazardbus$Age,.5)
    Curves<-as.data.frame(table(Hazardbus$Age2[Hazardbus$Dissolved2==1]))
    names(Curves)<-c("Age","Death_Count")
    temp<-as.data.frame(table(Hazardbus$Age2[Hazardbus$Dissolved2==0]))
    Curves$FinalAge<-temp$Freq
    Curves$SampleSize<-nrow(Hazardbus)-cumsum(Curves$FinalAge)
    Curves$PercentDeath<-round(Curves$Death_Count/Curves$SampleSize,3)
    Curves$CumulativeSurvival<-1-cumsum(Curves$PercentDeath)
          
    Hazardbus2<-Hazardbus[Hazardbus$DateFounded<2013,] # just study death trends of those born prior to 2013
    Curves<-as.data.frame(table(Hazardbus2$Age2[Hazardbus2$Dissolved2==1]))
    names(Curves)<-c("Age","Death_Count")
    Curves$SampleSize<-nrow(Hazardbus2)-cumsum(Curves$Death_Count)
    Curves$PercentSurvival<-Curves$SampleSize/nrow(Hazardbus2)

    
    Hazardbus2$propniche<-Hazardbus2$NDCPDFSE/Hazardbus2$NDAPDF
    Hazardbus2$propniche<-ifelse(is.na(Hazardbus2$propniche),0,Hazardbus2$propniche)
    Hazardbus2$propniche2<-ifelse(Hazardbus2$propniche<=median(Hazardbus2$propniche),0,1)
    
    table(Hazardbus2$Dissolved2,Hazardbus2$propniche2)
    Hazardbus3<-Hazardbus2[Hazardbus2$propniche2==1,]
    Hazardbus4<-Hazardbus2[Hazardbus2$propniche2==0,]
    
    Curves2<-as.data.frame(table(Hazardbus3$Age2[Hazardbus3$Dissolved2==1]))
    names(Curves2)<-c("Age","Death_Count")
    Curves2$SampleSize<-nrow(Hazardbus3)-cumsum(Curves2$Death_Count)
    Curves2$PercentSurvival<-Curves2$SampleSize/nrow(Hazardbus3)
    
    Curves3<-as.data.frame(table(Hazardbus4$Age2[Hazardbus4$Dissolved2==1]))
    names(Curves3)<-c("Age","Death_Count")
    Curves3$SampleSize<-nrow(Hazardbus4)-cumsum(Curves3$Death_Count)
    Curves3$PercentSurvival<-Curves3$SampleSize/nrow(Hazardbus4)
    
    Curves2<-Curves2[as.numeric(as.character(Curves2$Age))>0 & as.numeric(as.character(Curves2$Age))<4,]
    Curves3<-Curves3[as.numeric(as.character(Curves3$Age))>0 & as.numeric(as.character(Curves3$Age))<4,]
    Curves2[,c("Death_Count","SampleSize")]<-NULL
    Curves3[,c("Death_Count","SampleSize")]<-NULL
    Curves2$Group="Dense"
    Curves3$Group="Sparse"
    DF<-rbind(Curves2,Curves3)
    names(DF)[3]<-"Local Niche Overlap"
    
    p<-ggplot(DF, aes(x=Age, y=PercentSurvival,group=`Local Niche Overlap`))
    p+geom_line(size=3,aes(group=`Local Niche Overlap`,color=`Local Niche Overlap`))+
      ggtitle("Survival Curve For Restaurants by Local Niche Density") +
      labs(x="Age", y="Percent Survival")+
      theme(plot.title = element_text(face="bold", size=22), #Title size
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=18),
            axis.title.x= element_text(size=20),
            axis.title.y= element_text(size=20))
    
    ggsave(file="Survival Curves.png")
    
    
#Survival curves by betweenness
    
    Hazardbus2$propniche2<-ifelse(Hazardbus2$nicheBetweenness>mean(Hazardbus2$nicheBetweenness,na.rm=T),1,0)
    Hazardbus2$propniche2<-ifelse(is.na(Hazardbus2$propniche2),0,Hazardbus2$propniche2)
    
    table(Hazardbus2$Dissolved2,Hazardbus2$propniche2)
    Hazardbus3<-Hazardbus2[Hazardbus2$propniche2==1,]
    Hazardbus4<-Hazardbus2[Hazardbus2$propniche2==0,]
    
    Curves2<-as.data.frame(table(Hazardbus3$Age2[Hazardbus3$Dissolved2==1]))
    names(Curves2)<-c("Age","Death_Count")
    Curves2$SampleSize<-nrow(Hazardbus3)-cumsum(Curves2$Death_Count)
    Curves2$PercentSurvival<-Curves2$SampleSize/nrow(Hazardbus3)
    
    Curves3<-as.data.frame(table(Hazardbus4$Age2[Hazardbus4$Dissolved2==1]))
    names(Curves3)<-c("Age","Death_Count")
    Curves3$SampleSize<-nrow(Hazardbus4)-cumsum(Curves3$Death_Count)
    Curves3$PercentSurvival<-Curves3$SampleSize/nrow(Hazardbus4)
    
    Curves2<-Curves2[as.numeric(as.character(Curves2$Age))>0 & as.numeric(as.character(Curves2$Age))<4,]
    Curves3<-Curves3[as.numeric(as.character(Curves3$Age))>0 & as.numeric(as.character(Curves3$Age))<4,]
    Curves2[,c("Death_Count","SampleSize")]<-NULL
    Curves3[,c("Death_Count","SampleSize")]<-NULL
    Curves2$Group="High"
    Curves3$Group="Low"
    DF<-rbind(Curves2,Curves3)
    names(DF)[3]<-"Local Betweenness"
    
    p<-ggplot(DF, aes(x=Age, y=PercentSurvival,group=`Local Betweenness`))
    p+geom_line(size=3,aes(group=`Local Betweenness`,color=`Local Betweenness`))+
      ggtitle("Survival Curve For Restaurants by Local Niche Betweenness") +
      labs(x="Age", y="Percent Survival")+
      theme(plot.title = element_text(face="bold", size=22), #Title size
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=18),
            axis.title.x= element_text(size=20),
            axis.title.y= element_text(size=20))
    
    ggsave(file="Survival Curves (betweenness).png")
    
    
    
    