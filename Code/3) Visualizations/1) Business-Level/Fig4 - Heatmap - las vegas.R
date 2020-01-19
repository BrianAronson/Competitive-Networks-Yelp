library(rgdal)    
library(sp)       
library(leaflet)  
library(dplyr)    
library(ggplot2)  
library(rgeos)
library(maptools)
library(gpclib)
library(ggrepel)
gpclibPermit()

#1) Read tract info and convert the GEOID to a character - (Make sure the tract info is downloaded)
  setwd(paste(prep$mainDir,"Census/Las Vegas/cb_2016_32_tract_500k",sep=""))
  tract <- readOGR(dsn=".", layer = "cb_2016_32_tract_500k")
  tract@data$GEOID<-as.character(tract@data$GEOID)

#2) Read tabular census data from FactFinder 
  setwd(paste(prep$mainDir,"Census/Las Vegas/ACS_16_5YR_B03002",sep=""))   
  data <- read.csv("ACS_16_5YR_B03002.csv", stringsAsFactors = FALSE)
  #clean data
      #kill useless variables
          data<-data[,c("GEO.id2", "GEO.display.label","HD01_VD01","HD01_VD03", "HD01_VD04","HD01_VD06","HD01_VD12")]
      #rename variables
          names(data)<-c("id","geography","Total","White","Black","Asian","Latino")
      #Kill first line
          data<-data[-1,]
      #change variable classes
          data$Total<-as.numeric(data$Total)
          data$White<-as.numeric(data$White)/data$Total*100
          data$Black<-as.numeric(data$Black)/data$Total*100
          data$Asian<-as.numeric(data$Asian)/data$Total*100
          data$Latino<-as.numeric(data$Latino)/data$Total*100
          data$Black[is.nan(data$Black)]<-0
          data$Asian[is.nan(data$Asian)]<-0
          data$Latino[is.nan(data$Latino)]<-0
          
#3) Plot census data with ggplot2
    # convert polygons to data.frame
          ggtract<-fortify(tract, region = "GEOID") 
    # join tabular data
          ggtract<-left_join(ggtract, data, by=c("id")) 
    # here we limit to the NYC counties
          ggtract <- ggtract[grep("Clark", ggtract$geography),]
    # exclude bordering counties
          ggtract <- ggtract[!grepl("59.02|75|58.23|58.40|58.27|57.11|78|56.13|61.04|54.35|55.01|55.02|57.03|57.12|58.39", ggtract$geography),]
    # Manually assign colors to areas based on racial composition
          ggtract$Black2<-(ggtract$Black/100)
          ggtract$Asian2<-(ggtract$Asian/100)
          ggtract$Latino2<-(ggtract$Latino/100)
          ggtract$Asian2<-ifelse(ggtract$Asian2<.15,0,ggtract$Asian2)
          ggtract$Latino2<-ifelse(ggtract$Latino2<.2,0,ggtract$Latino2)
          ggtract$plotcolors <- rgb(1,1-ggtract$Latino2,1-ggtract$Asian2)
          ggtract$colors2<-ifelse(ggtract$Asian2>0,"#1bc3e5","#000000")
          ggtract$alpha <- ifelse(ggtract$Asian2>0,ggtract$Asian2*2.5,ifelse(ggtract$Latino2>0,ggtract$Latino2,0))
          ggtract$alpha<-ggtract$alpha^1.5
          ggtract$alpha<-ggtract$alpha*1.2
          ggtract$alpha[ggtract$Latino2>0 & ggtract$Asian2==0]<-ggtract$alpha/3
            summary(ggtract$alpha)
            summary(ggtract$Asian2)
    #plot
          ggplot() +
            theme_void()+
            coord_map(xlim = c(-115.4, -114.9), ylim = c(35.9,36.35))+
            geom_polygon(data = ggtract , aes(x=long, y=lat,group=group), color="grey50",fill=ggtract$colors2,alpha=ggtract$alpha)
    
          
#4) Create category labels to insert into plot
  clusters=15
  {
  #0 - Set Directory
      setwd(Directory)
      set.seed(123)
  #1 - Read Data
      Bus<-readRDS("Restaurants.rds")
      Bus<-Bus[Bus$city_super==City,]
      Atr<-readRDS(paste(City,"_atr.rds",sep=""))
      Atr2<-matrix(Atr,nrow=sqrt(length(Atr)),ncol=sqrt(length(Atr)))
  #2 - Use lat/lon coordinates
      x <- Bus$longitude
      y <- Bus$latitude
      df <- as.data.frame(cbind(x,y))
  #3 Find terms associated with particular clusters
      #1 - Grab term list according to numbers used in gower matrix
          terms<-readRDS(paste(Directory,City, "_gowerinput.rds",sep=""))
      #2 - Make sure dfs match
          terms<-terms[terms$id%in% Bus$id,]
          df<-df[Bus$id%in% terms$id,]
          Bus2<-Bus[Bus$id%in% terms$id,]
      #3 - convert all factors to numeric
          terms$id<-NULL
          names(terms)
          for(i in 1:length(terms))  {
            if(class(terms[,i])=="factor"){
              terms[,i]<-as.numeric(as.character(terms[,i]))
            }
          }
          a<-colMeans(terms)
          terms<-terms[,colMeans(terms)>0.003]
          
      #4 - Cluster data
          cl <- kmeans(df,centers=clusters)$cluster
      #5 - Collapse (sum) rows by their clusters
          terms$cl<-cl
          terms<-data.table(terms)
          terms<-terms[,lapply(.SD, sum, na.rm=TRUE),by=list(cl)]
      #6 - Divide the sums in #4 by the number of words in each cluster. (This is the TF)
          terms<-as.data.frame(terms)
          #put cluster info in separate vector
              tcl<-terms$cl
              terms$cl<-NULL
          for(i in 1:nrow(terms)){
            words<-sum(terms[i,])
            terms[i,]<-terms[i,]/words
          }
      #7 - Determine the "keyness" of each term in each document/cluster based on its rate in that document compared to its rate in the rest of the corpus
          terms2<-terms
          for(i in 1:ncol(terms2)){
            terms2[,i]<-terms[,i]/mean(terms[,i])
          }
          #print results as short list
              results<-list()
              for(i in 1:nrow(terms2))  {
                results[[i]]<-sort(terms2[i,],decreasing = T)[1:15]
              }
          #since words are too unique, weight somewhat more by frequency of word in dataset
              # terms3<-terms2*(terms^(1/2))
              terms3<-terms2*(terms^(1/5))
              #print results as short list
              results<-list()
              for(i in 1:nrow(terms3))  {
                results[[i]]<-sort(terms3[i,],decreasing = T)[1:15]
              }
          #print first result for each in vector
              results2<-vector()
              for(i in 1:nrow(terms3))  {
                results2[i]<-names(sort(terms3[i,],decreasing = T)[1])
              }
      #8 - Determine location of each cluster on graph
          df3<-df
          df3$cl<-cl
          df3<-data.table(df3)
          df3<-df3[,lapply(.SD, mean, na.rm=TRUE),by=list(cl)]
          #insert cluster information from step 6
              tempdf<-data.table(label=results2,cl=tcl)
              tempdf3<-as.data.frame(data.frame(table(cl)))
              names(tempdf3)<-c("cl","size")
              tempdf3$cl<-as.numeric(as.character(tempdf3$cl))
              tempdf<-merge(tempdf,tempdf3)
              df3<-merge(df3,tempdf,by="cl")
     
      #9 - Tweak text data
          results2[results2=="Chicken Shop"]<-"Fried Chicken" #This is what chicken shop refers to
          results2<-gsub('[[:digit:]]+', '', results2)
          results2<-gsub('[.]', '', results2)
          results2<-paste0(toupper(substr(results2, 1, 1)), (substring(results2, 2)))
          results2[results2=="PriceHigh"]<-"Price High"
          results2<-results2[order(tcl)]
          results<-results[order(tcl)]
          df3$label<-results2
          df3$y[df3$y==sort(df3$y)[1]]<-(sort(df3$y)[1]+sort(df3$y)[2])/2
          
      #10 -  standardize text sizes
        range01 <- function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}
        df3$size2<-5*range01(df3$size)+6

#5 - make plot with text labels
    png(file="C:/Users/admin/Desktop/Fig 3 - Common Terms in Las Vegas Neighborhoods.png", height=8.5, width=10,units = "in",res = 72)
      ggplot() +
      theme_void()+
      coord_map(xlim = c(-115.4, -114.9), ylim = c(35.9,36.35))+
      geom_polygon(data = ggtract , aes(x=long, y=lat,group=group), color="grey70",fill=ggtract$colors2,alpha=ggtract$alpha)+
      geom_text_repel(data=df3,aes(x=x,y=y,label=paste(label)),size=df3$size2, segment.colour = NA,color="Black",fontface="bold")+
      ggtitle("Common Terms in Las Vegas Neighborhoods")+
      theme(title = element_text(size=24,face="bold"),
            plot.title = element_text(hjust = 0.5))
      dev.off()
    }

