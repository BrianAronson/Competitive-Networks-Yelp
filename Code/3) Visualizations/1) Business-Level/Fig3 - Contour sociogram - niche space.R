library(ggplot2)
library(MASS)
library(ggrepel)

#0 - Set Directory
    setwd(Directory)
    set.seed(1234)

#1 - Read Data
    City=prep$Cities[6]
    Bus<-readRDS("Restaurants.rds")
    Bus<-Bus[Bus$city_super==City,]
    Dist<-readRDS(paste(City,"_dist.rds",sep=""))
    Atr<-readRDS(paste(City,"_atr.rds",sep=""))
    terms<-readRDS(paste(Directory,City, "_gowerinput.rds",sep=""))
    
#2 - convert attributes to matrix, then cmd scale it
    Atr2<-matrix(Atr,nrow=sqrt(length(Atr)),ncol=sqrt(length(Atr)))
    fit<-cmdscale(Atr2)
    x <- fit[,1]
    y <- fit[,2]
    df<-data.frame(x,y)

#3 - Since peaks on contour map are too high, create densities more manually, transforming the z scores to flatten map
    df2 = kde2d(df$x, df$y, n = 100)
    df2 = data.frame(expand.grid(x=df2$x, y=df2$y), z=as.vector(df2$z))
    df2$z2<-df2$z^(1/3)

#4 - Make sure term and attributes dfs match
    terms<-terms[terms$id%in% Bus$id,]
    df<-df[Bus$id%in% terms$id,]
    Bus2<-Bus[Bus$id%in% terms$id,]
    
#5 - convert all factors to numeric
    terms$id<-NULL
    for(i in 1:length(terms))  {
      if(class(terms[,i])=="factor"){
        terms[,i]<-as.numeric(as.character(terms[,i]))
      }
    }
    a<-colMeans(terms)
    terms<-terms[,colMeans(terms)>0.010]
    
#6 - Cluster data
    cl <- kmeans(df,centers=20,iter.max=100)$cluster
    
#7 - Collapse (sum) rows by their clusters
    terms$cl<-cl
    terms<-data.table(terms)
    terms<-terms[,lapply(.SD, sum, na.rm=TRUE),by=list(cl)]
    
#8 - Divide the sums in #4 by the number of words in each cluster. (This is the TF)
    terms<-as.data.frame(terms)
    #put cluster info in separate vector
        tcl<-terms$cl
        terms$cl<-NULL
    for(i in 1:nrow(terms)){
      words<-sum(terms[i,])
      terms[i,]<-terms[i,]/words
    }
        
#9 - Determine the "keyness" of each term in each document/cluster based on its rate in that document compared to its rate in the rest of the corpus
    terms2 <- terms
    for (i in 1:ncol(terms2)) {
        terms2[, i] <- terms[, i] / (mean(terms[, i]))
    }
    results <- list()
    for (i in 1:nrow(terms2))  {
        results[[i]] <- sort(terms2[i, ], decreasing = T)[1:10]
    }
    results2 <- vector()
    for (i in 1:nrow(terms2))  {
        results2[i] <- names(sort(terms2[i, ], decreasing = T)[1])
    }
    
#10 - since words are too unique, weight somewhat more by frequency of word in dataset
    terms3 <- terms2 * (.05 + terms)
    results <- list()
    for (i in 1:nrow(terms3))  {
        results[[i]] <- sort(terms3[i, ], decreasing = T)[1:10]
    }
    results2 <- vector()
    for (i in 1:nrow(terms3))  {
        results2[i] <- names(sort(terms3[i, ], decreasing = T)[1])
    }
    
#11 - Make a few tiny edits to text data
    results2[8]<-names(results[[8]])[2] #duplicate price high
    results2[7]<-names(results[[7]])[5] #duplicate hawaiian/filipino
    results2[results2=="Chicken Shop"]<-"Fried Chicken" #This is what chicken shop refers to
    results2<-gsub('[[:digit:]]+', '', results2)
    results2<-gsub('[.]', '', results2)
    results2<-paste0(toupper(substr(results2, 1, 1)), (substring(results2, 2)))
    results2[results2=="PriceHigh"]<-"Price High"
    results2[results2=="Full_bar"]<-"Full Bar"
    results2[results2=="Chain_super"]<-"Chain"
        
#12 - Determine location of each cluster on graph
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
    #standardize text sizes
        range01 <- function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}
        df3$size2<-7.5*range01(df3$size)+8
    #shift breakfast down just for visual flare
        temp<-(df3[df3$label=="Breakfast",]$y+df3[df3$label=="Steakhouses",]$y)/2
        df3[df3$label=="Breakfast",]$y<-temp
        
#13 - Graph terms
        png(file="C:/Users/bda13/Desktop/Fig 2 - Contour Sociogram of Niche Space in Las Vegas.png", height=15.25, width=18.5,units = "in",res = 72)
        ggplot() +
          stat_contour(data=df2, geom="polygon", aes(x=x,y=y,z=z2, fill=..level..),bins=15)+
          scale_fill_gradient2(low = "white", mid="#1bc3e5", high ="#159db8",midpoint=7)+
          theme_void()+
          geom_text_repel(data=df3,aes(x=x,y=y,label=paste(label)),size=df3$size2, segment.colour = NA,color="Black",fontface="bold",force=.5) + 
          theme(legend.position="none")+
          ggtitle("Contour Sociogram of Niche Space in Las Vegas")+
          theme(title = element_text(size=34,face="bold"),
                plot.title = element_text(hjust = 0.5))
        dev.off()
