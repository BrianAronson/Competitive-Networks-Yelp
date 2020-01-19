options(scipen=99)
options(digits=10)
library(ggrepel)
library(jsonlite)
library(RJSONIO)
library(data.table)
library(foreach)
library(parallel)
library(doParallel)
library(Imap)
library(mice)
library(geosphere)
library(cluster)
library(igraph)
library(Yelp)
library(XML)
library(RCurl)
library(RJSONIO)
library(acs)
library(ggplot2)
library(tigris)
library(sp)
library(raster)
library(survival)
library(survminer)
library(xlsx)
library(directlabels)
library(scales)
library(DirectedClustering)
library(dplyr)

#Prep variables
    prep<-list(laptop=1)
    prep$new<-0
    if(prep$new==1){
      prep$mainDir<-ifelse(prep$laptop==1,"C:/Users/bda13/Desktop/Sociology/Statistics/Yelp - 2018/","C:/Users/admin/Desktop/Sociology/Dissertation Stats/Yelp - 2018/")
    }else({
      prep$mainDir<-ifelse(prep$laptop==1,"C:/Users/bda13/Desktop/Sociology/Statistics/Yelp/","C:/Users/admin/Desktop/Sociology/Dissertation Stats/Data and Code/")
    })
    prep$baseDir=paste(prep$mainDir,"1 - Base Data/",sep="")
    prep$derDir<-paste(prep$mainDir,"2 - Derived Data/",sep="")
    prep$Directory<-prep$derDir
    prep$BaseDirectory=prep$baseDir
    prep$City="Charlotte"
    prep$DistMax=4
    prep$DistCuts=2
    prep$AtrType="Normal"
    prep$Competitors="All"
    prep$AtrMax=.1
    prep$AtrCuts=2
    prep$CustAtrCuts<-c(.025,.1)
    prep$CustAtrCuts<-NULL
    prep$Intervals=.5
    prep$Customers="No"
    prep$Parallel = F
    prep$atrranks=F
    prep$CustDistCuts<-c(.5,2,5,10)

# Master - Data preparation
    #1 - Convert JSON files to R (assumes a few initial steps
        yelp_JSON_to_R(Directory=(paste(prep$mainDir,"0 - Initial Steps/yelp_dataset_challenge_academic_dataset",sep="")))
    #2 - Create business review variables 
        yelp_Review_Variables(Directory=prep$baseDir)
    #3 - Create business super city and category variables
        yelp_Super_Variables(Directory=prep$baseDir,Parallel =T)
    #4 - Limit data to restaurants 
        yelp_Subset_Restaurants(Directory=prep$baseDir,outDirectory=prep$derDir)
    #5 - Create attributes dataset    
        yelp_Create_Attributes(mainDir=prep$mainDir, baseDir=prep$baseDir, derDir=prep$derDir)
    #6 - Modify and Merge attributes dataset    
        yelp_Merge_Attributes(Directory=prep$derDir)
    #7 - Create and modify more variables
        yelp_More_Vars(Directory=prep$derDir)
    #8 - Break categories into variables
        yelp_Unlist_Categories(Directory=prep$derDir)
    #9 - Impute missing data
        yelp_Impute_Missing(Directory=prep$derDir)
    #10 - Create review categories
        yelp_Review_Cats_Function(Directory=prep$derDir,BaseDirectory=prep$baseDir)
    #11 - Create customer adjacency list
        yelp_Customer_Adj_Function(City=City, Partition="All",Directory=derDir,BaseDirectory=baseDir)
    #12 - Create distance adjacency list
        yelp_Distance_Adj_Function(City=City,Directory=derDir)
    #13 - Create attribute adjacency list
        yelp_Attribute_Adj_Function(City=City,Directory=derDir)
    #14 - Create weighted attribute adjacency list and niche centrality measures
        yelp_Weighted_Adj_Function(City=City,Directory=derDir)
    #15 - Create dynamic review and stars variables
        yelp_Dynamic_Vars(City=City,Intervals=Intervals,Directory=derDir,BaseDirectory=baseDir)
    #16 - Create specialization variables
        yelp_Specialization_Vars(City=City,Intervals=Intervals,AtrMax=.05,Directory=derDir)
    #17 - Create Density Dependence Variables
        yelp_Density_Vars(City=City,DistMax=4,DistCuts=2,AtrType="Normal",AtrMax=.05,AtrCuts=1,Intervals=Intervals,Directory=derDir,CustAtrCuts=NULL,Customers="No",Parallel=F, Competitors="All",atrranks=atrranks,atrrankcuts=atrrankcuts)
        
