#1) Prep libraries
    setwd(Directory)
    library(gridExtra)
    library(RColorBrewer)
    library(ggplot2)
    library(dplyr)
    library(maps)
    library(ggmap)
    library(dbscan)
    library(viridis)
    library(tigris)
    library(sf)
    library(tidyverse)
    library(viridis)
    library(cowplot)
    options(tigris_class = "sf")
    options(tigris_use_cache = TRUE)
    
#2) Create function for grabbing tracts of metro areas
    metro_tracts <- function(metro_name) {
      # First, identify which states intersect the metro area using the `states` function in tigris
        st <- states(cb = TRUE)
        cb <- core_based_statistical_areas(cb = TRUE)
        metro <- filter(cb, grepl(metro_name, NAME))
        stcodes <- st[metro,]$STATEFP
      # Then, fetch the tracts, using rbind_tigris if there is more than one state
        if(length(stcodes) > 1) {
          tr <- rbind_tigris(
            map(stcodes, function(x) {
              tracts(x, cb = TRUE)
            })
          )
        } else {
          tr <- tracts(stcodes, cb = TRUE)
        }

      # Now, find out which tracts are within the metro area
          within <- st_within(tr, metro)
          within_lgl <- map_lgl(within, function(x) {
            if (length(x) == 1) {
              return(TRUE)
            } else {
              return(FALSE)
            }
          })

      # Finally, subset and return the output
          output <- tr[within_lgl,]

      return(output)
    }
    
#3) Grab info for each metro area I need
    t1=0;t2=0;t3=0;t4=0;t5=0;t6=0;t7=0
    t1 <- metro_tracts("Champaign-Urbana")
    t2 <- metro_tracts("Madison")
    t3 <- metro_tracts("Pittsburgh")
    t4 <- metro_tracts("Charlotte-Concord-Gastonia")
    t5 <- metro_tracts("Cleveland-Elyria")
    t6 <- metro_tracts("Las Vegas-Henderson-Paradise")
    t7 <- metro_tracts("Phoenix-Mesa-Scottsdale")
        
#4) Produce maps for each city
    Bus<-readRDS("Restaurants.rds")
    Cen<-readRDS("Censusinfo.rds")
    Cen<-Cen[match(Bus$id,Cen$id),]
    Bus$GEOID<-Cen$GEOID      
    tl<-list(t1,t2,t3,t4,t5,t6,t7)
    pl<-list()
    bboxl<-list()
    i=3
    for(i in 1:length(tl)){
        #load data
            cendf<-readRDS(paste(prep$Cities[i],"_cendf.rds",sep=""))
            clustdf<-readRDS(paste(prep$Cities[i],"_clustdf.rds",sep=""))
            temp2<-tl[[i]]
        #subset map data to those in business data
            temp1<-temp2
            temp2<-temp2[temp2$GEOID %in% cendf$GEOID,]
            
        #Pull in census clusters and population
            cls<-cendf[,c("cluster","GEOID")]
            cls<-cendf[,c("cluster","GEOID")]
            cls2<-clustdf[,c("cluster","N")]
            cls<-merge(cls,cls2)
            
            temp2<-merge(temp2,cls,by="GEOID")
            temp2$cluster2<-factor(temp2$cluster)
        #For Madison, remove stoughton
            if(prep$Cities[i]=="Madison"){
              tgeos<-unique(Bus$GEOID[Bus$city=="Stoughton"])
              temp2<-temp2[!(temp2$GEOID %in% tgeos),]
            }
        #determine bounding box of tracts
            a<-st_union(temp2$geometry)
            bbox <- make_bbox(1, 1, f = 0.05)
            bbox[]<-st_bbox(a)
            ext1<-abs(bbox[1]-bbox[3])*.05
            ext2<-abs(bbox[2]-bbox[4])*.05
            bbox[1]<-bbox[1]-ext1
            bbox[3]<-bbox[3]+ext1
            bbox[2]<-bbox[2]-ext2
            bbox[4]<-bbox[4]+ext2
            bboxl[[i]]<-bbox
        #Create geometry for putting bold lines between clusters
            clusts<-unique(temp2$cluster2)
            clustslines<-temp2$geometry[1]
            for(j in 1:length(clusts)){
              clustslines[j]<-st_union(temp2$geometry[temp2$cluster2==clusts[j]])
            }
        #Pull ggmap
            Bus2<-Bus[Bus$city_super==prep$Cities[i],]
            cendf<-readRDS(paste(prep$Cities[i],"_cendf.rds",sep=""))
            lon <- Bus2$longitude
            lat <- Bus2$latitude
            df <- as.data.frame(cbind(lon,lat))
            register_google(key = "HIDDEN")
            map <- get_map(bbox,maptype = "watercolor", source = "stamen",color="bw")
            
        #make surrounded tracts white
            #include surrounded tracts
                a<-t(sapply(temp2$geometry,st_bbox))
                b<-t(sapply(temp1$geometry,st_bbox))
                b[1,]
                keeps<-vector()
                for(j in 1:nrow(b)){
                  keeps[j]<-any(a[,1]<b[j,1]) & any(a[,2]<b[j,2]) & any(a[,3]>b[j,3]) & any(a[,4]>b[j,4])
                }
                temp1<-temp1[keeps,]
                border<-st_union(temp1$geometry)
            #determine which of temp3 are surrounded by temp2
                #make one giant geometry of my tracts
                    temp4<-st_combine(temp2)
                    a<-st_contains(temp4,temp2)
                #find convex hull of tracts
                    temp5<-st_convex_hull(temp4)
                #just keep tracts within that convex hull
                    a<-st_contains(temp5,temp1)
                    temp1<-temp1[unlist(a),]
            #make fill = N
                    temp2$N
                    temp2$N2<-temp2$N
                    temp2$N2[temp2$N2>80]<-80
                temp1$N2<-1

            pl[[i]]<-ggmap(map,darken=c(.3,"white"))+
              geom_sf(data=temp1,aes(fill=N2),size=1.5,color="black",inherit.aes = FALSE)+
              geom_sf(data=temp2,aes(fill=N2),color="black",size=.5,inherit.aes = FALSE)+
              geom_sf(data=clustslines,size=1.5,color="black",fill=NA, inherit.aes = FALSE)+
              # scale_fill_viridis(option=1,direction = -1,begin=0,end=.6)+
              # scale_fill_grey()+
              scale_fill_gradient(low = "white", high = "#1bc3e5")+
              theme_bw() +
              coord_sf(crs = st_crs(temp2), datum = NA)+
              theme(axis.line = element_line(colour = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    legend.position = "none",
                    plot.title = element_text(hjust = 0.5),
                    text=element_text(family="Times",face="bold",size=20),
                    axis.title=element_blank())+
              ggtitle(prep$Cities[i])
            
            
    }
    
#5) put maps into one png
    getwd()
    heights<-sapply(bboxl, function(x) abs(x[2]-x[4]))
    heights<-heights/sum(heights)
    png(file="C:/Users/bda13/Desktop/Figure 1 - Census tract clusters.png", height=30, width=11,units = "in",res = 100)
    plot_grid(pl[[1]],pl[[2]],pl[[3]],pl[[4]],pl[[5]],pl[[6]],pl[[7]], nrow=7, align = "v", 
              rel_heights = c(heights))
    dev.off()              
    
#6) put maps into separate pngs
    for(i in 1:length(pl)){
      png(file=paste("C:/Users/bda13/Desktop/Figure 1",i,"- Census tract clusters.png"), height=40*heights[i], width=40*heights[i]*1.35,units = "in",res = 200)
      print(pl[[i]])
      dev.off()
      print(i)
    }

  