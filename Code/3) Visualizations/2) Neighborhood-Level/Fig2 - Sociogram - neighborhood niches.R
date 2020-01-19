#0 - libraries
  {
    library(ggplot2)
    library(data.table)
    library(plm)
    library(pglm)
    library(lme4)
    library(nlme)
    library(lmerTest)
    library(ggConvexHull)
    library(sf)
    library(smoothr)
    library(ggalt) 
    library(RColorBrewer)
    library(viridis)
}
#0.5 - Create functions
  {
    #prep graphing functions
      VisTextNet <- function(text_network, viz=NULL, alpha = .25, label_degree_cut=0, betweenness=FALSE){
        if (is.null(V(text_network)$name)){
          text_network <- set_vertex_attr(text_network, "name", value = as.character(1:vcount(text_network)))
        }
        #create network backbone
        e <- cbind(igraph::as_data_frame(text_network)[, 1:2 ], weight = E(text_network)$weight)
        # in
        w_in <- igraph::graph.strength(text_network, mode = "in")
        w_in <- data.frame(to = names(w_in), w_in, stringsAsFactors = FALSE)
        k_in <- igraph::degree(text_network, mode = "in")
        k_in <- data.frame(to = names(k_in), k_in, stringsAsFactors = FALSE)
        e_in <- e %>%
          left_join(w_in, by = "to") %>%
          left_join(k_in, by = "to") %>%
          mutate(alpha_in = (1-(weight/w_in))^(k_in-1))
        # out
        w_out <- igraph::graph.strength(text_network, mode = "out")
        w_out <- data.frame(from = names(w_out), w_out, stringsAsFactors = FALSE)
        k_out <- igraph::degree(text_network, mode = "out")
        k_out <- data.frame(from = names(k_out), k_out, stringsAsFactors = FALSE)
        e_out <- e %>%
          left_join(w_out, by = "from") %>%
          left_join(k_out, by = "from") %>%
          mutate(alpha_out = (1-(weight/w_out))^(k_out-1))
        e_full <- left_join(e_in, e_out, by = c("from", "to", "weight"))
        e_full <- e_full %>%
          mutate(alpha = ifelse(alpha_in < alpha_out, alpha_in, alpha_out)) %>%
          dplyr::select(from, to, alpha)
        E(text_network)$alpha <- e_full$alpha
        pruned <- igraph::delete.edges(text_network, which(E(text_network)$alpha >= alpha))
        V(pruned)$degree <- igraph::degree(pruned)
        isolates <- V(pruned)[degree(pruned)==0]
        communities <- cluster_louvain(as.undirected(pruned))
        V(pruned)$modularity<-communities$membership
        isolates2 <- sum(data.frame(table(communities$membership))$Freq==2)
        if(is.null(viz)) viz="none"
        if(viz=="new"){
          vcol<-V(pruned)$modularity
          # pruned <- delete.vertices(pruned, isolates)
          out<-plot(pruned,vertex.color=V(pruned)$modularity,vertex.size=10,vertex.label=V(pruned)$name, edge.arrow.size=0,edge.width=0)
        }
        if(viz=="none") out<-list(Modularity=modularity(as.undirected(pruned),V(pruned)$modularity),transitivity=transitivity(pruned),clusters=length(unique(communities$membership)), isolates=length(isolates), isolates2=isolates2, comps=components(pruned)$no)
        if(viz=="orig") out<-plot(text_network,vertex.color=V(pruned)$modularity,vertex.label=NA)
        if(viz=="colors") out <- communities$membership
        if(viz=="layout") out <- layout.fruchterman.reingold(pruned)
        if(viz=="cust") out<-list(pruned,communities)
        return(out)
      }

      findalpha<-function(x){
        df<-list()
        alphas<-(c(1:60)/100)
        # alphas<-(c(5:35)/100)
        for(i in 1:length(alphas)){
          df[[i]]<-VisTextNet(x,alpha = alphas[i],viz="none")
        }
        df2<-data.frame(alph=alphas,iso=sapply(df, function(x) x$isolates),iso2=sapply(df, function(x) x$isolates2),clus=sapply(df, function(x) x$clusters),mod=sapply(df, function(x) x$Modularity),comps=sapply(df, function(x) x$comps))
         #alternate based on directed graph formula:
            df2$clus3<-
              df2$mod-
              1000000*df2$comps
    
        alph<-df2$alph[df2$clus3==max(df2$clus3)][1]
        return(alph)
      }
      
      prettyplot<-function(x,alpha) {
        suppressWarnings({
          set.seed(2)
          df<-as.data.frame(VisTextNet(x,alpha = alpha,viz="layout"))
          df$colors<-VisTextNet(x,alpha = alpha,viz="colors")
          temp<-as.data.frame(table(df$colors))
          temp<-as.numeric(as.character(temp$Var1[temp$Freq==1]))
          df$colors[df$colors%in% temp]<-0
          df$names<- V(x)$name
          nsize=125/length(df$names)
          out<-ggplot(df[df$colors>0,],aes(x=V1,y=V2,label=names,color=factor(colors),fill=factor(colors)))+
            geom_point(size=nsize*2,color="black",shape=21)+
            geom_text_repel(size=nsize*2,color="black",fontface="bold",force=2,box.padding=.25,nudge_y=.15)+ #,nudge_y=.1
            theme_void()+
            # stat_ellipse(geom = "polygon",alpha=.175,color="white")
            geom_convexhull(alpha=.2)
          return(out)
        })
      }
      
      prepdatefun<-function(geoids,closedate,opendate){
        #c) load relevant data
            Bus3<-Bus[Bus$GEOID %in% geoids & (Bus$date_close>=closedate | Bus$is_open==1) & Bus$date_open<=opendate,]
            cit<-Bus3$city_super[1]
            ind<-(Bus$GEOID %in% geoids & (Bus$date_close>=closedate | Bus$is_open==1) & Bus$date_open<=opendate)[Bus$city_super==cit]
            range01 <- function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}
            atr3<-range01(readRDS(paste(cit,"_atr.rds",sep="")))
            temp3<-readRDS(paste(cit,"_revtopics.rds",sep=""))
            temp3<-range01(temp3)
            atr3<-rowMeans(cbind(atr3,temp3),na.rm = T)
            atr3<-range01(atr3)
            atr3<-matrix(atr3,nrow=sqrt(length(atr3)))
            atr3<-atr3[which(ind),which(ind)]
        #i) prep graph
            geomat<-1/atr3
            diag(geomat)<-0
            geomat[is.infinite(geomat)]<-max(geomat[!is.infinite(geomat)])
            geomat[geomat>quantile(geomat,.99)]<-quantile(geomat,.99)*1.5
            perc.rank <- function(x) trunc(rank(x))/length(x)
            tempmat<-atr3
            tempmat<-apply(tempmat, 2, perc.rank)
            geomat2<-1/tempmat
            geomat2[is.infinite(geomat2)]<-0
            a<-rowSums(geomat2)/min(rowSums(geomat2))
            b<-apply(geomat2, 2, function(x) x/(a))
            geomat2<-b
            N<-nrow(geomat2)
            cutoff<-nrow(geomat2)*5
            cut<-sort(c(geomat2),decreasing = T)[cutoff]
            geomat2[geomat2<cut]<-0
            diag(geomat2)<-0
            geomat3<-(geomat2)*(geomat+max(geomat))
            g1<-graph_from_adjacency_matrix(geomat3,weighted = T,mode="directed")
            # sum(geomat2>0)/nrow(geomat2)
            # apply(geomat2,2,function(x) sum(x>0))
        #iii) find cluster names
            temp<-VisTextNet(g1,alpha = findalpha(g1),viz="cust")
            g1t<-temp[[1]]
            V(g1t)$aname<-Bus3$name
            communities<-temp[[2]]
        #iv) identify what each cluster is loading on, generally
            #subset the data to category info
                clus<-communities$membership
                tempdf<-Bus3[,c(which(names(Bus3)=="BikeParking"):which(names(Bus3)=="chain_super"),
                                which(names(Bus3)=="Acai Bowls"):which(names(Bus3)=="Wraps"))]
            #find group means for each cluster
                tempdf$clus<-clus
                tempdf2<-data.table(tempdf)
                tempdf3<-tempdf2[, lapply(.SD, mean, na.rm=TRUE), by=clus ]
                tempdf3$clus<-NULL
                ord<-unique(tempdf$clus)
            #find distinctive features, based on what distinguishes one from all others
                distinctfeatures<-list()
                for(i in 1:nrow(tempdf3)){
                  temprow<-tempdf3[i,]
                  tempdf4<-tempdf3[-i,]
                  tempdf4<-tempdf4[, lapply(.SD, max, na.rm=TRUE)]
                  distinctfeatures[[i]]<-round(sort(temprow-(tempdf4+.01),decreasing = T),2)[,1:5]
                }
                distinctfeatures<-distinctfeatures[match(1:nrow(tempdf3),unique(tempdf$clus))]
                nodecolours<-ifelse(Bus3$is_open==0,"red",
                             ifelse(Bus3$date_open>2013,"deepskyblue",
                             ifelse(Bus3$is_open==1,"blue",NA))) #number of original that died
        #v) return necessary info
            rets<-list(g1t,distinctfeatures,communities,nodecolours,atr3,Bus3)
            return(rets)
      }
  }
#1 - load and clean data
  {
    setwd(Directory)
    ch<-readRDS("2013-2016_changes.rds")
    Bus<-readRDS("Restaurants.rds")
    Cen<-readRDS("Censusinfo.rds")
    Bus<-cbind(Bus,Cen)
    datefun<-function(x) as.numeric(substr(x,1,4))+(as.numeric(substr(x,6,7)))/12
    Bus$date_open<-datefun(Bus$date_open)
    Bus$date_close<-datefun(Bus$date_close)
    Bus$date_close<-ifelse(Bus$is_open==1, 2100, Bus$date_close)
    Bus$date_close<-.5*floor(Bus$date_close/.5)
    Bus$date_open<-.5*floor(Bus$date_open/.5)
    
#2 - Create df of census tracts that includes hazard info
    cendf<-readRDS(paste(prep$Cities[1],"_cendf.rds",sep=""))
    cluststr<-readRDS(paste(prep$Cities[1], "_cluststr.rds",sep=""))
    cHaz<-merge(cendf,cluststr,by="cluster")
    for(i in 2:length(prep$Cities)){
      City<-prep$Cities[i]
      cendf<-readRDS(paste(City,"_cendf.rds",sep=""))
      cluststr<-readRDS(paste(City, "_cluststr.rds",sep=""))
      temp<-merge(cendf,cluststr,by="cluster")
      cHaz<-rbind(cHaz,temp)
    }

#3 - sort data by GEOID, Interval, and Cluster
    cHaz$cluster2<-cHaz$cluster+
       ifelse(cHaz$city_super=="Champaign",1000,
       ifelse(cHaz$city_super=="Charlotte",2000,
       ifelse(cHaz$city_super=="Cleveland",3000,
       ifelse(cHaz$city_super=="Las Vegas",4000,
       ifelse(cHaz$city_super=="Madison",5000,
       ifelse(cHaz$city_super=="Phoenix",6000,
       ifelse(cHaz$city_super=="Pittsburgh",7000,NA)))))))
    cHaz<-cHaz[order(cHaz$cluster2,cHaz$GEOID,cHaz$Interval),]
  } 
#4) Determine ideal cases to examine
  {  
  x=70;y=60
    a<-ch$modularity.2016[ch$alive.2013<x & ch$alive.2013>y]
    b<-ch$clusters.2016[ch$alive.2013<x & ch$alive.2013>y]
    max(a)
    max(b)
    min(a)
    min(b)
    d<-b
    b<-b+5
    a[a*b==max(a*b)]
    d[a*b==max(a*b)]
    a[a*b==min(a*b)]
    d[a*b==min(a*b)]
    clust1<-ch$cluster2[ch$alive.2013<x & ch$alive.2013>y][a*b==max(a*b)][1]
    clust2<-ch$cluster2[ch$alive.2013<x & ch$alive.2013>y][a*b==min(a*b)][1]
    clusttemp<-ch$cluster2[ch$alive.2013>100][109]
  }

#5) subset data
  {
    #a) 2013 or later
        cHaz2<-cHaz
        cHaz2<-cHaz2[cHaz2$Interval>=2013,]
        cHaz31<-cHaz2[cHaz2$cluster2==clust1,]
        cHaz32<-cHaz2[cHaz2$cluster2==clust2,]
        cHaztemp<-cHaz2[cHaz2$cluster2==clusttemp,]
        
        ch[cHaz31$GEOID[1]==ch$GEOID.2013,]$new.niche
        ch[cHaz31$GEOID[1]==ch$GEOID.2013,]$new.niche.outlier
        ch[cHaz31$GEOID[1]==ch$GEOID.2013,]$alive.2013
        
        ch[cHaz32$GEOID[1]==ch$GEOID.2013,]$new.niche
        ch[cHaz32$GEOID[1]==ch$GEOID.2013,]$new.niche.outlier
        ch[cHaz32$GEOID[1]==ch$GEOID.2013,]$alive.2016
        
        
        cHaz31[c(1,7),c("modularity","clusters","Interval")]
        cHaz32[c(1,7),c("modularity","clusters","Interval")]
        
    #b) load relevant data
        geoids1<-unique(cHaz31$GEOID)
        geoids2<-unique(cHaz32$GEOID)
        geoidstemp<-unique(cHaztemp$GEOID)
   }   
#6) compare 2013 to 2016 modules
  {
      set.seed(1); temp1<-prepdatefun(geoids=geoids1,opendate=2013,closedate=2013)
      g1t<-temp1[[1]]
      distinctfeatures1<-temp1[[2]]
      communities1<-temp1[[3]]
      nodecolours1<-temp1[[4]]
      communities1[[7]]
      distinctfeatures1
      cusdistinctfeatures1<-c("Pizza","Bars","Breakfast","Mexican","Chain","Wine")
      custlabs1<-cusdistinctfeatures1[communities1$membership]
      # custlabs[c(1,5)]<-NA  #for changing where node label goes
      custlabs1[duplicated(custlabs1)]<-NA
    #weight within-cluster edges more strongly
        #find get edge clusters
            g<-g1t
            coms<-communities1$names
            N<-max(as.numeric(coms))
            t1<-as.data.frame(as_edgelist(g))
            t2<-as.data.frame(cbind(rep(1:N,each=N),rep(1:N,N)))
            t2$clus1<-rep(V(g)$modularity,each=N)
            t2$clus2<-rep(V(g)$modularity,N)
            t1<-t2[paste(t2[,1],t2[,2]) %in% paste(t1[,1],t1[,2]),]
        #adjust weights based on shared cluster and cluster isolation
            #strengthen shared cluster ties
                a<-as.data.frame(table(t1[,3:4]))
                a<-a[a$clus1!=a$clus2,]
                a1<-aggregate(a$Freq,sum,by=list(a$clus1))
                a1$x<-a1$x+aggregate(a$Freq,sum,by=list(a$clus2))$x
                adj<-ifelse(t1$clus1==t1$clus2,1,0)
            #strengthen different cluster ties if ego isolated
                adj<-adj*a1$x[t1$clus1]+1
                adj2<-ifelse(t1$clus1!=t1$clus2,1,0)
                adj2<-adj2*1/(a1$x[t1$clus1]*a1$x[t1$clus2])*max((a1$x[t1$clus1]*a1$x[t1$clus2]))
                E(g)$weight<-E(g)$weight+adj^3+(adj2^3)/max(adj*1.5)
             g1t<-g
      set.seed(1);plot(communities1,g,vertex.size=5,col="white",vertex.label=custlabs1, edge.arrow.size=0,edge.width=0,edge.color="black",
                       vertex.label.cex = 2,vertex.label.color="black",vertex.label.font=2,layout=layout_with_fr)
      #assign weights to variables
          set.seed(1); temp2<-prepdatefun(geoids=geoids2,opendate=2013,closedate=2013)
          g2t<-temp2[[1]]
          distinctfeatures2<-temp2[[2]]
          communities2<-temp2[[3]]
          nodecolours2<-temp2[[4]]
          communities2[[1]]
          distinctfeatures2
          cusdistinctfeatures2<-c("Hot Dogs","Mexican","Bars","Delis","Breakfast","Southern","Expensive","Asian","Indian","Pizza","Greek","Lounge")
          custlabs2<-cusdistinctfeatures2[communities2$membership]
          custlabs2[duplicated(custlabs2)]<-NA
      #weight within-cluster edges more strongly
          #find get edge clusters
              g<-g2t
              coms<-communities2$names
              N<-max(as.numeric(coms))
              t1<-as.data.frame(as_edgelist(g))
              t2<-as.data.frame(cbind(rep(1:N,each=N),rep(1:N,N)))
              t2$clus1<-rep(V(g)$modularity,each=N)
              t2$clus2<-rep(V(g)$modularity,N)
              t1<-t2[paste(t2[,1],t2[,2]) %in% paste(t1[,1],t1[,2]),]
          #adjust weights based on shared cluster and cluster isolation
              #strengthen shared cluster ties
                  a<-as.data.frame(table(t1[,3:4]))
                  a<-a[a$clus1!=a$clus2,]
                  a1<-aggregate(a$Freq,sum,by=list(a$clus1))
                  a1$x<-a1$x+aggregate(a$Freq,sum,by=list(a$clus2))$x
                  adj<-ifelse(t1$clus1==t1$clus2,1,0)
              #strengthen different cluster ties if ego isolated
                  adj<-adj*a1$x[t1$clus1]+1
                  adj2<-ifelse(t1$clus1!=t1$clus2,1,0)
                  adj2<-adj2*1/(a1$x[t1$clus1]*a1$x[t1$clus2])*max((a1$x[t1$clus1]*a1$x[t1$clus2]))
                  E(g)$weight<-E(g)$weight+adj^3+(adj2^3)/max(adj*1.5)
                  g2t<-g
                  set.seed(1);plot(communities2,g2t,vertex.size=5,col=nodecolours2,vertex.label=custlabs2, edge.arrow.size=0,edge.width=0,edge.color="black",
                  vertex.label.cex = 2,vertex.label.color="black",vertex.label.font=2)
} 
#7) make graph prettier
  {
    #a) prep nodes and edges
      g<-g2t
      membs<-communities2$membership
        #grab data, make names, kill isolates
            set.seed(2)
            # df<-as.data.frame(VisTextNet(g,alpha = findalpha(g),viz="layout"))
            df<-as.data.frame(layout_with_fr(g))
            df$colors<-membs
            temp<-as.data.frame(table(df$colors))
            temp<-as.numeric(as.character(temp$Var1[temp$Freq==1]))
            df$colors[df$colors%in% temp]<-0
            df$names<- V(g)$aname
            nsize=125/length(df$names)
        #grab edgelist, make edges
            tg<-g
            V(tg)$name<-V(g)$aname
            edg<-as.data.frame(as_edgelist(tg))
            names(edg)<-c("send","rec")
            temp<-df[c(1,2,4)]
            names(temp)<-c("xbeg","ybeg", "send")
            edg<-merge(edg,temp)
            names(temp)<-c("xend","yend", "rec")
            edg<-merge(edg,temp)
            temp<-df[c(3,4)]
            names(temp)<-c("col1","send")
            edg<-merge(edg,temp)
            names(temp)<-c("col2","rec")
            edg<-merge(edg,temp)
        #remove duplicates
            edg$send<-as.character(edg$send)
            edg$rec<-as.character(edg$rec)
            for (i in 1:nrow(edg)){
              edg[i,1:2] = sort(edg[i,1:2 ])
            }
            edg = edg[!duplicated(edg[,1:2]),]

    #b) prep pretty contours
          rtnorm <- function(n, mean, sd, min = -Inf, max = Inf){
            qnorm(runif(n, pnorm(min, mean, sd), pnorm(max, mean, sd)), mean, sd)
          }
          myhull<-function(df,dist,n){
            xl<-list()
            yl<-list()
            #expand size of hull and slightly smooth
                for(i in 1:nrow(df)){
                  # xl[[i]]<-rtnorm(n=n,mean=df[i,1],sd=dist,min=df[i,1]-dist,max=df[i,1]+dist)
                  # yl[[i]]<-rtnorm(n=n,mean=df[i,2],sd=dist,min=df[i,2]-dist,max=df[i,2]+dist)
                  xl[[i]]<-c(df[i,1]+dist,df[i,1]-dist,df[i,1]+dist,df[i,1]-dist)
                  yl[[i]]<-c(df[i,2]+dist,df[i,2]+dist,df[i,2]-dist,df[i,2]-dist)
                }
                hulldf<-as.data.frame(cbind(do.call(c,xl),do.call(c,yl)))
                names(hulldf)<-c("x","y")
            #add community grouping if 3rd col in df
                if(ncol(df)>2){
                    # hulldf[,3]<-rep(df[,3],each=n)
                    hulldf[,3]<-rep(df[,3],each=4)
                    names(hulldf)[3]<-names(df)[3]
                }
            #smooth the edges substantially
                if(ncol(df)>2){
                    newhulls<-list()
                    hulldfl<-split(hulldf[,1:2],factor(hulldf[,3]))
                    for(i in 1:length(hulldfl)){
                      m<-hulldfl[[i]][chull(hulldfl[[i]]),]
                      P1 = Polygon(m)
                      Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")))
                      p_smooth_chaikin <- smooth(Ps1, method = "chaikin")
                      newhulls[[i]]<-as.data.frame(p_smooth_chaikin@polygons[[1]]@Polygons[[1]]@coords)
                    }
                    newhulldf<-do.call(rbind,newhulls)
                    names(newhulldf)<-c("x","y")
                    newhulldf[,3]<-unlist(mapply(rep, unique(df[,3]), sapply(newhulls,nrow)))
                    names(newhulldf)[3]<-names(df)[3]
                  }else{
                    m<-hulldf[chull(hulldf),1:2]
                    P1 = Polygon(m)
                    Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")))
                    p_smooth_chaikin <- smooth(Ps1, method = "chaikin")
                    newhulls<-as.data.frame(p_smooth_chaikin@polygons[[1]]@Polygons[[1]]@coords)
                    names(newhulldf)<-c("x","y")
                }
              return(newhulldf)
          }
          hulldf<-myhull(df,dist=.1,n=1000)
  }
      #c) plot prep
  {
        #match centers in hulldf with those in df to be same color
            colord<-unique(df$colors)
            hcenters<-cbind(sapply(split(hulldf[,1],hulldf$colors),mean),sapply(split(hulldf[,2],hulldf$colors),mean))
            dcenters<-cbind(sapply(split(df[,1],df$colors),mean),sapply(split(df[,2],df$colors),mean))
            hmatches<-vector()
            for(i in 1:nrow(hcenters)) hmatches[i]<-which(abs(hcenters[i,1]-dcenters[,1])+abs(hcenters[i,2]-dcenters[,2])==min(abs(hcenters[i,1]-dcenters[,1])+abs(hcenters[i,2]-dcenters[,2])))
            temp<-hulldf$colors
            for(i in 1:nrow(hcenters)) temp[hulldf$colors==unique(hulldf$colors)[i]]<-colord[hmatches[i]]
            hulldf$colors<-temp
        #turn color into factor
            hulldf$colors<-factor(hulldf$colors)
            df$colors<-factor(df$colors)
        #alternate edges; just between clusters
            edg2<-edg
            edg2<-as.data.frame(sapply(edg2,as.factor))
            edg2<-edg2[edg2$col1!=edg2$col2,]
            edg3<-edg2[,c("col1","col2")]
            #merge with hull centers
                hcenters<-cbind(sapply(split(hulldf[,1],hulldf$colors),mean),sapply(split(hulldf[,2],hulldf$colors),mean))
                for (i in 1:nrow(edg3)){
                  edg3[i,1:2] = sort(edg3[i,1:2 ])
                }
                a<-as.data.frame(table(edg3[,1:2]))
                edg2<-data.frame(xbeg=rep(hcenters[,1],each=nrow(hcenters)),ybeg=rep(hcenters[,2],each=nrow(hcenters)), xend=rep(hcenters[,1],nrow(hcenters)),yend=rep(hcenters[,2],nrow(hcenters)) )
                edg2$freq<-a$Freq
                edg2<-edg2[edg2$freq!=0,]
            #death info
                df$dissolved<-ifelse((Bus$date_close<2016)[match(df$names,Bus$name)],"black","grey99")
                #df$dissolved<-"grey99"
  }
  
  #plot for paper 1 (bw version)
      png(file="C:/Users/bda13/Desktop/Fig 6 - Death by Cluster2.png", height=8.5, width=11,units = "in",res = 300)
      ggplot()+
          geom_encircle(data=hulldf,aes(x=x,y=y,group=colors,color="black"),expand=0,colour="black",size=2)+
          geom_encircle(data=hulldf,aes(x=x,y=y,fill=colors),alpha=1,expand=0,colour="black")+
          geom_segment(data = edg,aes(x = xbeg, y = ybeg, xend = xend, yend = yend))+
          geom_point(data=df,aes(x=V1,y=V2),fill=df$dissolved, color="black",stroke=1.1,size=7,shape=21)+ #fill="grey99",color=factor(colors),fill=factor(colors)
          theme_void()+
          guides(fill=FALSE)+
          scale_fill_grey(start = .925, end = .999)
      dev.off()

  #plot for paper 1 (color version)
      df$dissolved<-ifelse((Bus$date_close<2016)[match(df$names,Bus$name)],"red","grey99")
      png(file="C:/Users/bda13/Desktop/Fig 6 - Death by Cluster2 (color).png", height=8.5, width=11,units = "in",res = 300)
      ggplot()+
          geom_encircle(data=hulldf,aes(x=x,y=y,group=colors,color="black"),expand=0,colour="black",size=2)+
          geom_encircle(data=hulldf,aes(x=x,y=y,group=colors,fill=colors),alpha=.5,expand=0,colour="black")+
          geom_segment(data = edg,aes(x = xbeg, y = ybeg, xend = xend, yend = yend))+
          geom_point(data=df,aes(x=V1,y=V2),fill=df$dissolved, color="black",stroke=1.1,size=7,shape=21)+ #fill="grey99",color=factor(colors),fill=factor(colors)
          theme_void()+
          guides(fill=FALSE)
      dev.off()

  #plot version with labels
      V(g)$aname[as.numeric(communities2[[9]])]
      Bus$cat[match(V(g)$aname,Bus$name)][as.numeric(communities2[[6]])]
      distinctfeatures2
      cusdistinctfeatures2<-c("Sushi","Cafes","High-end","Sandwich","Mexican","Chinese/Thai","Pubs/Bars","Pizza","Western Buffets")
      custlabs2<-cusdistinctfeatures2[communities2$membership]
      #make each label appear above highest dot
          for(i in unique(as.numeric(as.character(df$colors)))){
              ys<-df$V2[df$colors==i]
              ys<-ys==max(ys)
              custlabs2[df$colors==i]<-ifelse(ys,custlabs2[df$colors==i],NA)
          }
      #make label positions centered
          df2<-data.table(df)
          df2[,V1m:=mean(V1),by=df2$color]
      #plot
          png(file="C:/Users/bda13/Desktop/Fig 6 - Death by Cluster2 (labels).png", height=8.5, width=11,units = "in",res = 300)
          ggplot()+
            geom_encircle(data=hulldf,aes(x=x,y=y,group=colors,color="black"),expand=0,colour="black",size=2)+
            geom_encircle(data=hulldf,aes(x=x,y=y,group=colors,fill=colors),alpha=.5,expand=0,colour="black")+
            geom_segment(data = edg,aes(x = xbeg, y = ybeg, xend = xend, yend = yend))+
            geom_point(data=df,aes(x=V1,y=V2),fill=df$dissolved, color="black",stroke=1.1,size=7,shape=21)+ #fill="grey99",color=factor(colors),fill=factor(colors)
            theme_void()+
            guides(fill=FALSE,color=F)+
          geom_label_repel(data=df2,aes(x=V1m,y=V2,color=colors), label=custlabs2, size=nsize*4,fontface="bold",force=2,box.padding=.25,nudge_y=.15,segment.size = NA)#+
          dev.off()

          
          