{
City="Durham"

#load main data
    setwd(prep$derDir)
    Bus<-readRDS("Restaurants.rds")
    temp<-readRDS(paste(City,"_atr.rds",sep=""))
    dist<-readRDS(paste(City,"_dist.rds",sep=""))
    dist<-matrix(dist,nrow = sqrt(length(dist)))
    Bus2<-Bus[Bus$city_super==City,]
    
    
#Graph similarity matrix
    range01 <- function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}
    temp<-range01(temp)
    mathh<-matrix(temp,nrow=sqrt(length(temp)))
    
#remove non-restaurants
    rowrem<-which(Bus2$name=="The Freeman Center" | Bus2$name=="The Coop")
    mathh<-mathh[c(-rowrem),c(-rowrem)]
    Bus2<-Bus2[c(-rowrem),]
    dist<-dist[c(-rowrem),c(-rowrem)]
    
#get date closed and open for these places
    #read data
        setwd(prep$baseDir)
        revdf<-readRDS("durham_revdf.RDS")
        datedf<-readRDS("durham_datedf.RDS")
        Bus.dur<-readRDS("durham_bus.RDS")
        setwd(prep$derDir)
    #Subset according to Bus2
        datedf<-datedf[match(Bus2$id,Bus.dur$id)]
        revdf<-revdf[match(Bus2$id,Bus.dur$id)]
        Bus.dur<-Bus.dur[match(Bus2$id,Bus.dur$id),]
        Bus.dur$name
        Bus2$name
    #find close dates
        datefun<-function(x) suppressWarnings(as.numeric(substr(x,nchar(x)-3,nchar(x)))+ ifelse(substr(x,2,2)=="/",as.numeric(substr(x,1,1))/12,as.numeric(substr(x,1,2))/12))        
        Bus2$date_close<-sapply(datedf,function(x) max(datefun(x)))
        Bus2$date_open<-sapply(datedf,function(x) min(datefun(x)))
    
    #subset data to places open during at 2017
        subs<-Bus2$date_close>=2017 & Bus2$date_open<=2017
        # subs<-Bus2$date_close>=2017.5 & Bus2$date_open<=2017.5
        
        Bus2<-Bus2[subs,]
        mathh<-mathh[c(subs),c(subs)]
        Bus.dur<-Bus.dur[subs,]
        revdf<-revdf[subs]
        
        dist<-dist[c(subs),c(subs)]
        chubbys<-which(Bus2$name=="Chubby's Tacos")
    
    #make closer
        subs2<-dist[chubbys,]<.25

        Bus2<-Bus2[subs2,]
        mathh<-mathh[c(subs2),c(subs2)]
        Bus.dur<-Bus.dur[subs2,]
        dist<-dist[c(subs2),c(subs2)]
        chubbys<-which(Bus2$name=="Chubby's Tacos")
        revdf<-revdf[subs2]
        
    #TEMP - remove chains???
        # subs<-Bus2$chain!=1
        # Bus2<-Bus2[subs,]
        # mathh<-mathh[c(subs),c(subs)]
        # chubbys<-which(Bus2$name=="Chubby's Tacos")
        
    #graph network
        geomat<-1/mathh
        geomat[is.infinite(geomat)]<-0
        diag(geomat)<-0
        g1<-graph_from_adjacency_matrix(geomat,weighted = T,mode="undirected")


    #How about by closest alters
        perc.rank <- function(x) trunc(rank(x))/length(x)
        mathh2<-apply(mathh, 1, perc.rank)
        geomat2<-1/mathh2
        geomat2[is.infinite(geomat2)]<-0
        diag(geomat2)<-0
        g2<-graph_from_adjacency_matrix(geomat2,weighted = T,mode="directed")

    #middle weighting
        geomat3<-sqrt(geomat2)*geomat

        
        g3<-graph_from_adjacency_matrix(geomat3,weighted = T,mode="directed")
        
        saveRDS(geomat3,"C:/Users/bda13/Desktop/Data Analytics for Business/scf2016/geomat3.rds") 
        plot(g3,vertex.size=10, edge.arrow.size=0,edge.width=0,vertex.label=Bus2$name)
        
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
    select(from, to, alpha)
  E(text_network)$alpha <- e_full$alpha
  pruned <- igraph::delete.edges(text_network, which(E(text_network)$alpha >= alpha))
  V(pruned)$degree <- igraph::degree(pruned)
  isolates <- V(pruned)[igraph::degree(pruned)==0]
  pruned <- igraph::delete.vertices(pruned, isolates)
  communities <- cluster_optimal(pruned)
  if(is.null(viz)) {out<-list(Modularity=modularity(pruned,V(pruned)$modularity),transitivity=transitivity(pruned))
  }else if(viz=="new"){
    vcol<-V(pruned)$modularity
    out<-plot(pruned,vertex.color=vcol,vertex.size=10,vertex.label=NA, edge.arrow.size=0,edge.width=0)
    out<-plot(pruned,vertex.color=V(pruned)$modularity,vertex.size=10,vertex.label=Bus2$name, edge.arrow.size=0,edge.width=0)
  }else if(viz=="orig") out<-plot(text_network,vertex.color=V(pruned)$modularity,vertex.label=Bus2$name)
  if(viz=="colors") out <- communities$membership
  if(viz=="layout") out <- layout.fruchterman.reingold(pruned)
  if(viz=="network") out <- pruned
  
  return(out)
}
}

#Pull key data from network
    VisTextNet(g3,alpha = .25,viz="orig")
    set.seed(2); df<-VisTextNet(g3,alpha = .25,viz="layout")
    set.seed(2); colors<-VisTextNet(g3,alpha = .25,viz="colors")
    set.seed(2); net<-VisTextNet(g3,alpha = .25,viz="network")
    set.seed(2); VisTextNet(g3,alpha = .25,viz="new")

#prep variable names
    df<-as.data.frame(df)
    df$V3<-"black"
    df$V3[chubbys]<-"red"
    Bus2$name[1]<-"Ninth Street Coffee"
    Bus2$name[3]<-"Blue Seafood"
    Bus2$name[4]<-"Vine Sushi"
    Bus2$name[5]<-"International"
    Bus2$name[8]<-"Dale's Indian"
    Bus2$name[9]<-"Bruegger's"
    Bus2$name[10]<-"Blue Corn"
    Bus2$name[12]<-"Bali Hai"
    Bus2$name[14]<-"Devil's Pizzeria"
    Bus2$name[16]<-"Mad Hatter's"
    Bus2$name[19]<-"Cosmic Cantina"
    Bus2$name[21]<-"Chubby's"
    Bus2$name[22]<-"Panera"
    Bus2$name[24]<-"Metro 8"
    Bus2$name[25]<-"Dain's"
    Bus2$name[26]<-"Banh's"
    Bus2$name[27]<-"Papa John's"
    df$V4<-4
    df$V4[chubbys]<-6

#move dale's to the right
  df[8,][1]<-.3
  df[8,][2]<-(-.85)
  
#change color of dale's and vine
  colors[8]<-colors[21]
  colors[4]<-colors[21]
  
#plot
 df<-as.data.frame(df)
 png(file="C:/Users/bda13/Desktop/Contour3.png", height=8, width=8*1.8,units = "in",res = 300)
 ggplot(df,aes(x=V1,y=V2,label=Bus2$name,color=factor(colors),fill=factor(colors)))+
   geom_point(size=4,color="black",shape=21)+
   geom_text_repel(size=df$V4,color="black",fontface="bold")+ #,nudge_y=.1
   theme_void()+
   stat_ellipse(geom = "polygon",alpha=.05,color="white",level=c(.8))
 dev.off()
 
#shift blue corn up
 df[10,][2]<-df[10,][2]+.5

#add lines between closest points
 temp<-as.matrix(dist(df))
 q<-apply(temp,1,function(x) (which(x%in%sort(x)[2:4])))
 segs1<-df[,c(1,2)]
 segs<-rbind(segs1,segs1)
 segs<-rbind(segs,segs1)
 segs$xend<-NA
 segs$yend<-NA
 for(i in 1:(nrow(segs)/3)){
   segs$xend[i]<-segs$V1[q[1,i]]
   segs$yend[i]<-segs$V2[q[1,i]]
   segs$xend[i+(nrow(segs)/3)]<-segs$V1[q[2,i]]
   segs$yend[i+(nrow(segs)/3)]<-segs$V2[q[2,i]]
   segs$xend[i+(nrow(segs)*2/3)]<-segs$V1[q[3,i]]
   segs$yend[i+(nrow(segs)*2/3)]<-segs$V2[q[3,i]]
 }
 
#tweak more business names
  Bus2$name=="Ninth Street Coffee"
  Bus2$is_open[Bus2$name=="Dale's Indian"]<-1
  Bus2$is_open[Bus2$name=="Ninth Street Coffee"]<-1
  df$V3<-ifelse(Bus2$is_open==1,"red","black")
  df<-as.data.frame(df)
  
#plot
  png(file="C:/Users/bda13/Desktop/Contour network.png", height=6, width=9*1.2,units = "in",res = 300)
  ggplot(NULL)+
    geom_segment(aes(x = V1, y = V2, xend = xend, yend = yend), data = segs,size=.1)+
    theme_void()+
    geom_label(size=4,color=df$V3,fontface="bold",data=df,aes(x=V1,y=V2,label=Bus2$name,color=factor(colors)),nudge_y=0) #,nudge_y=.1
  dev.off()
 