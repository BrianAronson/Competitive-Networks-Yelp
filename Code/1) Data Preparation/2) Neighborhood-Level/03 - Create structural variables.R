#' Create census tract structural variables
#' @export
#' @import data.table 
#' @import RCurl 
#' @import RJSONIO 
#' @import acs 
#' @import ggplot2 
#' @import tigris
#' @import sp 
#' @import raster 

yelp_cluster_str <- function(Directory, City){
  
#1 - load data
    setwd(Directory)
    Bus <- readRDS("Restaurants.rds")
    Cen <- readRDS("Censusinfo.rds")
    Bus <- Bus[Bus$city_super==City, ]
    Cen <- Cen[Cen$id%in%Bus$id, ]
    Cust <- readRDS(paste(City, "_cust.rds", sep=""))
    Dist <- readRDS(paste(City, "_dist.rds", sep=""))
    cendf <- readRDS(paste(City, "_cendf.rds", sep=""))
    clustdf <- readRDS(paste(City, "_clustdf.rds", sep=""))

#2 - Change dates to numeric. Assume date_close only true if business marked as closed; assign all other dates closed as 2100;
    datefun <- function(x) as.numeric(substr(x, 1, 4))+(as.numeric(substr(x, 6, 7)))/12
    Bus$date_open <- datefun(Bus$date_open)
    Bus$date_close <- datefun(Bus$date_close)
    Bus$date_close <- ifelse(Bus$is_open==1, 2100, Bus$date_close)
    Bus$date_close <- .5*floor(Bus$date_close/.5)
    Bus$date_open <- .5*floor(Bus$date_open/.5)
    
#3 - Create business ID vector
    IDs <- Bus$id

#4 - Assign attribute thresholds
    Atr <- readRDS(paste(City, "_atr.rds", sep=""))
    #create functions
        range01  <-  function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
        perc.rank  <-  function(x) (rank(x)-1)/(length(x)-1)
    #expand to 0-1
        Atr2 <- range01(Atr)
    #Alter to percentile ranks by row (i.e. find out which places ego is most and least similar to)
        temp <- matrix(Atr, nrow=sqrt(length(Atr)))
        Atr <- apply(temp, 1, perc.rank)
        Atr <- c(Atr)
        
#5 - bring in topic models
    temp3 <- readRDS(paste(City, "_revtopics.rds", sep=""))
    temp3 <- range01(temp3)
    Atr2 <- rowMeans(cbind(Atr2, temp3), na.rm = T)
    Atr2 <- range01(Atr2)
    
#6 - create Interv vector
    Interv2 <- (2017-2011)/Intervals
    Interv <- c(2011+(0:(Interv2)*Intervals))
    
#7 - pull in cluster info
    Cen <- Cen[match(Bus$id, Cen$id), ]
    Bus$GEOID <- Cen$GEOID
    temp <- cendf[, c("cluster", "GEOID")]
    Bus <- merge(Bus, temp, by="GEOID", all=T)
    Bus <- Bus[order(Bus$id), ]

#8 - Create datatable of info, only keeping clusters
    DT <- data.table(ID=rep(Bus$id, each=nrow(Bus)), ID2=rep(Bus$id, nrow(Bus)), Atr2, Dist, open1=rep(Bus$date_open, each=nrow(Bus)), close1=rep(Bus$date_close, each=nrow(Bus)), open2=rep(Bus$date_open, nrow(Bus)), close2=rep(Bus$date_close, nrow(Bus)), clust1=rep(Bus$cluster, each=nrow(Bus)), clust2=rep(Bus$cluster, nrow(Bus)))
    DT <- DT[DT$clust1==DT$clust2, ]
    
#9 - Create a list (length = interval) that contains empty matrices that have space for counting the number of ties for each Bus that satisfy each threshhold
    results <- list()
    df <- data.frame(ID=Bus$id)
    clusters <- sort(as.numeric(unique(Bus$cluster)))
    cluststr  <-  data.table(
      cluster = rep(clusters, length(Interv) - 1), 
      Interval = rep(Interv[-length(Interv)], each = length(clusters)), 
      modularity = 0, 
      clusters = 0, 
      isolates = 0, 
      clustercoef = 0, 
      alive = 0, 
      dissolved = 0, 
      founded = 0, 
      distance = 0, 
      compression = 0, 
      names = list()
    )
    
#10 - Create visual textual network function
    VisTextNet  <-  function(text_network, viz=NULL, alpha = .25, label_degree_cut=0, betweenness=FALSE){
      if (is.null(V(text_network)$name)){
        text_network  <-  set_vertex_attr(text_network, "name", value = as.character(1:vcount(text_network)))
      }
      #create network backbone
        e  <-  cbind(igraph::as_data_frame(text_network)[, 1:2 ], weight = E(text_network)$weight)
      # in
        w_in  <-  igraph::graph.strength(text_network, mode = "in")
        w_in  <-  data.frame(to = names(w_in), w_in, stringsAsFactors = FALSE)
        k_in  <-  igraph::degree(text_network, mode = "in")
        k_in  <-  data.frame(to = names(k_in), k_in, stringsAsFactors = FALSE)
        e_in  <-  e %>%
          left_join(w_in, by = "to") %>%
          left_join(k_in, by = "to") %>%
          mutate(alpha_in = (1-(weight/w_in))^(k_in-1))
      # out
          w_out  <-  igraph::graph.strength(text_network, mode = "out")
          w_out  <-  data.frame(from = names(w_out), w_out, stringsAsFactors = FALSE)
          k_out  <-  igraph::degree(text_network, mode = "out")
          k_out  <-  data.frame(from = names(k_out), k_out, stringsAsFactors = FALSE)
          e_out  <-  e %>%
            left_join(w_out, by = "from") %>%
            left_join(k_out, by = "from") %>%
            mutate(alpha_out = (1-(weight/w_out))^(k_out-1))
          e_full  <-  left_join(e_in, e_out, by = c("from", "to", "weight"))
          e_full  <-  e_full %>%
            mutate(alpha = ifelse(alpha_in < alpha_out, alpha_in, alpha_out)) %>%
            dplyr::select(from, to, alpha)
          E(text_network)$alpha  <-  e_full$alpha
          pruned  <-  igraph::delete.edges(text_network, which(E(text_network)$alpha >= alpha))
      # make degree for labelling most popular nodes
          V(pruned)$degree  <-  igraph::degree(pruned)
      # remove isolates
          isolates  <-  V(pruned)[degree(pruned)==0]
          # pruned  <-  delete.vertices(pruned, isolates)
      # calculate modularity for coloring
          communities  <-  cluster_louvain(as.undirected(pruned))
          V(pruned)$modularity <- communities$membership
          isolates2  <-  sum(data.frame(table(communities$membership))$Freq==2)
      #produce stats or info
          if(is.null(viz)) viz="none"
          if(viz=="none") out <- list(Modularity=modularity(as.undirected(pruned), V(pruned)$modularity), transitivity=transitivity(pruned), clusters=length(unique(communities$membership)), isolates=length(isolates), isolates2=isolates2, comps=components(pruned)$no)
          if(viz=="orig") out <- plot(text_network, vertex.color=V(pruned)$modularity, vertex.label=NA)
          if(viz=="colors") out  <-  communities$membership
          if(viz=="layout") out  <-  layout.fruchterman.reingold(pruned)
      return(out)
    }
    
    
#11 - Create function for finding best alpha
  findalpha <- function(x){
    df <- list()
    alphas <- (c(1:60)/100)
    # alphas <- (c(5:35)/100)
    for(i in 1:length(alphas)){
      df[[i]] <- VisTextNet(x, alpha = alphas[i], viz="none")
    }
    df2 <- data.frame(alph=alphas, iso=sapply(df, function(x) x$isolates), iso2=sapply(df, function(x) x$isolates2), clus=sapply(df, function(x) x$clusters), mod=sapply(df, function(x) x$Modularity), comps=sapply(df, function(x) x$comps))
     #alternate based on directed graph formula:
        df2$clus3 <- 
          df2$mod-
          1000000*df2$comps
  
    alph <- df2$alph[df2$clus3==max(df2$clus3)][1]
    return(alph)
  }

#12 - Create density dependence info and other traits
  for(k in 1:(length(Interv)-1)){
    index <- Interv[k]
    #a - subset to those open during interval
        tempDT <- DT[DT$close1>=index & DT$close2>=index & DT$open1<=index & DT$open2<=index, ]
    #b - for each census tract cluster...
      for(j in 1:length(clusters)){
        #i - subset data to cluster
            tempDT2 <- tempDT[tempDT$clust1==j]
            if(nrow(tempDT2)<=100) next()
        #ii - cluster new dataset
            atr3 <- matrix(tempDT2$Atr2, nrow=sqrt(nrow(tempDT2)))
            geomat <- 1/atr3
            geomat2 <- atr3
            geomat <- 1/atr3
            diag(geomat) <- 0
            geomat[is.infinite(geomat)] <- max(geomat[!is.infinite(geomat)])
            geomat[geomat>quantile(geomat, .99)] <- quantile(geomat, .99)*1.5
        #iii - prep some more, such that orgs are positioned closer to places they are most similar to.
            perc.rank  <-  function(x) trunc(rank(x))/length(x)
            tempmat <- atr3
            tempmat <- apply(tempmat, 2, perc.rank)
            geomat2 <- 1/tempmat
            geomat2[is.infinite(geomat2)] <- 0
            a <- rowSums(geomat2)/min(rowSums(geomat2))
            b <- apply(geomat2, 2, function(x) x/(a))
            geomat2 <- b
            N <- nrow(geomat2)
            cutoff <- nrow(geomat2)*5
            cut <- sort(c(geomat2), decreasing = T)[cutoff]
            geomat2[geomat2<cut] <- 0
            diag(geomat2) <- 0
            geomat3 <- (geomat2)*(geomat+max(geomat))
            g <- graph_from_adjacency_matrix(geomat3, weighted = T, mode="directed")
        #iv - Text net modularity and clustercoef
            temp <- VisTextNet(g, alpha = findalpha(g))
            cluststr[index==Interval & cluster==j, modularity:=round(temp[[1]], 5)]
            cluststr[index==Interval & cluster==j, clustercoef:=round(temp[[2]], 5)]
            cluststr[index==Interval & cluster==j, clusters:=round(temp[[3]], 5)]
            cluststr[index==Interval & cluster==j, isolates:=round(temp[[4]], 5)]
        #v - get other key stats
            #number alive
                cluststr[index==Interval & cluster==j, alive:=round(length(unique(tempDT2$ID)), 5)]
            #number dissolved
                cluststr[index==Interval & cluster==j, dissolved:=round(sum(tempDT2$close1[!duplicated(tempDT2$ID)]==index), 5)]
            #number founded
                cluststr[index==Interval & cluster==j, founded:=round(sum(tempDT2$open1[!duplicated(tempDT2$ID)]==index), 5)]
            #mean physical distance of businesses
                cluststr[index==Interval & cluster==j, distance:=round(mean(tempDT2$Dist), 5)]
            #mean similarity of businesses (niche compression)
                cluststr[index==Interval & cluster==j, compression:=round(mean(tempDT2$Atr2), 5)]
            #Bus names
                cluststr[index==Interval & cluster==j, names:=list(list(unique(tempDT2$ID)))]
                setTxtProgressBar(txtProgressBar(min = 0, max = length(clusters), style = 3), j)
        }
}
  
#13 - Save results              
    saveRDS(cluststr, paste(Directory, City, "_cluststr.rds", sep=""))

}
