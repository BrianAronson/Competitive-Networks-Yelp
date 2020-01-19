#Get smoother and wider hull positions
  library(sf)
  library(smoothr)
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
            hulldf[,3]<-factor(hulldf[,3])
            names(hulldf)[3]<-names(df)[3]
        }
    #smooth the edges substantially
        if(ncol(df>2)){
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
            newhulldf[,3]<-factor(newhulldf[,3])
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
  
ggplot()+
    geom_encircle(data=hulldf,aes(x=x,y=y,fill=colors),alpha=1,expand=0,colour="black")+
    geom_encircle(data=hulldf,aes(x=x,y=y,group=colors,color="black"),alpha=1,expand=0,colour="black",size=2)+
    geom_point(data=df[df$colors>0,],aes(x=V1,y=V2),size=2,fill="black",shape=21)+ #color=factor(colors),fill=factor(colors)+
    geom_segment(data = edg,aes(x = xbeg, y = ybeg, xend = xend, yend = yend), size=1)+
    theme_void()
    

  ggplot()+geom_convexhull(data=hulldf,aes(x=x,y=y,fill=colors))
  ggplot()+geom_convexhull(data=df,aes(x=V1,y=V2,fill=factor(colors)))
  ggplot()+geom_convexhull(data=newcorners,aes(x=x,y=y))
  
