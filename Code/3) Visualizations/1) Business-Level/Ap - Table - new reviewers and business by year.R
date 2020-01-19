#1 - load data
    setwd(prep$baseDir)
    Bus<-readRDS("BusinessData.rds")

#2 - Subset Data
    #Limit sample to just restaurants
        Bus<-Bus[!is.na(sapply(Bus$cat,function(x) any(match(x,"Restaurants")))),]

#3 - get rid of cities outside US for the time being
    Bus<-Bus[Bus$city_super!="Montreal" & Bus$city_super!="Stuttgart" & Bus$city_super!="Edinburgh" & Bus$city_super!="Toronto" & Bus$city_super!="Calgary",]

#4 - Remove businesses associated with categories that are not just restaurants or in complete competition with others (e.g. hotels, golf courses, book stores, etc.)
    kill<-c("Adult Entertainment","Airports","Amateur Sports Teams","Amusement Parks","Appliances","Arcades","Automotive","Beauty & Spas","Bed & Breakfast","Books","Bookstores","Butcher","Candy Stores","Chocolatiers & Shops","Cinema","Comedy Clubs","Community Service/Non","Cooking Classes","Cooking Schools","Convenience Stores","Cosmetics & Beauty Supply","Country Clubs","Custom Cakes","Day Spas","Department Stores","Doctors","Drugstores","Dry Cleaning & Laundry","Education","Ethic Grocery","Ethnic Grocery","Farmers Market","Festivals","Financial Services","Fitness & Instruction","Florists","Furniture Stores","Gas & Service Stations","Golf","Grocery","Gyms","Hair Salons","Health & Medical","Health Markets","Home & Garden","Home Decor","Home Services","Hotels","Hotels & Travel","Kids Activities","Kitchen & Bath","Landmarks & Historical Buildings","Local Services","Mags","Massage","Museums","Music & Video","Nail Salons","Nutritionists","Organic Stores","Outlet Stores","Personal Chefs","Pets","Playgrounds","Pretzels","Professional Services","Real Estate","Recreation Centers","Resorts","Social Clubs","Specialty Schools","Sporting Goods","Sports Clubs","Swimming Pools","Tours","Toy Stores","Transportation","Travel Services","Wedding Planning","Wholesale Stores","Wigs","Women's Clothing")
    Bus<-Bus[is.na(sapply(Bus$cat,function(x) any(match(x,kill)))),]
    #Remove businesses with extremely uncommon categories
        temp<-as.data.frame(table(unlist(Bus$cat)))
        kill<-temp[temp$Freq<5,]$Var1
        Bus<-Bus[is.na(sapply(Bus$cat,function(x) any(match(x,kill)))),]

#5 - kill meaningless vars in the data itself
    kill<-c("Active Life", "Beer Bar", "Beer Garden", "Chocolatiers & Shops", "Do-It-Yourself Food", "Event Planning & Services", "Health & Medical", "Health Markets", "Home & Garden", "Hotels & Travel", "Jazz & Blues", "Local Services", "Party & Event Planning", "Pasta Shops", "Performing Arts", "Seafood Markets", "Shopping Centers", "Venues & Event Spaces")
    Bus$cat<-Bus$cat
    Bus$cat<-sapply(Bus$cat,setdiff,kill)


#6 - Bus dates - set to numeric    
    datefun<-function(x) suppressWarnings(as.numeric(substr(x,1,4))+ as.numeric(substr(x,6,7))/12 + as.numeric(substr(x,9,10))/365)
    Bus$date_close<-sapply(Bus$date_close,function(x) max(datefun(x)))
    Bus$date_open<-sapply(Bus$date_open,function(x) min(datefun(x)))

#7 - find unique dates in sample
    udates<-data.frame(date=sort(unique(c(Bus$date_open,Bus$date_close))))
    udates$alive<-0
    udates$born<-0
    udates$died<-0
    udates$died2<-0
    for(i in 1:nrow(udates)){
        udates$alive[i]<-sum(Bus$date_open<=udates$date[i] & (Bus$date_close>=udates$date[i] | Bus$is_open==1) )
        udates$born[i]<-sum(Bus$date_open==udates$date[i])
        udates$died[i]<-sum(Bus$date_close==udates$date[i])
        udates$died2[i]<-sum(Bus$date_close==udates$date[i] & Bus$is_open==0)
    }  
    ggplot(udates, aes(x=date,y=alive)) + geom_smooth()
    ggplot(udates, aes(x=date,y=born)) + geom_smooth()
    ggplot(udates, aes(x=date,y=died)) + geom_smooth()
    ggplot(udates, aes(x=date,y=died2)) + geom_smooth()
    ggplot(udates[udates$date<2016,], aes(x=date,y=born)) + geom_smooth(span=.1)
    
#8 - annual growth rate
    Bus<-Bus2
    udates<-data.frame(date=2005+((1:30)/2))
    #for each date, count how many places opened that day and closed after that day
    udates$alive<-0
    udates$born<-0
    udates$died<-0
    udates$died2<-0
    for(i in 1:nrow(udates)){
      udates$alive[i]<-sum(Bus$date_open<=udates$date[i] & (Bus$date_close>=udates$date[i] | Bus$is_open==1) )
      udates$born[i]<-sum(Bus$date_open>=udates$date[i] & Bus$date_open<udates$date[i+1])
      if(i>1){
        udates$growth[i]<-(udates$alive[i]-udates$alive[i-1])/udates$alive[i-1]
        udates$died[i]<-sum(Bus$date_close<=udates$date[i] & Bus$date_close>udates$date[i-1])
        udates$died2[i]<-sum(Bus$date_close<=udates$date[i] & Bus$date_close>udates$date[i-1] & Bus$is_open==0)
      }
    }  
    udates$growth<-udates$growth
    udates<-udates[udates$date<2017,]
    
#9 - Plot growth rate    
    png(file="C:/Users/bda13/Desktop/Sample Growth rate.png", height=6, width=8.5,units = "in",res = 72)
    ggplot(udates, aes(x=date,y=growth)) + 
      geom_smooth(color="#1bc3e5",size=3) + 
      scale_y_continuous(labels=scales::percent,breaks = (0:15)/4)+
      labs(x="Year", y="% Growth")+
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=16),
        title =element_text(size=32), #, face='bold'
        plot.title = element_text(hjust = 1),
        legend.position="none",
        legend.text=element_text(size=31),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.title.y = element_text(size=20, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size=20, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        text=element_text(family="serif")
      )+
      scale_x_continuous(breaks = c(2006:2018))
    dev.off()
    
    
#10 - Age at death
    Bus<-Bus2
    Bus<-Bus[Bus$is_open==0 & Bus$age<x,]
    udates<-data.frame(date=2005+((1:30)/2))
    udates$age<-0
    for(i in 1:nrow(udates)){
      udates$agemed[i]<-median(Bus$age[Bus$date_open>=udates$date[i] & Bus$date_open<udates$date[i+1]])
      udates$agemean[i]<-mean(Bus$age[Bus$date_open>=udates$date[i] & Bus$date_open<udates$date[i+1]])
    }  
    udates[udates$date<2016.5-x/365,]
    ggplot(udates[udates$date<2016.5-x/365,], aes(x=date,y=agemed/365)) + geom_smooth()
    ggplot(udates[udates$date<2016.5-x/365,], aes(x=date,y=agemean/365)) + geom_smooth()

