#0) NOTES: This is to grab information of restaurants around Chubby's, Durham

#1) load packages
    require(tidyverse)
    require(httr)
    library(data.table)
    
#2) input token
    token <- ""

#3) Assign search criteria
    yelp <- "https://api.yelp.com"
    term <- "restaurants"
    location <- "Durham, NC"
    limit <- 50
    radius <- round(.5 / 0.000621371) #miles to meters
    url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                      query = list(term = term, longitude = -78.9228318, latitude=36.0092557, 
                                   limit = limit,
                                   radius = radius))
    

#4) Make search
    res <- GET(url, add_headers('Authorization' = paste("bearer", token)))

#5) Format results
    results <- content(res)
    yelp_httr_parse <- function(x) {
      parse_list <- list(id = x$id, 
                         name = x$name, 
                         stars = x$rating, 
                         review_count = x$review_count, 
                         latitude = x$coordinates$latitude, 
                         longitude = x$coordinates$longitude, 
                         address1 = x$location$address1, 
                         city = x$location$city, 
                         state = x$location$state, 
                         distance = x$distance,
                         is_closed=x$is_closed,
                         price=x$price,
                         categories=x$categories,
                         transactions=x$transactions,
                         url=x$url)
      
      Nulls<-which(sapply(parse_list,is.null))
      if(length(Nulls)>0){
        for(i in 1:length(Nulls)){
          temp<-Nulls[i]
          parse_list[temp]<-NA
        }
      }

      df <- data.table(id=parse_list$id,
                       name=parse_list$name, 
                       stars = parse_list$stars, 
                       review_count = parse_list$review_count, 
                       latitude=parse_list$latitude, 
                       longitude = parse_list$longitude, 
                       address1 = parse_list$address1, 
                       city = parse_list$city, 
                       state = parse_list$state, 
                       distance= parse_list$distance,
                       is_closed=parse_list$is_closed,
                       price=parse_list$price,
                       transactions=list(parse_list$transactions),
                       categories=list(unlist(parse_list$categories)),
                       url=parse_list$url
                       )
      return(df)
    }
    results_list <- lapply(results$businesses, FUN = yelp_httr_parse)
    payload <- do.call("rbind", results_list)
    Bus.dur.open<-data.table(payload)
    Bus.dur.open[,1:13]
    
#6) shorten urls
    url2<-vector()
    for(i in 1:length(Bus.dur.open$url)){
      url<-Bus.dur.open$url[i]
      url2[i]<-str_sub(url,1,gregexpr(pattern ='?adjust_creative=',url)[[1]][1]-2)
    }
    Bus.dur.open$url<-url2


#7) Assign basic info for closed places (manually)
    closednames<-c("Chubby's Tacos",
                   "Charlie's Neighborhood Bar & Grille",
                   "The Pie Hole - Durham",
                   "Epachamo",
                   "Mitch's Bar and Grille",
                   "George's Garage",
                   "Vita",
                   "Baba Ghannouj")
    closedaddresses<-c("748 9th St",
                       "758 9th St",
                       "810 9th St",
                       "730 9th St",
                       "730 9th St",
                       "737 9th St",
                       "Erwin Square",
                       "2200 W Main St")
    urls<-c("https://www.yelp.com/biz/chubbys-tacos-durham",
            "https://www.yelp.com/biz/charlies-neighborhood-bar-and-grille-durham",
            "https://www.yelp.com/biz/the-pie-hole-durham-durham",
            "https://www.yelp.com/biz/epachamo-durham",
            "https://www.yelp.com/biz/mitchs-bar-and-grille-durham",
            "https://www.yelp.com/biz/georges-garage-durham",
            "https://www.yelp.com/biz/vita-durham",
            "https://www.yelp.com/biz/baba-ghannouj-durham")

    
#8) Use yelp api for these places
    results<-list()
    url<-vector()
    for( i in 1: length(closednames)){
        url <- modify_url(yelp, path = c("v3", "businesses", "matches"),
                          query = list(name=closednames[i],address1=closedaddresses[i],
                                       city="Durham",state="NC",country="US"))
        res <- GET(url, add_headers('Authorization' = paste("bearer", token)))
        results[[i]] <- content(res)$businesses[[1]]    
    }
    
#9) format results
    results_list2 <- lapply(results, FUN = yelp_httr_parse)
    payload <- do.call("rbind", results_list2)
    Bus.dur.closed<-data.table(payload)
    Bus.dur.closed$url<-urls
    Bus.dur.closed$is_closed<-T
    Bus.dur.closed[,1:13]
    
#10) bind restaurants
    Bus.dur<-rbind(Bus.dur.open,Bus.dur.closed)
    Bus.dur[,1:13]
    Bus.dur[40,1:13]

