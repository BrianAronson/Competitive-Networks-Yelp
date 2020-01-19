#' 10 - Create review categories
#' @description This function creates a series of new categories based on the occurrence of Yelp categories in Yelp Reviews
#' @export
#' @import data.table
#' @import cluster

yelp_Review_Cats_Function <- function(Directory, BaseDirectory){
  #1 - load data
      setwd(BaseDirectory)
      ReviewData <- readRDS("ReviewData(Full).rds")
      setwd(Directory)
      Bus <- readRDS("Restaurants.rds")
      saveRDS(Bus, "Restaurants2.rds")
      
  #2 - Prep data
      ReviewData <- ReviewData[ReviewData$business_id %in% Bus$id, ]
      
  #3 - Replace this with categories of interest
      cat <- as.data.frame(table(unlist(Bus$cat)))
      cat <- as.character(sort(cat$Var1))
  
  #4 - create categories to search for
      #place variables (variables where I append the "places" strings to their searches)
          placevars <- list("Afghan", "African", "american", "Arabian", "Argentine", "asian", c("Barbeque", "bbq"), "Basque", "Beer", "Belgian", "Brazilian", "breakfast", "British", "brunch", "Bubble Tea", "Burmese", "Cajun", "Cambodian", "Cantonese", "Caribbean", "Chinese", "Colombian", "Creole", "crepe", "Cuban", "Dominican", "Ethiopian", "european", "Falafel", "Filipino", c("Fish & Chips", "fish and chips"), "Fondue", "French", "Gelato", "German", "Greek", "Halal", "Hawaiian", "Himalayan", "Hungarian", "Indian", "Indonesian", "Iranian", "Irish", "Italian", "Japanese", "Korean", "Kosher", "Laotian", "latin", "Latin American", "Lebanese", "Malaysian", "Mediterranean", "Mexican", "Middle Eastern", "Mongolian", "Moroccan", "Nepalese", "Pakistani", "Pan Asian", "Persian", "Peruvian", "Pizza", "Poke", "Polish", "Portuguese", "Puerto Rican", "Ramen", "Russian", "Salad", "Salvadoran", "Seafood", "Shanghainese", "Shaved Ice", "Soup", "Southern", "Spanish", "sushi", "Szechuan", "Tacos", "Taiwanese", "Thai", "Turkish", "Ukrainian", "Vegan", "Vegetarian", "Venezuelan", "Vietnamese")
          pluralplacevars <- c("Waffles", "Wraps", "Sandwiches", "Hot Dogs", "Empanadas", "Bagels", "Donuts", "Burgers", "Cheesesteaks", "Chicken Wings", "Cupcakes", "Desserts")
          unpluralplacevars <- ifelse(substr(pluralplacevars, nchar(pluralplacevars), nchar(pluralplacevars))=="s", substr(pluralplacevars, 1, nchar(pluralplacevars)-1), pluralplacevars)
          unpluralplacevars[3] <- "Sandwich"
          placevars2 <- c(placevars, unpluralplacevars)
          placeindicators <- c("bar", "cuisine", "dishes", "experience", "food", "joint", "menu", "options", "place", "places", "restaurant", "restaurants", "spot")
          placevarslookup <- list()
          for(i in 1:length(placevars2)){
              placevarslookup[[i]] <- apply(expand.grid(placevars2[[i]], placeindicators), 1, function(x) paste(x[1], x[2], sep=" "))
          }
      #nonplace variables (those above without the place strings in search)
          bothpluralvars <- list()
          for(i in 1:length(pluralplacevars)){
            bothpluralvars[[i]] <- c(pluralplacevars[i], unpluralplacevars[i])
          }
          pluralizedandnormalplacevars <- list()
          for(i in 1:length(placevars)){
              pluralizedandnormalplacevars[[i]] <- c(placevars[[i]], paste(placevars[[i]], "s", sep=""))
          }
          nonplacevarslookup <- c(pluralizedandnormalplacevars, bothpluralvars)
          placevars <- placevars2
      #defined variables (categories that don't need a place string)
          definedvars <- c("Acai Bowls", "Art Galleries", "Asian Fusion", "Bakeries", "Bars", "Beer Gardens", "Bistros", "Brasseries", "Breweries", "Buffets", "Cafes", "Cafeteria", "Casinos", "Caterers", "Champagne Bars", "Cheese Shops", "Chicken Shop", "Cocktail Bars", "Coffee Roasteries", "Comfort Food", "Country Dance Halls", "Creperies", "Dance Clubs", "Delis", "Dim Sum", "Diners", "Dive Bars", "Ethnic Food", "Fast Food", "Food Court", "Food Stands", "Food Trucks", "Gastropubs", "Gay Bars", "Gluten-Free", "Hookah Bars", "Hot Pot", "Imported Food", "Internet Cafes", "Irish Pub", "Karaoke", "Local Flavor", "Lounges", "Macarons", "Meat Shops", "Music Venues", "Nightlife", "Noodle house", "Piano Bars", "Pool Halls", "Poutineries", "Pubs", "Restaurants", "Shopping", "Smokehouse", "Soul Food", "Speakeasies", "Specialty Food", "Sports Bars", "Steakhouses", "Street Vendors", "Sushi Bars", "Tapas Bars", "Tea Rooms", "Teppanyaki", "Tex-Mex", "Tobacco Shops", "Whiskey Bars", "Wine Bars", "Wine Tasting Room", "Wineries")
          nonpluraldefinedvars <- ifelse(substr(definedvars, nchar(definedvars), nchar(definedvars))=="s", substr(definedvars, 1, nchar(definedvars)-1), definedvars)
          nonpluraldefinedvars[which(definedvars=="Breweries")] <- "Brewery"
          nonpluraldefinedvars[which(definedvars=="Speakeasies")] <- "Speakeasy"
          nonpluraldefinedvars[which(definedvars=="Wineries")] <- "Winery"
          nonpluraldefinedvars[which(definedvars=="Bakeries")] <- "Bakery"
          nonpluraldefinedvars[which(definedvars=="Art Galleries")] <- "Art Gallery"
          definedvarslookup <- list()
          for(i in 1:length(definedvars)){
            temp <- definedvars[i]==nonpluraldefinedvars[i]
            if(temp==T){
                definedvarslookup[[i]] <- definedvars[i]
            }else{
                definedvarslookup[[i]] <- c(definedvars[i], nonpluraldefinedvars[i])
            }
          }
          definedvarslookup[[which(definedvars=="Tex-Mex")]] <- c("Tex-Mex", "tex mex", "texmex")
      #attribute variables (variables based on yelp attributes)
          attributevars <- list("breakfast", "brunch", "casual", "Caters", "crowded", c("delivery", "delivers"), "dinner", "divey", c("dog", "dogs"), "outdoor seating", "drive through", "early", "hipster", "intimate", "kids", "late", "lunch", "quiet", "parking lot", "popular", "romantic", c("take out", "carry out"), c("tourist", "tourists", "touristy"), "trendy", "TV", "upscale", "valet", "fancy", c("cheap", "bargain", "inexpensive"), c("chain", "chains"), c("dancing", "dance"), c("classy", "formal"), c("WiFi", "internet"), c("noisey", "noisy", "loud"), c("reservations", "reservation"), c("expensive", "rip off", "pricey"), c("table service", "waiter", "wait staff"))
          attributevarslookup <- attributevars
      #custom variables (variables based on my intuition)
          customvars <- list("custom", "shop", "noodles", "chic", "authentic", "ethnic", "fusion", "healthy", "hookah", "imported", "local", "modern", "pool", "raw", "spirits", "sushi", "traditional", "juice bar", "bowling", c("vegetable", "vegetables", "veggies"), c("tea", "teas"), c("steak", "steaks"), c("smoothy", "smoothies"), c("pasta", "pastas"), c("juice", "juices"), c("ice cream", "icecream", "frozen yogurt", "froyo"), c("fruit", "fruits"), c("crepe", "crepes"), c("coffee", "coffees"), c("cocktail", "cocktails"), c("jazz", "blues", "live music", "music venue", "entertainment", "performance"), c("sports", "football", "baseball", "basketball", "hockey", "tennis", "soccer", "olympics"), c("fish", "lobster", "tilapia", "salmon", "tuna", "flounder", "fluke", "crab", "clam", "oyster", "squid", "octopus", "cod", "catfish"), "art", "tapas")
          customvarslookup <- customvars
          vars <- list(placevars, definedvars, attributevars, customvars)
          names(vars) <- c("placevars", "definedvars", "attributevars", "customvars")
      #save
          saveRDS(vars, "vars.rds")
              
  #5 - Finalize search criteria
      #convert all searches to lower case
          placevarslookup <- lapply(placevarslookup, tolower)
          nonplacevarslookup <- lapply(nonplacevarslookup, tolower)
          definedvarslookup <- lapply(definedvarslookup, tolower)
          attributevarslookup <- lapply(attributevarslookup, tolower)
          customvarslookup <- lapply(customvarslookup, tolower)
      #convert to regular expression that searches for these words and their alternatives
          prepfun <- function(x){
              paste("\\b", x, "\\b", sep="", collapse="|")
          }
          placevarslookup <- sapply(placevarslookup, prepfun)
          nonplacevarslookup <- sapply(nonplacevarslookup, prepfun)
          definedvarslookup <- sapply(definedvarslookup, prepfun)
          attributevarslookup <- sapply(attributevarslookup, prepfun)
          customvarslookup <- sapply(customvarslookup, prepfun)
      #cut placevarslookups into smaller pieces
          placevarslookup1 <- placevarslookup[1:round(length(placevarslookup)/4, 0)]
          placevarslookup2 <- placevarslookup[round(length(placevarslookup)/4+1, 0):round(length(placevarslookup)*2/4, 0)]
          placevarslookup3 <- placevarslookup[(round(length(placevarslookup)*2/4, 0)+1):round(length(placevarslookup)*3/4, 0)]
          placevarslookup4 <- placevarslookup[(round(length(placevarslookup)*3/4, 0)+1):length(placevarslookup)]
      #create list version
          lookups <- list(placevarslookup1, placevarslookup2, placevarslookup3, placevarslookup4, nonplacevarslookup, definedvarslookup, attributevarslookup, customvarslookup)
          
  #6 - prep review data
      #Make all reviews in lower case
          ReviewDatatext <- tolower(ReviewData$text)
          
  #7 - Search lookup criteria in reviews
      #For each search criteria, find whether search terms were in a given review
          system.time({
            Cores=detectCores()
            registerDoParallel(cores=Cores)
            matrices <- foreach(i=1:length(lookups)) %dopar%{
              sapply(lookups[[i]], grepl, ReviewDatatext)
            }
          })
    
  #8 - revert matrices 1:4 back into one matrix
      temp <- matrices[c(1, 5:8)]
      temp[[1]] <- cbind(matrices[[1]], matrices[[2]], matrices[[3]], matrices[[4]])      
      matrices <- temp
  
  #9 - name matrices and dimensions
      #matrix names
          names(matrices) <- c("placevars", "nonplacevars", "definedvars", "attributevars", "customvars")
      #dimension (column) names
          dimnames(matrices[[1]]) <- list(NULL, paste(sapply(placevars, "[[", 1), 1, sep=""))
          dimnames(matrices[[2]]) <- list(NULL, paste(sapply(placevars, "[[", 1), 2, sep=""))
          dimnames(matrices[[3]]) <- list(NULL, definedvars)
          dimnames(matrices[[4]]) <- list(NULL, sapply(attributevars, "[[", 1))
          dimnames(matrices[[5]]) <- list(NULL, sapply(customvars, "[[", 1))
        
  #10 - Save copy of results
      saveRDS(matrices, "reviewdf.rds")
      matrices <- readRDS("reviewdf.rds")
  
  #11 - Create a large version of matrices
      matrices2 <- cbind(matrices[[1]], matrices[[2]], matrices[[3]], matrices[[4]], matrices[[5]])
  
  #12 - Count occurrence by Business
      matrices2 <- as.data.table(matrices2)
      matrices2$id <- ReviewData$business_id
      matrices2 <- matrices2[, lapply(.SD, sum, na.rm=TRUE), by=id ]
      #Reorder to match Bus
          matrices2 <- matrices2[order(match(matrices2$id, Bus$id)), ]
      #kill id
          matrices2$id <- NULL
  
  #13 - Estimate occurrences divided by reviews 
      #Create temporary matrix
          matrices2 <- as.data.frame(matrices2)
          propmat <- matrices2
          for(m in 1:(length(propmat))){
            propmat[, m] <- propmat[, m]/Bus$rev_count
          }
     
  #14 - Save results
      saveRDS(propmat, "propmat.rds")
}
