#' 5 - Create attributes dataset
#' @description This function transform Yelp's attributes list into a dataframe
#' @param baseDir Directory of original Yelp files.
#' @param derDir Directory of derived Yelp files.
#' @export
#' @import data.table

yelp_Create_Attributes <- function(baseDir, derDir){
  
  #1) Load data
      setwd(baseDir)
      temp <- readRDS("BusinessData.rds")
      Atr <- temp$attributes
      setwd(derDir)
      Bus <- readRDS("Restaurants.rds")

  #2) Add ids to Atr and sort to match dfs
      Atr$business_id <- temp$business_id
      Atr <- Atr[match(Bus$business_id, Atr$business_id), ]
      Atr <- Atr[, order(names(Atr))]
      
  #3) Deal with problematic attributes
        annatr <- Atr[, c("Ambience", "BestNights", "BusinessParking", "DietaryRestrictions", "GoodForMeal", "Music")]
      #String split and unlist all attributes
          a <- unlist(strsplit(unlist(annatr), "[: , '{}]"))
      #Manually remove super attributes and blanks. This creates a vector that alternates between an attribute name and its corresponding characteristic
          a <- a[a!="" & a!="BusinessParking" & a!="Ambience" & a!="GoodForMeal" & a!="BestNights" & a!="Music" & a!="DietaryRestrictions" & a!="HairSpecializesIn"]
      #Create a vector that alternates between 1 and 2
          b <- rep(c(1, 2), length(a)/2)
      #Find unique attribute names
          Attrnames <- unique(a[b==1])
          Attrnames <- Attrnames[order(Attrnames)]

  #4) Reformat each column of problematic attributes with a string split function
      splitfunction <- function(x){
        if(!is.null(x)){
          strsplit(unlist(x), "[: , '{}]")
        }
      }
      Attr1 <- sapply(annatr[, 1], splitfunction)
      Attr2 <- sapply(annatr[, 2], splitfunction)
      Attr3 <- sapply(annatr[, 3], splitfunction)
      Attr4 <- sapply(annatr[, 4], splitfunction)
      Attr5 <- sapply(annatr[, 5], splitfunction)
      Attr6 <- sapply(annatr[, 6], splitfunction)

  #5) Reformat attribute list by removing super attributes and blanks, and unlisting objects within it
      removefunction <- function(x){
        if(!is.null(x)){
          a <- unlist(x)
          b <- a[a!="" & a!="BusinessParking" & a!="Ambience" & a!="GoodForMeal" & a!="BestNights" & a!="Music" & a!="DietaryRestrictions" & a!="HairSpecializesIn"]
        }
      }
      Attr1 <- sapply(Attr1, removefunction)
      Attr2 <- sapply(Attr2, removefunction)
      Attr3 <- sapply(Attr3, removefunction)
      Attr4 <- sapply(Attr4, removefunction)
      Attr5 <- sapply(Attr5, removefunction)
      Attr6 <- sapply(Attr6, removefunction)

  #6) Bind these attribute lists
      Attrs <- list()
      for(i in 1:length(Attr1)){
        temp <- c(Attr1[[i]], Attr2[[i]], Attr3[[i]], Attr4[[i]], Attr5[[i]], Attr6[[i]])
        Attrs[[i]] <- temp[!is.na(temp)]
        print(i)
      }

  #7) Reformat attribute list by making each item a vector of the responses to attrnames, ordered alphabetically; this syntax is hyper efficient
      emptymat <- matrix(ncol=2, nrow=length(Attrnames))
      dffunction <- function(x){
        if(!is.null(x)){
          b <- rep(c(1, 2), length(x)/2)
          missing <- setdiff(Attrnames, x[b==1])
          emptymat[, 1] <- c(x[b==1], missing)
          emptymat[, 2] <- c(x[b==2], rep(NA, length(missing)))
          emptymat <- emptymat[order(emptymat[, 1]), ]
          return(emptymat[, 2])
        }
        else{
          return(rep(NA, nrow(emptymat)))
        }
      }
      Attr <- lapply(Attrs, dffunction)

  #8) Convert list to data.table
      Attr  <-  data.frame(matrix(unlist(Attr), ncol=length(Attrnames), nrow=nrow(Bus), byrow=T), stringsAsFactors=FALSE)
      names(Attr) <- Attrnames

  #9) Bind to other attributes
      temp <- Atr
      temp[, c("Ambience", "BestNights", "BusinessParking", "DietaryRestrictions", "GoodForMeal", "Music")] <- NULL
      Atr <- cbind(temp, Attr)
      Atr[, is.na(names(Atr))] <- NULL

  #10) Save
      setwd(derDir)
      saveRDS(Atr, "Attributes.rds")
  
}

