#' 17 - Estimate tie strength given a null model
#' @import data.table
#' @description 

yelp_tie_strength <- function(Directory, City, Intervals=NULL, AtrMax=NULL){
   #1 - Load data
       setwd(Directory)
       Bus <- readRDS("Restaurants.rds")
       Bus <- Bus[Bus$city_super==City, ]
       Cust <- readRDS(paste(City, "_cust.rds", sep=""))
       Dist <- readRDS(paste(City, "_dist.rds", sep=""))
   
   #2 - Run null models
       #Create two columns with bus id names, with a cell for each tie between ego and alter
           #estimate number of ties between each ego and alter
               DT <- data.table(Cust, Bus1=rep(Bus$id, each=nrow(Bus)), Bus2=rep(Bus$id, nrow(Bus)))
               DT2 <- DT[DT$Bus1!=DT$Bus2, ]
               DT2 <- DT2[, .(Cust=sum(Cust)), by=list(Bus1)]
           #create an index for ego names (to save space)
               index <- factor(1:nrow(Bus))
               nameindex <- DT2$Bus1
               DT2$Bus1 <- index
           #replicate ego id by Cust 
               a <- suppressWarnings(DT2[, .(ID=rep(Bus1, Cust), by=Bus1)])
               a$by <- NULL
               a$ID2 <- a$ID
           #Create an empty DT to store results within
               #Results <- data.table(DT$Cust, ID=rep(index, each=nrow(Bus)), ID2=rep(index, nrow(Bus)))
               Results <- data.table(ID=rep(index, each=nrow(Bus)), ID2=rep(index, nrow(Bus)))
           #Create an empty path to put results in - MUST RUN R AS ADMINISTRATOR
               unlink(paste(Directory, "/temp", sep=""), recursive = T)
               dir.create(paste(Directory, "/temp", sep=""))
           #Create function to create a column to split data.tables by
               chunks <- function(x){
                 chunk  <-  1000000
                 n  <-  nrow(x)
                 rep(1:ceiling(n/chunk), each=chunk)[1:n]
               }
           #set seed
               set.seed(1234)
           #do the following many times:
               for(i in 1:100){
               #randomly distribute ties, keeping degrees constant
                   a$ID2 <- sample(a$ID2)
                   a$ID2[a$ID==a$ID2] <- sample(a$ID2[a$ID==a$ID2])
               #get tie counts among each pair
                   b <- a[, .(Count=.N), by=list(ID, ID2)]
               #order results
                   Results2 <- merge(Results, b, by=c("ID", "ID2"), all=T)
                   Results2 <- data.table(Results2$Count)
               #convert NAs to 0s
                   Results2$V1 <- ifelse(is.na(Results2$V1), 0, Results2$V1)
               #split into 1, 000, 000 element pieces
                   Results2$chunks <- chunks(Results2)
                   Results2 <- split(Results2, Results2$chunks)
               #save components of results2 iteratively
                   for(j in 1:length(Results2)){
                       tempdir <- paste(Directory, "/temp/", j, "-", i, ".rds", sep="")
                       saveRDS(Results2[[j]], tempdir)    
                   }
                   setTxtProgressBar(txtProgressBar(max = 100, style = 3), i)
               }
   
   #3 - estimate null model results
       #find file names in directory
           files <- list.files(paste(Directory, "/temp", sep=""))
       #determine first number in each file name (this indicates chunk, second number indicates iteration)
           a <- strsplit(files, "-")
           files2 <- sapply(a, `[[`, 1)
           files2 <- as.numeric(files2)
           b <- vector()
       #iteratively read the saved files
           for(i in 1:length(unique(files2))){
             files3 <- files[files2==i]
             for(j in 1:100){
               if(j==1){
                 temp <- readRDS(paste(Directory, "temp/", sep="", files3[1]))
                 dt <- data.table(temp$V1)
               }
               else{
                 temp <- readRDS(paste(Directory, "temp/", sep="", files3[1]))
                 dt <- cbind(dt, temp$V1)
               }
             }
             a <- rowMeans(dt[, 2:length(dt)])
             b <- c(b, a)
             setTxtProgressBar(txtProgressBar(max = length(unique(files2)), style = 3), i)
           }
   
   
   #4 - further alter results
       #Average the expected values among upper and lower triangles.
           bm <- matrix(b, nrow=sqrt(length(b)))
           cm <- (t(bm)+bm)/2
       #create datatable of results
           Results2 <- data.table(Observed=DT$Cust, Expected=c(cm))
       #set self ties to 0 for observed/expected
           Results2$Observed[DT$Bus1==DT$Bus2] <- 1
           Results2$Expected[DT$Bus1==DT$Bus2] <- 1
       #add one to all observations and expectations
           Results2$Observed <- 1+Results2$Observed
           Results2$Expected <- 1+Results2$Expected
       #Estimate difference between observed and expected
           Results2$Proportion <- Results2$Observed/Results2$Expected
           
       #normalize results
           #log them
               Results2$Proportion <- log(Results2$Proportion)
           #set max and min as 1 and -1 times 95th percentile 
               Results2$Proportion <- ifelse(Results2$Proportion<quantile(Results2$Proportion, .05), quantile(Results2$Proportion, .05), Results2$Proportion)
               Results2$Proportion <- ifelse(Results2$Proportion>-quantile(Results2$Proportion, .05), -quantile(Results2$Proportion, .05), Results2$Proportion)
           #subtract mean from number; reset max (mean will almost be 0); do this a few times
               for(i in 1:5){
                   Results2$Proportion <- Results2$Proportion-mean(Results2$Proportion)
                   Results2$Proportion <- ifelse(Results2$Proportion>-min(Results2$Proportion), -min(Results2$Proportion), Results2$Proportion)
               }
           #scale to 1 and -1
               range01  <-  function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))*2-1}
               Results2$Proportion <- range01(Results2$Proportion)
   
   #5 - Save RDS
       saveRDS(Results2, paste(Directory, City, "_nullties.rds", sep=""))
}