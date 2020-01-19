#' 4 - Limit data to restaurants
#' @description This subsets the dataset to places with the supercategories restaurants, nightlife, and food. Should be put in a new directory.
#' @param Directory Directory of input files.
#' @param outDirectory Directory of output files
#' @export
#' @import data.table
#' @details 
#' This script does the following:
#' 0 - Set Directory  
#' 1 - Read Data
#' 2 - Subset data
#' 3 - Save RDS
#' 4 - Remove foreign cities
#' 5 - Remove meaningless categories
#' 6 - Save

yelp_Subset_Restaurants  <- function(Directory, outDirectory=NULL){
    #0 - Set Directory
        setwd(Directory)
  
    #1 - Read Data
        Bus <- readRDS("BusinessData.rds")
    
    #2 - Subset Data
        #Limit sample to just restaurants
            Bus <- Bus[!is.na(sapply(Bus$cat, function(x) any(match(x, "Restaurants")))), ]
        #Limit Sample to businesses alive after 2010
            Bus <- Bus[Bus$date_close>=as.Date("2010-01-01, 1, 0"), ]

    #3 - Get rid of cities outside US for the time being            
        Bus <- Bus[Bus$city_super!="Montreal" & Bus$city_super!="Stuttgart" & Bus$city_super!="Edinburgh" & Bus$city_super!="Toronto" & Bus$city_super!="Calgary", ]

    #4 - Remove businesses associated with categories that are not just restaurants or in complete competition with others (e.g. hotels, golf courses, book stores, etc.)        
        kill <- c("Adult Entertainment", "Airports", "Amateur Sports Teams", "Amusement Parks", "Appliances", "Arcades", "Automotive", "Beauty & Spas", "Bed & Breakfast", "Books", "Bookstores", "Butcher", "Candy Stores", "Chocolatiers & Shops", "Cinema", "Comedy Clubs", "Community Service/Non", "Cooking Classes", "Cooking Schools", "Convenience Stores", "Cosmetics & Beauty Supply", "Country Clubs", "Custom Cakes", "Day Spas", "Department Stores", "Doctors", "Drugstores", "Dry Cleaning & Laundry", "Education", "Ethic Grocery", "Ethnic Grocery", "Farmers Market", "Festivals", "Financial Services", "Fitness & Instruction", "Florists", "Furniture Stores", "Gas & Service Stations", "Golf", "Grocery", "Gyms", "Hair Salons", "Health & Medical", "Health Markets", "Home & Garden", "Home Decor", "Home Services", "Hotels", "Hotels & Travel", "Kids Activities", "Kitchen & Bath", "Landmarks & Historical Buildings", "Local Services", "Mags", "Massage", "Museums", "Music & Video", "Nail Salons", "Nutritionists", "Organic Stores", "Outlet Stores", "Personal Chefs", "Pets", "Playgrounds", "Pretzels", "Professional Services", "Real Estate", "Recreation Centers", "Resorts", "Social Clubs", "Specialty Schools", "Sporting Goods", "Sports Clubs", "Swimming Pools", "Tours", "Toy Stores", "Transportation", "Travel Services", "Wedding Planning", "Wholesale Stores", "Wigs", "Women's Clothing")
        Bus <- Bus[is.na(sapply(Bus$cat, function(x) any(match(x, kill)))), ] 
        #Remove businesses with extremely uncommon categories
            temp <- as.data.frame(table(unlist(Bus$cat)))
            kill <- temp[temp$Freq<5, ]$Var1
            Bus <- Bus[is.na(sapply(Bus$cat, function(x) any(match(x, kill)))), ]
            
    #5 - Kill meaningless vars in the data itself
        kill <- c("Active Life", "Beer Bar", "Beer Garden", "Chocolatiers & Shops", "Do-It-Yourself Food", "Event Planning & Services", "Health & Medical", "Health Markets", "Home & Garden", "Hotels & Travel", "Jazz & Blues", "Local Services", "Party & Event Planning", "Pasta Shops", "Performing Arts", "Seafood Markets", "Shopping Centers", "Venues & Event Spaces")
        Bus$cat <- Bus$cat
        Bus$cat <- sapply(Bus$cat, setdiff, kill)
        
    #6 - Save RDS
        if(!is.null(outDirectory)){
            setwd(outDirectory)
        }
            saveRDS(Bus, "Restaurants.rds")
        
            
}

