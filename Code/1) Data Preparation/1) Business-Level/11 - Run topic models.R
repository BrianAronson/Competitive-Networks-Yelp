#' 11 - Topic Models
#' @description This function creates variables based on topic models of common food terms in Yelp reviews.
#' @export
#' @import data.table tm topicmodels
#' @import cluster

yelp_topic_models <- function(){
for(i in 1:length(prep$Cities)){
    print(i)
    City <- prep$Cities[i]
  
  #0 - Set Directory
      setwd(prep$derDir)
      Bus <- readRDS("Restaurants.rds")
      setwd(prep$baseDir)
  
  #1 - Read Data
      Rev <- readRDS("ReviewData(Full).rds")
      setwd(prep$derDir)
  
  #2 - Subset data
      Bus <- Bus[Bus$city_super==City, ]
      Rev <- Rev[Rev$business_id %in% Bus$id, ]
      Rev <- Rev[!is.na(Rev$business_id), ]
      
  #3 - Set terms to use in topic models
      good <- c("acorn", "alfalfa", "almond", "anchovy", "anise", "appetite", "appetizer", "apple", "apricot", "artichoke", "asparagus", "aspic", "ate", "avocado", "bacon", "bagel", "bake", "baked", "bamboo", "banana", "barbecue", "barley", "basil", "batter", "beancurd", "beans", "beef", "beet", "bell", "berry", "biscuit", "bitter", "black", "black", "black-eyed", "blackberry", "bland", "blood", "blueberry", "boil", "bowl", "boysenberry", "bran", "bread", "breadfruit", "breakfast", "brisket", "broccoli", "broil", "brown", "brownie", "brunch", "Brussels", "buckwheat", "buns", "burrito", "butter", "butter", "cake", "calorie", "candy", "candy", "cantaloupe", "capers", "caramel", "caramel", "carbohydrate", "carrot", "cashew", "cassava", "casserole", "cater", "cauliflower", "caviar", "cayenne", "celery", "cereal", "chard", "cheddar", "cheese", "cheesecake", "chef", "cherry", "chew", "chick", "chicken", "chili", "chips", "chives", "chocolate", "chopsticks", "chow", "chutney", "cilantro", "cinnamon", "citron", "citrus", "clam", "cloves", "cobbler", "coconut", "cod", "coffee", "coleslaw", "collard", "comestibles", "cook", "cookbook", "cookie", "corn", "cornflakes", "cornmeal", "cottage", "crab", "crackers", "cranberry", "cream", "cream", "crepe", "crisp", "crunch", "crust", "cucumber", "cuisine", "cupboard", "cupcake", "curds", "currants", "curry", "custard", "daikon", "daily", "dairy", "dandelion", "Danish", "dates", "dessert", "diet", "digest", "digestive", "dill", "dine", "diner", "dinner", "dip", "dish", "dough", "doughnut", "dragonfruit", "dressing", "dried", "drink", "dry", "durian", "eat", "Edam", "edible", "egg", "eggplant", "elderberry", "endive", "entree", "fast", "fat", "fava", "feast", "fed", "feed", "fennel", "fig", "fillet", "fire", "fish", "flan", "flax", "flour", "food", "food", "foodstuffs", "fork", "freezer", "French", "fried", "fritter", "frosting", "fruit", "fry", "garlic", "gastronomy", "gelatin", "ginger", "ginger", "gingerbread", "glasses", "Gouda", "grain", "granola", "grape", "grapefruit", "grated", "gravy", "green", "green", "greens", "grub", "guacamole", "guava", "gyro", "halibut", "ham", "hamburger", "hash", "hazelnut", "herbs", "honey", "honeydew", "horseradish", "hot", "hot", "hot", "hummus", "hunger", "hungry", "ice", "ice", "ice", "iceberg", "iced", "icing", "jackfruit", "jalapeño", "jam", "jelly", "jellybeans", "jicama", "jimmies", "Jordan", "jug", "juice", "julienne", "junk", "kale", "kebab", "ketchup", "kettle", "kettle", "kidney", "kitchen", "kiwi", "knife", "kohlrabi", "kumquat", "ladle", "lamb", "lard", "lasagna", "legumes", "lemon", "lemonade", "lentils", "lettuce", "licorice", "lima", "lime", "liver", "loaf", "lobster", "lollipop", "loquat", "lox", "lunch", "lunch", "lunchmeat", "lychee", "macaroni", "macaroon", "main", "maize", "mandarin", "mango", "maple", "margarine", "marionberry", "marmalade", "marshmallow", "mashed", "mayonnaise", "meat", "meatball", "meatloaf", "melon", "menu", "meringue", "micronutrient", "milk", "milkshake", "millet", "mincemeat", "minerals", "mint", "mints", "mochi", "molasses", "mole", "mozzarella", "muffin", "mug", "munch", "mushroom", "mussels", "mustard", "mustard", "mutton", "napkin", "nectar", "nectarine", "nibble", "noodles", "nosh", "nourish", "nourishment", "nut", "nutmeg", "nutrient", "nutrition", "nutritious", "oatmeal", "oats", "oil", "okra", "oleo", "olive", "omelet", "omnivore", "onion", "orange", "order", "oregano", "oven", "oyster", "pan", "pancake", "papaya", "parsley", "parsnip", "pasta", "pastry", "pate", "patty", "pattypan", "pea", "pea", "peach", "peanut", "peanut", "pear", "pecan", "pepper", "pepperoni", "persimmon", "pickle", "picnic", "pie", "pilaf", "pineapple", "pita", "pitcher", "pizza", "plate", "platter", "plum", "poached", "pomegranate", "pomelo", "pop", "popcorn", "popovers", "popsicle", "pork", "pork", "pot", "pot", "potato", "preserves", "pretzel", "prime", "protein", "provisions", "prune", "pudding", "pumpernickel", "pumpkin", "punch", "quiche", "quinoa", "radish", "raisin", "raspberry", "rations", "ravioli", "recipe", "refreshments", "refrigerator", "relish", "restaurant", "rhubarb", "ribs", "rice", "roast", "roll", "rolling", "romaine", "rosemary", "rye", "saffron", "sage", "salad", "salami", "salmon", "salsa", "salt", "sandwich", "sauce", "sauerkraut", "sausage", "savory", "scallops", "scrambled", "seaweed", "seeds", "sesame", "shallots", "sherbet", "shish", "shrimp", "slaw", "slice", "smoked", "snack", "soda", "sole", "sorbet", "sorghum", "sorrel", "soup")
      good2 <- c("sour", "soy", "soybeans", "spaghetti", "spareribs", "spatula", "spices", "spicy", "spinach", "split", "spoon", "spork", "sprinkles", "sprouts", "spuds", "squash", "squid", "steak", "stew", "stir-fry", "stomach", "stove", "straw", "strawberry", "string", "stringy", "strudel", "sub", "submarine", "succotash", "suet", "sugar", "summer", "sundae", "sunflower", "supper", "sushi", "sustenance", "sweet", "sweet", "Swiss", "syrup", "taco", "take-out", "tamale", "tangerine", "tapioca", "taro", "tarragon", "tart", "tea", "teapot", "teriyaki", "thyme", "toast", "toaster", "toffee", "tofu", "tomatillo", "tomato", "torte", "tortilla", "tuber", "tuna", "turkey", "turmeric", "turnip", "ugli", "unleavened", "utensils", "vanilla", "veal", "vegetable", "venison", "vinegar", "vitamin", "wafer", "waffle", "walnut", "wasabi", "water", "water", "watercress", "watermelon", "wheat", "whey", "whipped", "wok", "yam", "yeast", "yogurt", "yolk", "zucchini", "squash", "sprouts", "Alaska", "shoots", "pepper", "beans", "tea", "peas", "orange", "rice", "sprouts", "bean", "apple", "apple", "pepper", "peas", "greens", "cheese", "cheese", "bread", "greens", "pastry", "system", "cheese", "beans", "pyramid", "fries", "ale", "cheese", "bean", "tea", "dog", "sauce", "cream", "cream", "lettuce", "tea", "almonds", "food", "corn", "beans", "beans", "box", "course", "orange", "syrup", "potatoes", "sauce", "greens", "squash", "pod", "butter", "bread", "chops", "roast", "rib", "pin", "seed", "kebab", "bread", "cream", "sauce", "peas", "bean", "sandwich", "sandwich", "squash", "potato", "chard", "fruit", "chestnut", "cream", "cone")
      good <- c(good, good2)
  
  #4 - Remove all other words from Reviews
      keep_words <- function(text, keep) {
        words <- strsplit(text, " ")[[1]]
        txt <- paste(words[words %in% keep], collapse = " ")
        return(txt)
      }
      rev_corpus <- sapply(Rev$text, function(x) keep_words(x, good))
          
  #5 - Make data ASCII
      rev_corpus <- iconv(rev_corpus, "latin1", "ASCII", sub="")
      
  #6 - Turn to corpus
      rev_corpus <- Corpus(VectorSource(as.vector(rev_corpus)))
      
  #7 - Remove punctuation
      rev_corpus <- tm_map(rev_corpus, content_transformer(removePunctuation))
      
  #8 - Set to lower case
      rev_corpus <- tm_map(rev_corpus,  content_transformer(tolower))
      
  #9 - Remove blanks
      rev_corpus <- tm_map(rev_corpus , content_transformer(stripWhitespace))
  
  #10 - create document term matrix
      rev_corpus <- DocumentTermMatrix(rev_corpus, control = list(wordLengths = c(2, Inf)))
      
  #11 - remove common terms
      removeCommonTerms <- function (x, pct) {
        stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")), 
                  is.numeric(pct), pct > 0, pct < 1)
        m <- if (inherits(x, "DocumentTermMatrix")) t(x) else x
        t <- table(m$i) < m$ncol * (pct)
        termIndex <- as.numeric(names(t[t]))
        if (inherits(x, "DocumentTermMatrix")) x[, termIndex] else x[termIndex, ]
      }
      # removeCommonTerms(rev_corpus , .05)
      
  #12 - Choose k
      k <- 30
      
  #13 - Set model parameters
      control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = T, 
      verbose = 0, prefix = tempfile(), 
      save = 0, 
      keep = 50, 
      seed = 980, # for reproducibility
      nstart = 1, best = T, 
      delta = 0.1, 
      iter = 2000, 
      burnin = 100, 
      thin = 2000)
      
  #14 - Remove documents with no words
      ui = unique(rev_corpus$i)
      rev_corpus = rev_corpus[ui, ]
      Rev <- Rev[ui, ]
      
  #15 - Run topic model
      topic_model20 <-  LDA(rev_corpus, k, method = "Gibbs", control = control_LDA_Gibbs)
      
  #16 - Look at topics
      terms(topic_model20, 10)
      
  #17 - aggregregate gamma by business
      gammas <- data.table(topic_model20@gamma)
      gammas$ID <- Rev$business_id
      RevTopics <- gammas[, lapply(.SD, mean), by=ID]
      
  #18 - save model results
      #sort output
          RevTopics <- RevTopics[match(Bus$id, RevTopics$ID), ]
      #convert to a distance matrix
          temp <- as.data.frame(RevTopics[, -1])
          temp <- as.matrix(dist(temp))
          temp <- c(temp)
      #save
          saveRDS(temp, paste(Directory, City, "_revtopics.rds", sep=""))
  }
}        
