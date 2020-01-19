setwd('/misc/utopia3/bda13/lanhome/Data/RData')
uniquebus<-readRDS("Uniquebus.rds")

#Graph relationship between review count and percent reviews that are by elite
  temp<-data.frame(supercity=uniquebus$supercity,percentreviewslost=((uniquebus$review_count-uniquebus$review_count_nonelites)/uniquebus$review_count),review_count=uniquebus$review_count)
  temp$review_count<-ifelse(uniquebus$review_count>500,500,uniquebus$review_count)
  temp$percentreviewslost<-ifelse(percentreviewslost<0,0,percentreviewslost)

  library(ggplot2)
  pdf(file="Percent Reviews by Elites.pdf", height=8, width=12)
  ggplot(temp, aes(review_count,y=percentreviewslost)) + 
    geom_smooth()+
    ggtitle("Percent Reviews by Elites") +
    labs(x="Review Count", y="Percent Reviews by Elites")
  dev.off()

