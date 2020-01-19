library(gridExtra)
library(pROC)

#0 - Set Directory
    setwd(Directory)

#1 - Read Data
    pam1 <- readRDS("pam1.rds")
    pam2 <- readRDS("pam2.rds")
    pam3 <- readRDS("pam3.rds")
    pam4 <- readRDS("pam4.rds")
    I <- readRDS("I.rds")

#2 - Prepare ROC function
    calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
      tpr <- function(df, threshold) {
        sum(df$pred >= threshold & df$dissolved == 1) / sum(df$dissolved == 1)
      }
      fpr <- function(df, threshold) {
        sum(df$pred >= threshold & df$dissolved == 0) / sum(df$dissolved == 0)
      }
      cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
        sum(df$pred >= threshold & df$dissolved == 0) * cost_of_fp +
          sum(df$pred < threshold & df$dissolved == 1) * cost_of_fn
      }
      roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
      roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
      roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
      roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
      return(roc)
    }

#3 - Make predictions
    predictions3<-data.frame(dissolved=I$Dissolved,pred=exp(predict(pam3)))
    
#4 - calculate ROC
    roc3 <- calculate_roc(predictions3, 1, 2, n = 10000)

#5 - plot results    
    png(file="C:/Users/bda13/Desktop/ROC.png", height=7, width=9,units = "in",res = 200)
    ggplot(NULL) +
      geom_point(data=roc3, aes(fpr,tpr), size=3, alpha=0.5,color="black")+  #color=col_by_cost,
      coord_fixed() +
      geom_line(data=roc3, aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5),size=2) +
      labs(title = sprintf("")) + xlab("False Positive Rate") + ylab("True Positive Rate")+
      theme(
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        title =element_text(size=32),
        legend.position="none",
        text=element_text(family="serif"))
    dev.off()
