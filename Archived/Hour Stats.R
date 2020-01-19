uniquebus<-readRDS("C:/Users/bdaro_000/Sociology/Dissertation/Data and Code/RData/BusinessData.rds")

HoursStats<-as.data.frame(matrix(nrow=7,ncol=4))
names(HoursStats)<-c("Day","NotOpen","OpenHour","CloseHour")
HoursStats$Day<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
HoursStats$NotOpen<-c(sum(uniquebus$MondayNotOpen),sum(uniquebus$TuesdayNotOpen),sum(uniquebus$WednesdayNotOpen),sum(uniquebus$ThursdayNotOpen),sum(uniquebus$FridayNotOpen),sum(uniquebus$SaturdayNotOpen),sum(uniquebus$SundayNotOpen))
HoursStats$OpenHour<-c(mean(uniquebus$MondayOpen,na.rm=T),mean(uniquebus$TuesdayOpen,na.rm=T),mean(uniquebus$WednesdayOpen,na.rm=T),mean(uniquebus$ThursdayOpen,na.rm=T),mean(uniquebus$FridayOpen,na.rm=T),mean(uniquebus$SaturdayOpen,na.rm=T),mean(uniquebus$SundayOpen,na.rm=T))
HoursStats$CloseHour<-c(mean(uniquebus$MondayClose,na.rm=T),mean(uniquebus$TuesdayClose,na.rm=T),mean(uniquebus$WednesdayClose,na.rm=T),mean(uniquebus$ThursdayClose,na.rm=T),mean(uniquebus$FridayClose,na.rm=T),mean(uniquebus$SaturdayClose,na.rm=T),mean(uniquebus$SundayClose,na.rm=T))


