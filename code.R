#script to test code for R markdown document
#Loading and preprocessing the data

unzip("activity.zip", exdir = ".")
mydata<-read.csv("activity.csv")
summary(mydata)
str(mydata)

library(dplyr)
mydata %>% 
  group_by(date) %>% 
  summarise(Total = sum(steps, na.rm = TRUE))

mydata %>% 
  group_by(date) %>% 
  summarise(Mean = mean(steps, na.rm = TRUE),Median = median(steps, na.rm = TRUE))

if (!dir.exists("/.figure")) {dir.create("/.figure")}
Total<-summarise(group_by(mydata,date), Total=sum(steps))
Total<-Total[!is.na(Total$Total),]
png(filename = "./figure/plot1.png")
plot(as.Date(Total$date),Total$Total,lwd=5, type="h",xlab = "Date", ylab = "Sum of steps taken")
dev.off()


plot2<-summarise(group_by(mydata,interval), 
                 Avgsteps=mean(steps, na.rm=TRUE)
                 )
plot(plot2$interval,plot2$Avgsteps,type = "l",xlab = "5 minute interval", ylab = "Mean of steps taken")

plot2$interval[plot2$Avgsteps==max(plot2$Avgsteps)]

sum(is.na(mydata))

mydata2<-mydata
mymean<-mean(mydata2$steps,na.rm = TRUE)
mydata2$steps[is.na(mydata2$steps)]<- mymean

Total2<-summarise(group_by(mydata2,date), Total=sum(steps))
png(filename = "./figure/plot3.png")
plot(as.Date(Total2$date),Total2$Total,lwd=5, type="h",xlab = "Date", ylab = "Sum of steps taken")
dev.off()

mydata$weekday[weekdays(strptime(mydata$date,"%F"))=="Saturday"]<-"weekend"
mydata$weekday[weekdays(strptime(mydata$date,"%F"))=="Sunday"]<-"weekend"
mydata$weekday[weekdays(strptime(mydata$date,"%F"))!="Saturday" & weekdays(strptime(mydata$date,"%F"))!="Sunday" ]<-"weekday"
mydata$weekday<-factor(c("weekday","weekend"))

mydata3<-mydata %>% 
  group_by(interval,weekday) %>% 
  summarise(average = mean(steps, na.rm = TRUE))

library(lattice)
png(filename = "./figure/plot4.png")
xyplot(average~interval|weekday,mydata3,layout=c(1,2), ylab = "Number of steps", xlab = "Interval",type="l")
dev.off()
