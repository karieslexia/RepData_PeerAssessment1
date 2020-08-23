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


Total<-summarise(group_by(mydata,date), Total=sum(steps))
Total<-Total[!is.na(Total$Total),]
plot(as.Date(Total$date),Total$Total,lwd=5, type="h",xlab = "Date", ylab = "Sum of steps taken")

