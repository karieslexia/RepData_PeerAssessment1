---
output:
  html_document: 
    keep_md: yes
  pdf_document: default
---
Week2 assignment
================

#Loading and preprocessing the data

```r
unzip("activity.zip", exdir = ".")
mydata<-read.csv("activity.csv")
summary(mydata)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
str(mydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

#What is mean total number of steps taken per day?
##Calculate the total number of steps taken per day

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
mydata %>% 
  group_by(date) %>% 
  summarise(Total = sum(steps, na.rm = TRUE))
```

```
## # A tibble: 61 x 2
##    date       Total
##    <chr>      <int>
##  1 2012-10-01     0
##  2 2012-10-02   126
##  3 2012-10-03 11352
##  4 2012-10-04 12116
##  5 2012-10-05 13294
##  6 2012-10-06 15420
##  7 2012-10-07 11015
##  8 2012-10-08     0
##  9 2012-10-09 12811
## 10 2012-10-10  9900
## # ... with 51 more rows
```
## Make a histogram of the total number of steps taken each day

```r
if (!dir.exists("./figure")) {dir.create("./figure")}
Total<-summarise(group_by(mydata,date), Total=sum(steps))
Total<-Total[!is.na(Total$Total),]
png(filename = "./figure/plot1.png")
plot(as.Date(Total$date),Total$Total,lwd=5, type="h",xlab = "Date", ylab = "Sum of steps taken")
dev.off()
```

```
## png 
##   2
```
##Calculate and report the mean and median of the total number of steps taken per day

```r
mydata %>% 
  group_by(date) %>% 
  summarise(Mean = mean(steps, na.rm = TRUE),Median = median(steps, na.rm = TRUE))
```

```
## # A tibble: 61 x 3
##    date          Mean Median
##    <chr>        <dbl>  <dbl>
##  1 2012-10-01 NaN         NA
##  2 2012-10-02   0.438      0
##  3 2012-10-03  39.4        0
##  4 2012-10-04  42.1        0
##  5 2012-10-05  46.2        0
##  6 2012-10-06  53.5        0
##  7 2012-10-07  38.2        0
##  8 2012-10-08 NaN         NA
##  9 2012-10-09  44.5        0
## 10 2012-10-10  34.4        0
## # ... with 51 more rows
```

#What is the average daily activity pattern?
##Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot2<-summarise(group_by(mydata,interval), 
                 Avgsteps=mean(steps, na.rm=TRUE)
                 )
png(filename = "./figure/plot2.png")
plot(plot2$interval,plot2$Avgsteps,type = "l",xlab = "5 minute interval", ylab = "Mean of steps taken")
dev.off()
```

```
## png 
##   2
```
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
plot2$interval[plot2$Avgsteps==max(plot2$Avgsteps)]
```

```
## [1] 835
```

#Imputing missing values
##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```r
sum(is.na(mydata))
```

```
## [1] 2304
```
##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

##Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
mydata2<-mydata
mymean<-mean(mydata2$steps,na.rm = TRUE)
mydata2$steps[is.na(mydata2$steps)]<- mymean
```
##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
Total2<-summarise(group_by(mydata2,date), Total=sum(steps))
png(filename = "./figure/plot3.png")
plot(as.Date(Total2$date),Total2$Total,lwd=5, type="h",xlab = "Date", ylab = "Sum of steps taken")
dev.off()
```

```
## png 
##   2
```

#Are there differences in activity patterns between weekdays and weekends?
##Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
mydata$weekday[weekdays(strptime(mydata$date,"%F"))=="Saturday"]<-"weekend"
mydata$weekday[weekdays(strptime(mydata$date,"%F"))=="Sunday"]<-"weekend"
mydata$weekday[weekdays(strptime(mydata$date,"%F"))!="Saturday" & weekdays(strptime(mydata$date,"%F"))!="Sunday" ]<-"weekday"
mydata$weekday<-factor(c("weekday","weekend"))
```
##Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
mydata3<-mydata %>% 
  group_by(interval,weekday) %>% 
  summarise(average = mean(steps, na.rm = TRUE))

library(lattice)
png(filename = "./figure/plot4.png")
xyplot(average~interval|weekday,mydata3,layout=c(1,2), type="l",ylab = "Number of steps", xlab = "Interval")
dev.off()
```

```
## png 
##   2
```

