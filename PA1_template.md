---
title: 'Course 5 Reproducible Research: Course Project 1'
output: html_document
---
#### by Ellen Oosting

This reports answers the question of Course project 1 of the Coursera Data Scientist Course 5: Reproducible Research. 

First the right libraries have to be loaded:
```{r  message=F, warning=F}
library(dplyr)
library(ggplot2)
```

### Step1: Loading and preprocesseing the data
First the enviroment of r will be cleaned with:
```{r}
rm(list=ls())
```
Secondly the working directory has to be set correct, I can't show everything business pc:

setwd("~Coursera/Course 5")

Then the file has to be read into r under the name "activitydata"
```{r}
activitydata<-read.csv("activity.csv")
```
The column with date is set in the right format:
```{r}
activitydata$date<-as.Date(activitydata$date, "%Y-%m-%d")
```

### Step 2: Answering questions
**What is the mean total number of steps taken per day?**  
The total number of steps taken per day is calculated with the code:
```{r}
sumac<-with(activitydata, aggregate(activitydata[,1],by=list(activitydata$date),FUN=sum, na.rm=TRUE))  
names(sumac)<-c("date","sum")
```
The histogram of the total number of steps taken each day: 
```{r}
qplot(sumac$sum, binwidth=1000, xlab="Total number of steps taken each day",main="Activity data Histogram")
dev.copy(png,"plot1.png")
dev.off()
```
  
The mean and median of the total number of steps taken per day are in the next dataframes:
```{r}
mean(sumac$sum,na.rm=TRUE)
median(sumac$sum,na.rm=TRUE)
```
**What is the average daily activity pattern?**
The next graph is a time series plot of the 5-minute interval (x-axis),averaged number of steps across all days (y-axis).
```{r}
time <- aggregate(steps~interval, data=activitydata, FUN=mean, na.rm=TRUE)
plot(x = time$interval, y = time$steps, type = "l",xlab="Time in interval",ylab="Total number of steps") 
dev.copy(png,"plot2.png")
dev.off()
```
The 5-minute interval that, on average, contains the maximum number of steps:
```{r}
topmean<-arrange(time,-steps)
print(topmean[1,])  
```


### Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
The total number of missing values in the dataset can be calculated by:
```{r}
sum(is.na(activitydata$steps))
```

The strategy I am going to use for the missing values is to leave them out. The sum then for each day is stored in sumac2.

```{r}
activity<-na.omit(activitydata)
sumac2<-aggregate(steps~date,data=activity,FUN=sum)
qplot(sumac2$steps, binwidth=1000, xlab="Total number of steps taken each day",main="Activity data histogram without missing data")
dev.copy(png,"plot3.png")
dev.off()
print(mean(sumac2$steps))
print(median(sumac2$steps))
```

These values differ from the data with missing values:
```{r}
type_data<-c("with NA","without NA")
mean_data<-c(mean(sumac$sum,na.rm=TRUE),mean(sumac2$steps) )
median_data<-c(median(sumac$sum,na.rm=TRUE), median(sumac2$steps))
diff<-data.frame(type_data,mean_data,median_data)
```

**Are there differences in activity patterns between weekdays and weekends?**
First I made column with the names of the day of the week, in that column I look for saterdays and sundays (in dutch). Then I make to column with the type of day and change the rows for the weekend.

```{r}
activitydata$nameday<-weekdays(activitydata$date)
saturdays<-grep("zaterdag",activitydata$nameday)
sundays<-grep("zondag",activitydata$nameday)
activitydata$typeday<-"weekday"
activitydata$typeday[saturdays]<-"weekend"
activitydata$typeday[sundays]<-"weekend"

```

Now a panel plot with time series plot of the 5-minute interval (x-axis) and the avarage number of steps taken avaraged across all weekdays and weekend days (y=axis). 

```{r}
weekdayactivity<-activitydata[activitydata$typeday=="weekday",]
weekdendactivity<-activitydata[activitydata$typeday=="weekend",]
par(mfrow=c(2,1))
weekdaytime <- aggregate(steps~interval, data=weekdayactivity, FUN=mean, na.rm=TRUE)
plot(x = weekdaytime$interval, y = weekdaytime$steps, type = "l",xlab="Time in interval",ylab="Total number of steps", main="Week day activity") 
weekendtime <- aggregate(steps~interval, data=weekdendactivity, FUN=mean, na.rm=TRUE)
plot(x = weekendtime$interval, y = weekendtime$steps, type = "l",xlab="Time in interval",ylab="Total number of steps",main="Weekend day activity") 
dev.copy(png,"plot4.png")
dev.off()
```


