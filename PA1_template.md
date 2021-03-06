---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Welcome to my Reproducible Research Peer Assignment 1 document. 

## Loading and preprocessing the data

First, we need to load the data. We assume that the file is already downloaded. Also, we assume that it is located in the working directory. Then we convert date values to *date* format.


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
data$Date<-as.Date(data$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

Now we need to calculate the total number of steps taken per day. We will use *aggregate* function for this task.


```r
agg.date <- aggregate(x=data$steps, by=list(data$Date), FUN=sum, na.rm=TRUE)
names(agg.date) <- c("Date", "steps")
hist(agg.date$steps, xlab="Steps per day", main="Steps per day distribution")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Then we take mean and median of daily values.


```r
mean(agg.date$steps)
```

```
## [1] 9354.23
```

```r
median(agg.date$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Now let us look at how the number of steps changes during a day (we take interval averages for all days). Intervals are converted to *time*. 


```r
agg.interval <- aggregate(x=data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
names(agg.interval)=c("interval", "steps")
agg.interval$steps <- round(agg.interval$steps)
library(lubridate)
agg.interval$time<-parse_date_time(agg.interval$interval, c("%H%M", "%M"))
plot(agg.interval$time, agg.interval$steps, type="l", ylab="Steps", xlab="Time", main="Average daily activity pattern")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Next task is to find the largest number of steps and determine when it happened.


```r
max <- which.max(agg.interval$steps)
max
```

```
## [1] 104
```

```r
max.interval <- agg.interval[max, 1]
max.interval
```

```
## [1] 835
```

```r
max.steps <- agg.interval[max, 2]
max.steps
```

```
## [1] 206
```

As we see, maximum number of steps is 206 and it occurs at 835 interval.

## Imputing missing values

Let us check whether we have missing values in our dataset.


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Since we do have missing values, let us replace them with a mean for respective interval.


```r
data.na <- data
agg.na <- aggregate(x=data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
for (i in 1:nrow(data.na))
    {
    if (is.na(data.na[i, 1]))
        {
        data.na[i, 1] <- round(agg.na$x[agg.na$Group.1==data.na[i, 3]])    
        }
    }
agg.na.date <- aggregate(x=data.na$steps, by=list(data.na$Date), FUN=sum)
names(agg.na.date) <- c("Date", "steps")
hist(agg.na.date$steps, xlab="Steps per day", main="Steps per day distribution (no NAs)")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Again, we take mean and median of daily values:


```r
mean(agg.na.date$steps)
```

```
## [1] 10765.64
```

```r
median(agg.na.date$steps)
```

```
## [1] 10762
```

As we see from both the shapes of the distribution and the differences between the means and the medians, imputing missing variables makes distribution look closer to normal.

## Are there differences in activity patterns between weekdays and weekends?

In order to see the difference, we need to split the dataset according to day type. Then we will plot the graph for each day type. Scale will be the same for comparison convenience.


```r
Sys.setlocale("LC_TIME", "US") 
```

```
## [1] "English_United States.1252"
```

```r
data.na$weekday <-ifelse((weekdays(data.na$Date)=="Saturday" | weekdays(data.na$Date)=="Sunday"), "weekend", "weekday")
weekday<-data.na[data.na$weekday=="weekday", ]
weekend<-data.na[data.na$weekday=="weekend", ]

agg.weekday <-aggregate(x=weekday$steps, by=list(weekday$interval), FUN=mean)
names(agg.weekday) <- c("interval", "steps")
agg.weekday$time<-parse_date_time(agg.weekday$interval, c("%H%M", "%M"))

agg.weekend <-aggregate(x=weekend$steps, by=list(weekend$interval), FUN=mean)
names(agg.weekend) <- c("interval", "steps")
agg.weekend$time<-parse_date_time(agg.weekend$interval, c("%H%M", "%M"))

par(mfrow=c(1,2))
plot(agg.weekday$time, agg.weekday$steps, ylim=c(0, 250), xlab="Time", ylab="Steps", main="Working days")
plot(agg.weekend$time, agg.weekend$steps, ylim=c(0, 250), xlab="Time", ylab="Steps", main="Weekend")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

We can see that activity distribution during a day depends on the type of a day. On working days people get up earlier, then commute (some part of commuting is walking), walk a bit during their work, then get back home (in the evening they are tired that's why walk less and take public transportation as soon as it is possible). During weekend, people get up later, walk a lot during a day, and the distribution of activity is more even. Local minima are probably associated with meals.
