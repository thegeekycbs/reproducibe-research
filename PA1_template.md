---
title: "Reproducibe Research: Peer Asessment 1 "
author: "cbs"
date: "Thursday, February 12, 2015"
---

Please set working directory to the folder where activity.csv is donwloaded and would be extracted from. 

Please use setwd() to set the working director and getwd() to find out the appropriate directory

Load the approrpiate packages/libraries to execute code


```r
library(ggplot2)
```

```
## Find out what's changed in ggplot2 with
## news(Version == "1.0.0", package = "ggplot2")
```

```r
library(plyr)
```

Loading and preprocessing the data


```r
unzip(zipfile ="activity.zip")
```

```
## Warning in unzip(zipfile = "activity.zip"): error 1 in extracting from zip
## file
```

```r
activity_data <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
```

Transform the data into a format suitable for your analysis

```r
library(plyr)
activity_data$date <- as.Date(activity_data$date)

act_total <- ddply(activity_data, "date", summarize,
                steps = sum(steps),
                interval = sum(interval))

activity2 <- activity_data[complete.cases(activity_data),]

activity2$time <- strptime((sapply(activity2$interval, formatC, width = 4, flag = 0)), format = "%H%M")

activity2$time <- format(activity2$time, "%H:%M")

activity_sum2 <- ddply(activity2, "date", summarize,
                 steps = sum(steps),
                 interval = sum(interval),
                 time = max(time))

interval_sum2 <- ddply(activity2, "time", summarize,
                      date = mode(date),
                      steps = mean(steps),
                      interval = max(interval))
```


##What is mean total number of steps taken per day?


```r
library(ggplot2)
qplot(steps, data = activity_sum2, main="Histogram for number of steps per day", binwidth = 1000)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

The mean of steps taken per day is calculated as:


```r
mean(activity_sum2$steps)
```

```
## [1] 10766.19
```

and the median for total of steps per days is:


```r
median(activity_sum2$steps)
```

```
## [1] 10765
```


##What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
qqq<-interval_sum2$time
aaa<-interval_sum2$interval
a1<-aaa[seq(1,length(aaa),30)]
q1<-qqq[seq(1,length(qqq),30)]

ggplot(data = interval_sum2, aes(interval, steps )) + geom_line() + scale_x_continuous(breaks=a1, labels=q1) + ylab("Average Steps Per 5-Minute Interval") + xlab("Time Interval")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Which 5-minute interval, on average across all the days in the datasets, contains the maximum number of steps?


```r
maxint <- interval_sum2$interval[which(interval_sum2$steps == max(interval_sum2$steps))]
maxtime <- interval_sum2$time[which(interval_sum2$steps == max(interval_sum2$steps))]
```

On average, across all the days in the dataset, the maximum number of steps occurs during the interval is
835
or the daily time of
08:35

Imputing missing value

##Calculate and report the total number of missing values in the dataset


```r
x <- (is.na(activity_data$steps))
numna <- sum(x)
```

Hence, there are
'r numna`
number of of NA values in the dataset

Devise a strategy for filling in all of the missing in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day. or the mean for the 5 minute interval etc

I use the strategy where I impute the 5 minute interval mean


```r
stepmeans <- ddply(activity2, ~interval, summarise, mean = mean(steps))
interval <- stepmeans$interval
df <- activity_data

for(i in 1:length(df[,1])){
  if(is.na(df[i,1]))
    {df[i,1] <- stepmeans[which(interval == df[i,3]),2]}
  else if (is.na(df[i,1]) ==  FALSE)
    { df[i,1] <- df[i,1]}
  }
```

Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps take per day. 


```r
df$time<-strptime((sapply(df$interval, formatC, width = 4, flag = 0)), format = "%H%M")
df$time <- format(df$time,"%H:%M")

dfsum2<-ddply(df, "date", summarize, 
              steps=sum(steps),
              interval=sum(interval),
              time=max(time))

qplot(steps, data=dfsum2, main='Histogram Using Imputed Values', binwidth=1000)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
oldmean<-mean(activity_sum2$steps)
oldmedian<-median(activity_sum2$steps)

impmean<-mean(dfsum2$steps)
impmedian<-median(dfsum2$steps)
```

The new mean is 1.0766189 &times; 10<sup>4</sup> compared to the value of 1.0766189 &times; 10<sup>4</sup>. Similarly, the new median is 1.0766189 &times; 10<sup>4</sup>, compared to the old value of 10765.

*Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

The median values do not differ very much and the mean values are identical. That's probably because we were imputing mean values into the dataset.

This histogram is more centralizied, with more values landing on the average, whereas the tails are mostly not affected.

##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
for (i in 1:length(df[, 2])){
    if (weekdays(df[i, 2])=="Saturday")
        {df$weekend[i]<-'weekend'}
    else if ((weekdays(df[i, 2])=="Sunday"))
        {df$weekend[i]<-'weekend'}
    else {df$weekend[i]<-'weekday'}
}

 dfweekend<-ddply(df,~time*weekend,summarise,
                  date=mode(date),
                  steps=format(mean(steps), scientific = NA),
                  interval=max(interval),
                  time=max(time))
```


Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
library(lattice)

dfweekend <- transform(dfweekend, weekend = factor(weekend))
x<-dfweekend$interval
y<-dfweekend$steps
z<-dfweekend$weekend
dfweekend$steps<-as.numeric(dfweekend$steps)

ggplot(data=dfweekend, aes(x=interval, y=steps, color=weekend))+geom_line() + facet_grid(weekend~.)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

Thank you for viewing the report. 

