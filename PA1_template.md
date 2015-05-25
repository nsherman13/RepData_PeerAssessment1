---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
I used read.csv() with default values to load the raw activity data.


```r
library(knitr)
library(ggplot2)
opts_chunk$set(echo = TRUE)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
raw_data <- read.csv("activity.csv")
```

## What is the mean total number of steps taken per day?

My first step was to remove all NA values from the raw_data. I used complete.cases() to do this particular task.
I calculated the sum per day, the mean per day and the median per day by aggregating data by date.


```r
clean_data <- raw_data[complete.cases(raw_data$steps),]
total_steps_per_day <- aggregate(clean_data$steps,by = list(clean_data$date),FUN = sum)
mean_steps_per_day <- aggregate(clean_data$steps,by = list(clean_data$date),FUN = mean)
median_steps_per_day <- aggregate(clean_data$steps,by = list(clean_data$date),FUN = median)
```

Here is the histogram representing the frequencies of total steps taken per day.

```r
hist(total_steps_per_day$x,
     main = "Histogram of Total Steps taken per day",
     xlab = "Total Steps per day",
     ylab = "Frequency",
     col = "blue",
     breaks = 10
    )
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

## What is the average daily activity pattern?

I used a similar strategy to calculate the average steps taken per interval across all days. 
I aggregated data by interval instead of date.


```r
mean_steps_per_interval <- aggregate(clean_data$steps,by = list(clean_data$interval),FUN = mean)
plot(y = mean_steps_per_interval$x, 
     x = mean_steps_per_interval$Group.1,
     type = "l",
     main = "Average Steps taken per Interval",
     xlab = "Time interval",
     ylab = "Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

* Maximum Steps

The maximum value is 206 steps and this occurs at 835 am. This makes sense, as at that time many people are rushing
out the door to catch transportation to work. Alternatively, they could be moving around the house making sure they
are prepared to leave.

## Imputing missing values

* Total number of missing values


```r
sum(is.na(raw_data$steps))
```

```
## [1] 2304
```

* Strategy for replacing missing values

I wanted to replace missing values with the mean steps taken per that time interval. In order to 
do that, I appended the mean_steps_per_interval column to raw data. I renamed columns and then used 
mutate with an ifelse to properly populate missing values.

* Creating new dataset


```r
imputted_data <- cbind(raw_data,mean_steps_per_interval$x)
colnames(imputted_data) <- c("Steps","Date","Interval","Mean.Steps")
imputted_data <- mutate(imputted_data,Steps = ifelse(is.na(Steps),Mean.Steps,Steps))
imputted_total_steps_per_day <- aggregate(imputted_data$Steps,by = list(imputted_data$Date),FUN = sum)
imputted_mean_steps_per_day <- aggregate(imputted_data$Steps,by = list(imputted_data$Date),FUN = mean)
imputted_median_steps_per_day <- aggregate(imputted_data$Steps,by = list(imputted_data$Date),FUN = median)
```

* Comparision to non complete dataset


```r
hist(imputted_total_steps_per_day$x,
     main = "Histogram of  Imputted Total Steps taken per day",
     xlab = "Total Steps per day",
     ylab = "Frequency",
     col = "blue",
     breaks = 10,
    )
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 


## Are there differences in activity patterns between weekdays and weekends?

For this question, I needed to convert the Date vector to date format.

```r
imputted_data$Date <- as.character(imputted_data$Date)
imputted_data$Date <- as.Date(imputted_data$Date)
```



```r
imputted_data$Date <- as.character(imputted_data$Date)
imputted_data$Date <- as.Date(imputted_data$Date)
imputted_data$work_day <- weekdays(imputted_data$Date)
imputted_data$week_type[imputted_data$work_day %in% 
                      c("Monday","Tuesday","Wednesday","Thursday","Friday")] <- 0
imputted_data$week_type[imputted_data$work_day %in% 
                      c("Saturday","Sunday")] <- 1
imputted_data$day_type <- factor(imputted_data$week_type,labels = c("weekday","weekend"))
```

Here is the calculation of average steps per time interval per weekdays/weekend.


```r
final_mean <- aggregate(imputted_data$Steps,by = list(imputted_data$Interval,imputted_data$day_type),FUN = mean)
```

Here is the graph of average steps per time interval. 

```r
qplot(Group.1,x,
      data = final_mean,
      xlab = "Interval",
      ylab = "Steps",
      main = "Average Activity Steps per Interval",
      facets = Group.2~.,
       geom = c("line","smooth")
      )
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

It seems like the is less volatile over the weekends and the 
mean seems slightly more centered towards the afternoon on the weekends. This
makes sense as people go out to the beach and other leisurely activities.


