# Reproducible Research: Peer Assessment 1

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

Here are the calculated mean and median steps per day.

```r
mean_steps_per_day
```

```
##       Group.1          x
## 1  2012-10-02  0.4375000
## 2  2012-10-03 39.4166667
## 3  2012-10-04 42.0694444
## 4  2012-10-05 46.1597222
## 5  2012-10-06 53.5416667
## 6  2012-10-07 38.2465278
## 7  2012-10-09 44.4826389
## 8  2012-10-10 34.3750000
## 9  2012-10-11 35.7777778
## 10 2012-10-12 60.3541667
## 11 2012-10-13 43.1458333
## 12 2012-10-14 52.4236111
## 13 2012-10-15 35.2048611
## 14 2012-10-16 52.3750000
## 15 2012-10-17 46.7083333
## 16 2012-10-18 34.9166667
## 17 2012-10-19 41.0729167
## 18 2012-10-20 36.0937500
## 19 2012-10-21 30.6284722
## 20 2012-10-22 46.7361111
## 21 2012-10-23 30.9652778
## 22 2012-10-24 29.0104167
## 23 2012-10-25  8.6527778
## 24 2012-10-26 23.5347222
## 25 2012-10-27 35.1354167
## 26 2012-10-28 39.7847222
## 27 2012-10-29 17.4236111
## 28 2012-10-30 34.0937500
## 29 2012-10-31 53.5208333
## 30 2012-11-02 36.8055556
## 31 2012-11-03 36.7048611
## 32 2012-11-05 36.2465278
## 33 2012-11-06 28.9375000
## 34 2012-11-07 44.7326389
## 35 2012-11-08 11.1770833
## 36 2012-11-11 43.7777778
## 37 2012-11-12 37.3784722
## 38 2012-11-13 25.4722222
## 39 2012-11-15  0.1423611
## 40 2012-11-16 18.8923611
## 41 2012-11-17 49.7881944
## 42 2012-11-18 52.4652778
## 43 2012-11-19 30.6979167
## 44 2012-11-20 15.5277778
## 45 2012-11-21 44.3993056
## 46 2012-11-22 70.9270833
## 47 2012-11-23 73.5902778
## 48 2012-11-24 50.2708333
## 49 2012-11-25 41.0902778
## 50 2012-11-26 38.7569444
## 51 2012-11-27 47.3819444
## 52 2012-11-28 35.3576389
## 53 2012-11-29 24.4687500
```

```r
median_steps_per_day
```

```
##       Group.1 x
## 1  2012-10-02 0
## 2  2012-10-03 0
## 3  2012-10-04 0
## 4  2012-10-05 0
## 5  2012-10-06 0
## 6  2012-10-07 0
## 7  2012-10-09 0
## 8  2012-10-10 0
## 9  2012-10-11 0
## 10 2012-10-12 0
## 11 2012-10-13 0
## 12 2012-10-14 0
## 13 2012-10-15 0
## 14 2012-10-16 0
## 15 2012-10-17 0
## 16 2012-10-18 0
## 17 2012-10-19 0
## 18 2012-10-20 0
## 19 2012-10-21 0
## 20 2012-10-22 0
## 21 2012-10-23 0
## 22 2012-10-24 0
## 23 2012-10-25 0
## 24 2012-10-26 0
## 25 2012-10-27 0
## 26 2012-10-28 0
## 27 2012-10-29 0
## 28 2012-10-30 0
## 29 2012-10-31 0
## 30 2012-11-02 0
## 31 2012-11-03 0
## 32 2012-11-05 0
## 33 2012-11-06 0
## 34 2012-11-07 0
## 35 2012-11-08 0
## 36 2012-11-11 0
## 37 2012-11-12 0
## 38 2012-11-13 0
## 39 2012-11-15 0
## 40 2012-11-16 0
## 41 2012-11-17 0
## 42 2012-11-18 0
## 43 2012-11-19 0
## 44 2012-11-20 0
## 45 2012-11-21 0
## 46 2012-11-22 0
## 47 2012-11-23 0
## 48 2012-11-24 0
## 49 2012-11-25 0
## 50 2012-11-26 0
## 51 2012-11-27 0
## 52 2012-11-28 0
## 53 2012-11-29 0
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

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

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

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

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

Here are the imputted mean and median steps per day.

```r
imputted_mean_steps_per_day
```

```
##       Group.1          x
## 1  2012-10-01 37.3825996
## 2  2012-10-02  0.4375000
## 3  2012-10-03 39.4166667
## 4  2012-10-04 42.0694444
## 5  2012-10-05 46.1597222
## 6  2012-10-06 53.5416667
## 7  2012-10-07 38.2465278
## 8  2012-10-08 37.3825996
## 9  2012-10-09 44.4826389
## 10 2012-10-10 34.3750000
## 11 2012-10-11 35.7777778
## 12 2012-10-12 60.3541667
## 13 2012-10-13 43.1458333
## 14 2012-10-14 52.4236111
## 15 2012-10-15 35.2048611
## 16 2012-10-16 52.3750000
## 17 2012-10-17 46.7083333
## 18 2012-10-18 34.9166667
## 19 2012-10-19 41.0729167
## 20 2012-10-20 36.0937500
## 21 2012-10-21 30.6284722
## 22 2012-10-22 46.7361111
## 23 2012-10-23 30.9652778
## 24 2012-10-24 29.0104167
## 25 2012-10-25  8.6527778
## 26 2012-10-26 23.5347222
## 27 2012-10-27 35.1354167
## 28 2012-10-28 39.7847222
## 29 2012-10-29 17.4236111
## 30 2012-10-30 34.0937500
## 31 2012-10-31 53.5208333
## 32 2012-11-01 37.3825996
## 33 2012-11-02 36.8055556
## 34 2012-11-03 36.7048611
## 35 2012-11-04 37.3825996
## 36 2012-11-05 36.2465278
## 37 2012-11-06 28.9375000
## 38 2012-11-07 44.7326389
## 39 2012-11-08 11.1770833
## 40 2012-11-09 37.3825996
## 41 2012-11-10 37.3825996
## 42 2012-11-11 43.7777778
## 43 2012-11-12 37.3784722
## 44 2012-11-13 25.4722222
## 45 2012-11-14 37.3825996
## 46 2012-11-15  0.1423611
## 47 2012-11-16 18.8923611
## 48 2012-11-17 49.7881944
## 49 2012-11-18 52.4652778
## 50 2012-11-19 30.6979167
## 51 2012-11-20 15.5277778
## 52 2012-11-21 44.3993056
## 53 2012-11-22 70.9270833
## 54 2012-11-23 73.5902778
## 55 2012-11-24 50.2708333
## 56 2012-11-25 41.0902778
## 57 2012-11-26 38.7569444
## 58 2012-11-27 47.3819444
## 59 2012-11-28 35.3576389
## 60 2012-11-29 24.4687500
## 61 2012-11-30 37.3825996
```

```r
imputted_median_steps_per_day
```

```
##       Group.1        x
## 1  2012-10-01 34.11321
## 2  2012-10-02  0.00000
## 3  2012-10-03  0.00000
## 4  2012-10-04  0.00000
## 5  2012-10-05  0.00000
## 6  2012-10-06  0.00000
## 7  2012-10-07  0.00000
## 8  2012-10-08 34.11321
## 9  2012-10-09  0.00000
## 10 2012-10-10  0.00000
## 11 2012-10-11  0.00000
## 12 2012-10-12  0.00000
## 13 2012-10-13  0.00000
## 14 2012-10-14  0.00000
## 15 2012-10-15  0.00000
## 16 2012-10-16  0.00000
## 17 2012-10-17  0.00000
## 18 2012-10-18  0.00000
## 19 2012-10-19  0.00000
## 20 2012-10-20  0.00000
## 21 2012-10-21  0.00000
## 22 2012-10-22  0.00000
## 23 2012-10-23  0.00000
## 24 2012-10-24  0.00000
## 25 2012-10-25  0.00000
## 26 2012-10-26  0.00000
## 27 2012-10-27  0.00000
## 28 2012-10-28  0.00000
## 29 2012-10-29  0.00000
## 30 2012-10-30  0.00000
## 31 2012-10-31  0.00000
## 32 2012-11-01 34.11321
## 33 2012-11-02  0.00000
## 34 2012-11-03  0.00000
## 35 2012-11-04 34.11321
## 36 2012-11-05  0.00000
## 37 2012-11-06  0.00000
## 38 2012-11-07  0.00000
## 39 2012-11-08  0.00000
## 40 2012-11-09 34.11321
## 41 2012-11-10 34.11321
## 42 2012-11-11  0.00000
## 43 2012-11-12  0.00000
## 44 2012-11-13  0.00000
## 45 2012-11-14 34.11321
## 46 2012-11-15  0.00000
## 47 2012-11-16  0.00000
## 48 2012-11-17  0.00000
## 49 2012-11-18  0.00000
## 50 2012-11-19  0.00000
## 51 2012-11-20  0.00000
## 52 2012-11-21  0.00000
## 53 2012-11-22  0.00000
## 54 2012-11-23  0.00000
## 55 2012-11-24  0.00000
## 56 2012-11-25  0.00000
## 57 2012-11-26  0.00000
## 58 2012-11-27  0.00000
## 59 2012-11-28  0.00000
## 60 2012-11-29  0.00000
## 61 2012-11-30 34.11321
```

* Comparision to non complete dataset

Here is a histogram for the dataset with imputted values.

```r
hist(imputted_total_steps_per_day$x,
     main = "Histogram of  Imputted Total Steps taken per day",
     xlab = "Total Steps per day",
     ylab = "Frequency",
     col = "blue",
     breaks = 10,
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 


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

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

It seems like the is less volatile over the weekends and the 
mean seems slightly more centered towards the afternoon on the weekends. This
makes sense as people go out to the beach and other places.



