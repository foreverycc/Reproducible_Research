---
title: "Assignment1"
author: "Chenchen"
date: "April 11, 2015"
output: html_document
---

##Loading and preprocessing the data

```r
Data = read.csv("../data/activity.csv")
Data$date_fac = factor(Data$date)
Data$interval_fac = factor(Data$interval)
Data_NARM = Data[complete.cases(Data), ]
head(Data_NARM)
```

```
##     steps       date interval   date_fac interval_fac
## 289     0 2012-10-02        0 2012-10-02            0
## 290     0 2012-10-02        5 2012-10-02            5
## 291     0 2012-10-02       10 2012-10-02           10
## 292     0 2012-10-02       15 2012-10-02           15
## 293     0 2012-10-02       20 2012-10-02           20
## 294     0 2012-10-02       25 2012-10-02           25
```

##What is mean total number of steps taken per day?

```r
Data_NARM_SumByDay = aggregate(Data_NARM$steps, by = list(Data_NARM$date_fac), sum)
names(Data_NARM_SumByDay) = c("date", "steps")
hist(Data_NARM_SumByDay$steps, breaks = 20, 
     main = "Histogram of Total Steps by Day",
     xlab = "Total Steps Per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
# mean total steps per day
mean(Data_NARM_SumByDay$steps)
```

```
## [1] 10766.19
```

```r
# median total steps per day
median(Data_NARM_SumByDay$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?
![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
# The interval that contains the maximum number of steps
as.numeric(as.character(Data_NARM_AvgByInterval[which.max(Data_NARM_AvgByInterval$steps_avg), "interval"]))
```

```
## [1] 835
```

##Imputing missing values

```r
# calculate total NAs
sum(is.na(Data$steps))
```

```
## [1] 2304
```

```r
# impute missing values by average steps of the interval
Data_imput = Data
for (i in 1:nrow(Data_imput)) {
  if (is.na (Data_imput$steps[i])) {
    interval_index = which (Data_NARM_AvgByInterval$interval == Data_imput$interval[i])
    Data_imput$steps[i] = round(Data_NARM_AvgByInterval$steps_avg[interval_index], 0)
  } else next
}
head(Data_imput)
```

```
##   steps       date interval   date_fac interval_fac
## 1     2 2012-10-01        0 2012-10-01            0
## 2     0 2012-10-01        5 2012-10-01            5
## 3     0 2012-10-01       10 2012-10-01           10
## 4     0 2012-10-01       15 2012-10-01           15
## 5     0 2012-10-01       20 2012-10-01           20
## 6     2 2012-10-01       25 2012-10-01           25
```
Make histograms and calculate average and median sum steps per day

```r
Data_imput_SumByDay = aggregate(Data_imput$steps, by = list(Data_imput$date_fac), sum)
names(Data_imput_SumByDay) = c("date", "steps")
hist(Data_imput_SumByDay$steps, breaks = 20, 
     main = "Histogram of Total Steps by Day",
     xlab = "Total Steps Per Day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
# mean total steps per day
mean(Data_imput_SumByDay$steps)
```

```
## [1] 10765.64
```

```r
# median total steps per day
median(Data_imput_SumByDay$steps)
```

```
## [1] 10762
```
##Are there differences in activity patterns between weekdays and weekends?

```r
# make a new variable of weekday and weekend 
Data_imput$weekday = weekdays(as.Date(Data_imput$date))
Data_imput$day_level = factor (ifelse(Data_imput$weekday %in% c("Saturday", "Sunday"), "weekend", "weekday"))
Data_imput_AvgByInterval = aggregate(Data_imput$steps, by = list(Data_imput$day_level, Data_imput$interval_fac), mean)
names(Data_imput_AvgByInterval) = c("day_level", "interval", "steps")

library(lattice)
xyplot(Data_imput_AvgByInterval$steps~Data_imput_AvgByInterval$interval|Data_imput_AvgByInterval$day_level, type = "l", scales = list(x = list(tick.number =4)), layout = c(1,2), xlab = "Interval", ylab = "Average Steps", main = "Average Steps by Intervals")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 
