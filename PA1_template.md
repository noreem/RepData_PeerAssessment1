# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


1. Load the data

First we setup the environment
   

```r
setwd("C:\\Work\\DataScience\\RepData_PeerAssessment1")
```
   
Then unpack the data

```r
unzip("activity.zip", overwrite = T, exdir = ".")
```

And finally load the data

```r
data <- read.csv("activity.csv")
```

2. Convert the date column from factor to date


```r
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
totalStepsPerDay <- aggregate(steps~date, data=data, FUN = sum, na.rm = TRUE)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(totalStepsPerDay$steps, breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(totalStepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(totalStepsPerDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
averageStepsOnInterval <- aggregate(steps~interval, data=data, FUN = mean, na.rm = TRUE)
plot(averageStepsOnInterval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
subset(data, data$steps == max(data$steps, na.rm = TRUE))[["interval"]]
```

```
## [1] 615
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset


```r
sumOfMissingValues = sum(is.na(data$steps))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc


```r
fillSteps <- function(steps, interval) {
  if(is.na(steps))
    return(averageStepsOnInterval[averageStepsOnInterval$interval == interval, "steps"])
  else
    return(c(steps))
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
filledData <- data
filledData$steps <- mapply(fillSteps, data$steps, data$interval)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalStepsPerDayFilled <- aggregate(steps~date, data=filledData, FUN=sum, na.rm = TRUE)
hist(totalStepsPerDayFilled$steps, breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

Is there any difference?


```r
mean(totalStepsPerDayFilled$steps)
```

```
## [1] 10766.19
```

```r
median(totalStepsPerDayFilled$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
library(chron)
filledData$weekDay <- as.factor(ifelse(is.weekend(filledData$date), "weekend", "weekday"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
par(mfrow = c(2, 1))
weekdaySteps <- aggregate(steps ~ interval, data = filledData, subset = filledData$weekDay =="weekday", FUN = mean)
plot(weekdaySteps, type = "l", main = "weekday")

weekendSteps <- aggregate(steps ~ interval, data = filledData, subset = filledData$weekDay == "weekend", FUN = mean)
plot(weekendSteps, type = "l", main = "weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 
