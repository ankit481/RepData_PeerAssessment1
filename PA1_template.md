# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

### Set the current working directory


```r
setwd("C:/Coursera//Data Science//Reproducible Research//assignment//1")
```


### Load the necessary library files.


```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.1.3
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.3
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

- Load the data (i.e. read.csv())
- Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity <- read.csv("activity.csv", header = T, stringsAsFactors = F)
```


## What is mean total number of steps taken per day?

- Calculate the total number of steps taken per day


```r
steps.summary <- summarise(group_by(activity, date), StepsPerDay = sum(steps))
```

- If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
ggplot(steps.summary, aes(StepsPerDay)) +
  labs(x = "Total Steps Per Day") +
  geom_histogram(color = "black", fill = "salmon") +
  geom_vline(aes(xintercept = mean(StepsPerDay, na.rm = T)),
             color = "blue", linetype = "dashed", size = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

- Calculate and report the mean and median of the total number of steps taken per day


```r
mean(steps.summary$StepsPerDay, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(steps.summary$StepsPerDay, na.rm = T)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
activity <- activity %>%
  group_by(interval) %>%
  mutate(average.step.interval = mean(steps, na.rm = T))

ggplot(activity, aes(interval, average.step.interval)) + 
  geom_line() +
  ylab("Average Steps per Interval") + xlab("Interval") +
  geom_hline(aes(yintercept = mean(average.step.interval, na.rm = T)),
             color = "blue", linetype = "dashed", size = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
unique(subset(activity$interval, activity$average.step.interval ==  max(activity$average.step.interval, na.rm = T)))
```

```
## [1] 835
```


## Imputing missing values

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sapply(activity, function(x) sum(is.na(x)))
```

```
##                 steps                  date              interval 
##                  2304                     0                     0 
## average.step.interval 
##                     0
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity.new <- as.data.frame(activity)
activity.new$steps[is.na(activity.new$steps)] <- activity.new$average.step.interval
```

```
## Warning in activity.new$steps[is.na(activity.new$steps)] <-
## activity.new$average.step.interval: number of items to replace is not a
## multiple of replacement length
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps.summary.new <- summarise(group_by(activity.new, date), steps.per.day = sum(steps))

ggplot(steps.summary.new, aes(steps.per.day)) +
  labs(x = "Total Steps Per Day") +
  geom_histogram(color = "black", fill = "salmon") +
  geom_vline(aes(xintercept = mean(steps.per.day, na.rm = T)),
             color = "blue", linetype = "dashed", size = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

```r
mean(steps.summary.new$steps.per.day, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(steps.summary.new$steps.per.day, na.rm = T)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels â âweekdayâ and âweekendâ indicating whether a given date is a weekday or weekend day.


```r
activity.new <- activity.new %>% mutate(pattern = ifelse(weekdays(as.Date(date)) %in% c("Sunday", "Saturday"), "Weekend",
                                "Weekday"))
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
activity.new <- activity.new %>%
  group_by(pattern, interval) %>%
  mutate(average.step.interval.wde = mean(steps, na.rm = T))

ggplot(activity.new, aes(interval, average.step.interval.wde)) + 
  geom_line() +
  ylab("Average Steps per Interval") + xlab("Interval") +
  facet_grid(pattern ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
