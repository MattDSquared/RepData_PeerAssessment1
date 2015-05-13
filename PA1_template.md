# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Set up libraries used in this analysis.

```r
library(plyr); library(dplyr); 
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

Unzip "activity.zip" and load the data in file activity.csv.

```r
setwd("~/../datascience/RepData_PeerAssessment1")
fname <- unzip("activity.zip")
activity <- read.csv(fname)
```

Format the date column.

```r
activity <- mutate(activity, date=as.Date(date))
```

## What is mean total number of steps taken per day?

```r
dailysteps <- activity %>% 
    group_by(date) %>% 
    summarize(steps=sum(steps, na.rm=TRUE)) 
hist(dailysteps$steps, breaks=10,
     col="lightblue", main="Total Daily Steps", xlab="steps")
```

![](PA1_template_files/figure-html/meansteps-1.png) 

```r
mean(dailysteps$steps)
```

```
## [1] 9354.23
```

```r
median(dailysteps$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
activitypattern <- activity %>%
    group_by(interval) %>%
    summarize(steps=mean(steps, na.rm=TRUE))
plot(activitypattern$interval,activitypattern$steps, type="l")
```

![](PA1_template_files/figure-html/activitypattern-1.png) 

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
