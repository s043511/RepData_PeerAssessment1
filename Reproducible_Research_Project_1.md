---
output: 
  html_document: 
    keep_md: yes
---
# Reproducible Research: Peer Assessment 1

## 1.Code for reading in the dataset and/or processing the data

##### 1.1. Load the data (i.e. read.csv())


```r
rawDataDir <- "./rawData"
rawDataUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
rawDataFilename <- "rawData.zip"
rawDataDFn <- paste(rawDataDir, "/", "rawData.zip", sep = "")
dataDir <- "./data1"

if (!file.exists(rawDataDir)) {
  dir.create(rawDataDir)
  download.file(url = rawDataUrl, destfile = rawDataDFn)
}
if (!file.exists(dataDir)) {
  dir.create(dataDir)
  unzip(zipfile = rawDataDFn, exdir = dataDir)
}

activity <- read.csv(paste(sep = "", dataDir, "/activity.csv"))
```


##### 1.2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
#activity$interval <- strptime(gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", activity$interval), format='%H:%M')
```

## 2.Histogram of the total number of steps taken each day


##### 2.1. Calculate the total number of steps taken per day

```r
stepsByDay <- aggregate(steps ~ date, activity, sum)
```

##### 2.2. Make a histogram of the total number of steps taken each day



```r
hist(stepsByDay$steps, main = paste("Total number of steps taken each day"), col="blue", xlab="Number of Steps")
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## 3.Calculate and report the mean and median of the total number of steps taken per day


```r
result.mean <- mean(stepsByDay$steps)
result.median <- median(stepsByDay$steps)
```

* Mean: 1.0766189\times 10^{4}
* Median:  10765

## 4.Time series plot of the average number of steps taken

##### 4.1. What is the average daily activity pattern?



```r
stepsByInterval <- aggregate(steps ~ interval, activity, mean)
```

##### 4.2.  Make a time series plot


```r
library(ggplot2)

ggplot(data=stepsByInterval, aes(x=interval, y=steps)) +
  geom_line() +
  ylab(expression('Average step')) +
  ggtitle('Time series plot of the average number of steps taken')  
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


## 5.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxInterval <- stepsByInterval[which.max(stepsByInterval$steps),1]
maxValue <- stepsByInterval[which.max(stepsByInterval$steps),2]
```

* Max Interval: 835
* Max Value   :  206.1698113

## 6.Code to describe and show a strategy for imputing missing data

##### 6.1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nbMissingValues <- sum(is.na (activity$steps)) 
```

* Total number of missing values   :  2304

##### 6.2. Devise a strategy for filling in all of the missing values in the dataset. I use the mean for  that 5-minute interval.


```r
activityImputed <- activity
meanByInterval <- aggregate(steps ~ interval, activity, mean)
```

##### 6.3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityImputed <- merge(x = activity, y = meanByInterval, by = "interval", all.x = TRUE)
activityImputed <-data.frame(
  ifelse(is.na (activityImputed$steps.x), activityImputed$steps.y, activityImputed$steps.x) ,
  activityImputed$date,
  activityImputed$interval
  )
colnames(activityImputed) <- c("steps","date","interval")
```

## 7.Histogram of the total number of steps taken each day after missing values are imputed

##### 7.1. Make a histogram of the total number of steps taken each day 


```r
stepsByIntervalImputed <- aggregate(steps ~ date, activityImputed, sum)

hist(stepsByIntervalImputed$steps, main = paste("Total number of steps taken each day"), col="blue", xlab="Number of Steps")
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


##### 7.2. Calculate and report the mean and median total number of steps taken per day. 



* Mean              :  1.0766189\times 10^{4}
* Median            :  1.0766189\times 10^{4}


##### 7.3. Difference between imputed and non-imputed


```r
result.Diff.mean   <- result.Imputed.mean   - result.mean
result.Diff.median <- result.Imputed.median - result.median
```

* Diff Mean         :  0
* Diff Median       :  1.1886792

Mean value is the same after imputing missing data, as the missing values have been replaced by the mean.
Median value is higher after imputing missing data as the missing interfered in the calculation of the median.

##### 7.4. Impact on the total difference


```r
result.Total.diff <- sum(stepsByIntervalImputed$steps) - sum(stepsByInterval$steps)
```

* Total difference  :  6.4597132\times 10^{5}

## 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

##### 8.1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday 1->5 or weekend day 6,0.


```r
activityImputed$noday <- as.POSIXlt(activityImputed$date)$wday
activityImputed$daytype <- ifelse(activityImputed$noday %in% c(0,6), 'weekend', 'weekday')
```

##### 8.2. Time serie plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends



```r
stepsByIntervalDaytype <- aggregate(steps ~ interval+daytype,activityImputed, mean)

ggplot(data=stepsByIntervalDaytype, aes(x=interval, y=steps , group=daytype, colour=daytype   )
       ) +
  geom_line() +
  ylab(expression('Average step')) +
  ggtitle('Time series plot of the average number of steps taken')  +
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
        )
```

![](Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-17-1.png)<!-- -->



