# Reproducible Research Course, second week Project
Medhat  
August 19, 2017  



## Purpose

This is an R Markdown document. It is written as an assignment for reproducible research course at JHU on coursera. This assignment is intended to train students on usage of R markdown & Knitter.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The requirements of the assignment will be addressed in the following headings.

### Reading the dataset and processing the data

- Set the working directory, download the dataset, unzip file & get the data into working space in "db"" variable


```r
setwd("~/R Directory/C5W2")
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if(!file.exists("activity.zip"))download.file(fileurl, "activity.zip")
if(!file.exists("activity.csv"))read.csv("activity.zip")
if(!exists("db"))db <- read.csv("activity.csv")
```

### Histogram of the total number of steps taken each day

- Change the format of "date"" variable. Calculate total steps taken per day then plot results. It's important to not include spaces in the name of code chunks containing figures as this leads to figures not appearing in the resulting markdown file


```r
db$date <- as.Date(db$date)
sum_steps <- tapply(db$steps, db$date, sum)
qplot(unique(db$date), weight = sum_steps, bins = length(unique(db$date)), col = "red", ylab = "Steps", xlab = "Day", main = "Total steps per day") + theme_bw() + theme(legend.position = "none")
```

![](PA1_template_files/figure-html/Daily_steps_histogram-1.png)<!-- -->

### Mean and median number of steps taken each day

- Calculate the mean and median of the total number of steps taken per day


```r
temp1 <- round(mean(sum_steps, na.rm = TRUE), 2)
temp2 <- median(sum_steps, na.rm = TRUE)
```

Daily steps mean is **10766.19** and median is **10765**

### Time series plot of the average number of steps taken

- Calculate average steps per interval & plot results


```r
av_steps <- tapply(db$steps, db$interval, mean, na.rm = TRUE)
qplot(unique(db$interval), av_steps, col = "red", ylab = "Steps", xlab = "Interval", main = "Average steps per interval", geom = "line") + theme_bw() + theme(legend.position = "none")
```

![](PA1_template_files/figure-html/Time_series_plot-1.png)<!-- -->

### The 5-minute interval that, on average, contains the maximum number of steps

- Calculate interval with maximum average steps


```r
temp3 <- unique(db$interval)[which.max(av_steps)]
```

Interval with maximum average steps is **835**

### Code to describe and show a strategy for imputing missing data

- The total number of missing observations is calculated, then these missing values are substituted by average of same interval across all other days rounding to nearest integer to eliminate fractions in number of steps


```r
temp4 <- sum(is.na(db$steps))
db <- db %>% group_by(interval) %>% mutate(int_av = mean(steps, na.rm = TRUE))
db$steps[is.na(db$steps)] <- round(db$int_av[is.na(db$steps)])
temp5 <- sum(is.na(db$steps))
```

The total number of missing values before imputing was **2304**. This number is perfectly reduced to **0** after imputing

### Histogram of the total number of steps taken each day after missing values are imputed

- Replot the histogram of the total number of steps taken each day & recalculate the mean and median.


```r
sum_steps <- tapply(db$steps, db$date, sum, na.rm = TRUE)
qplot(unique(db$date), weight = sum_steps, bins = length(unique(db$date)), col = "red", ylab = "Steps", xlab = "Day", main = "Total steps per day") + theme_bw() + theme(legend.position = "none")
```

![](PA1_template_files/figure-html/Repeat_daily_steps_histogram-1.png)<!-- -->

```r
temp1 <- round(mean(sum_steps), 2)
temp2 <- median(sum_steps)
```

Daily steps mean after imputing is **10765.64** and median is **10762**. That means that imputing the missing data caused the values of mean & median to be almost equal.

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

- Categorize days into weekdays & weekends, then calculate the average of each category & plot results  


```r
db$dayclass <- weekdays(db$date) != "Saturday" & weekdays(db$date) != "Sunday"
db$dayclass <- factor(db$dayclass, labels = c("weekend", "weekday"))
db <- db %>% group_by(dayclass, interval) %>% mutate(int_av = mean(steps, na.rm = TRUE))
qplot(data = db, x = interval, y = int_av, facets = dayclass ~ ., geom = "line", xlab = "Interval", ylab = "Steps", main = "Average steps per interval", col = "red") + theme_bw() + theme(legend.position = "none")
```

![](PA1_template_files/figure-html/Weekday_weekend_average-1.png)<!-- -->
