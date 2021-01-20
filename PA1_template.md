---
title: "Reproducible Research: Peer Assessment 1"
author: 'Paul Bruynel'
date: 'Jan 20th, 2021'
output: 
  html_document:
    keep_md: true
---

### Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](https://www.fitbit.com/home), [Nike Fuelband](https://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as *NA*) </br>
- **date**: The date on which the measurement was taken in YYYY-MM-DD format </br>
- **interval**: Identifier for the 5-minute interval in which measurement was taken </br>
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.



### Loading and preprocessing the data


```r
# Loading the necessary libraries.
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
```

The data is already available in the provided activity.zip file. First the data is extracted and read.

```r
unzip("activity.zip")
activityData <- read.csv("activity.csv")
```

### What is the mean total number of steps taken per day?
To answer this question first the total number of steps that were taken each day is calculated.

```r
total_steps_per_day <- activityData %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps))
```
The histogram below shows the frequency of the number of steps taken each day.

```r
ggplot(total_steps_per_day, aes(x = steps)) +
  geom_histogram(fill = "steelblue", binwidth = 1000) +
  labs(title = "Frequency of daily steps", x = "Steps (binwidth=1000)", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
# Calculate and report the mean and median of the total number of steps taken per day.
meanSteps_PerDay <- mean(total_steps_per_day$steps, na.rm = TRUE)
medianSteps_PerDay <- median(total_steps_per_day$steps, na.rm = TRUE)
```
The mean of the total number of steps taken per day is: 10766.19  
The median of the total number of steps taken per day is: 10765


### What is the average daily activity pattern?
To gain insight of the daily activity pattern the average number of steps per 5-minute interval taken daily is calculated and plotted.

```r
avg_steps_per_interval <- activityData %>% 
  drop_na() %>% 
  group_by(interval) %>% 
  summarise(average = mean(steps, na.rm = TRUE))
ggplot(avg_steps_per_interval, aes(x = interval , y = average)) + 
  geom_line(color = "steelblue", size = 0.5) + 
  labs(title = "Daily average number of steps by 5-minute interval", x = "5-minute interval", y = "Daily average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
max_avg_steps_interval <- filter(avg_steps_per_interval, average==max(average))
max_average <- max_avg_steps_interval$average
max_interval <- max_avg_steps_interval$interval
```
The maximum average number of steps that have been taken in an interval: 206.17  
The interval where the maximum average number of steps have been taken is: 835  


### Imputing missing values
There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data. In this section the missing values are imputed to see what effect the missing values have on the calculations.  

```r
total_NAs <- sum(is.na(activityData$steps))
```
The number of missing values in the original dataset is 2304  

The strategy for dealing with missing values is to replace each missing value with the average number of steps for the interval for which the number of steps are missing. In this way the new calculated average values are not affected. They should be the same as the averages calculated before the replacement of missing values.

```r
# avg_steps_per_interval holds the average number of daily steps for each interval
maxIndex <- nrow(avg_steps_per_interval)
# Create a copy of the original dataset and replace the missing values.
activityData_NoNAs <- activityData
for (i in 1:nrow(activityData_NoNAs))
{
  if (is.na(activityData_NoNAs[i,"steps"])==TRUE)
  {
    # Replace the NA with the average for that interval.
    # Calculate the index of the average value to be used to replace the missing value.
    index <- i%%maxIndex  
    if (index==0)
    {
      index <- maxIndex
    }
    # Now replace the missing value with the average for that interval.
    activityData_NoNAs[i,"steps"] <- avg_steps_per_interval[index,"average"]
  } 
}
```
To be sure that there are no more missing values in the newly created dataset we check the number of NA's of that dataset.

```r
total_NAs <- sum(is.na(activityData_NoNAs$steps))
```
The number of missing values in the newly created dataset is 0  
So all missing values have been replaced.  
To show the effect of the imputing strategy the histogram is shown below again, this time for the dataset with the missing values replaced.


```r
# Calculate the total steps per day for the new dataset
total_steps_per_day_NoNAs <- activityData_NoNAs %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps))

# Show in a histogram both the dataset with NA's and the dataset with the NA's replaced
total_steps_per_day_NoNAs$NAs <- '1'
total_steps_per_day$NAs <- '2'
combined <- rbind(total_steps_per_day, total_steps_per_day_NoNAs)
ggplot(combined, aes(steps, fill = NAs)) + 
  geom_histogram(position = 'identity', binwidth = 1000) +
  scale_fill_discrete(name=NULL,
    breaks=c("1", "2"),
    labels=c("Replaced NA's", "Original values")) +
  labs(title = "Frequency of daily Steps (missing values are replaced)", x = "Steps (binwidth=1000)", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
# Calculate the mean and median of the new dataset with NAs replaced.
meanSteps_PerDay_NoNAs <- mean(total_steps_per_day_NoNAs$steps)
medianSteps_PerDay_NoNAs <- median(total_steps_per_day_NoNAs$steps)
```
Mean and median before replacing missing values:  
- Mean: 10766.19  
- Median: 10765  
Mean and median after replacing missing values:  
- Mean: 10766.19  
- Median: 10766.19  
  
As expected the mean (average) number of steps per day is not affected by the chosen imputing strategy.
The new median however has taken the same value as the mean.  
The histograms shows that nothing changed, except that the number of days with 
10500-11500 steps has increased with 8 (from 7 to 15). This can be explained as follows:  
There are 8 days with no step measurements for the whole day as the following shows:

```r
summary(total_steps_per_day$steps)  # shows that there are 8 NAs
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```
While the new dataset does not contain any NA's.

```r
summary(total_steps_per_day_NoNAs$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

All missing values for the steps have been replaced by the average values.
That means that for each day with missing values the total number of steps is increased by:

```r
sum(avg_steps_per_interval$average)
```

```
## [1] 10766.19
```

So there are 8 days more with 10766 steps, which accounts for the increase of 8 
in the 10500-11500 range of the histogram.


### Are there differences in activity patterns between weekdays and weekends?
To answer this question the dataset is split in two parts: one part contains the
data for the weekdays (Monday - Friday), the other part contains the data for the 
weekends (Saturday - Sunday). The figure below shows a plot for both parts.

```r
# Add new column/variable daytype to activityData_NoNAs
activityData_NoNAs$daytype <- ifelse(wday(ymd(as.character(activityData_NoNAs$date))) %in% c(1,7), "weekend", "weekday")
# Calculate the average number of steps for both types of days
avg_steps_per_interval_daytype <- activityData_NoNAs %>% 
  group_by(interval,daytype) %>% 
  summarise(average = mean(steps))
# Show the results in a plot.
ggplot(avg_steps_per_interval_daytype, aes(interval, average, col = factor(daytype))) +
  facet_grid(daytype~.) +
  geom_line(show.legend = F) +
  labs(title = "Daily average steps", x = "5-minute interval", y = "Daily average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
  
The plots show some differences in activities on weekdays and on weekends. The shapes
are roughly the same with a peak around 9 AM. On weekends the activity during office
hours is higher than on weekdays, which could be explained by the fact that on weekdays
the subject is working and that his/her job does not require him/her to walk around
a lot. But we do not know that for sure.


