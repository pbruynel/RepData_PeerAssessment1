---
title: "Reproducible Research: Peer Assessment 1"
author: 'Paul Bruynel'
date: 'Jan 20th, 2021'
output: 
  html_document:
    keep_md: true
---

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](https://www.fitbit.com/home), [Nike Fuelband](https://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as *NA*) </br>
- **date**: The date on which the measurement was taken in YYYY-MM-DD format </br>
- **interval**: Identifier for the 5-minute interval in which measurement was taken </br>
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.



## Loading and preprocessing the data


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

## What is mean total number of steps taken per day?
To answer this question first the total number of steps that were taken each day is calculated.

```r
total_steps_per_day <- activityData %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps))
```
The histogram below shows frequency of the number of steps taken each day.

```r
ggplot(total_steps_per_day, aes(x = steps)) +
  geom_histogram(fill = "steelblue", binwidth = 1000) +
  labs(title = "Frequency of daily Steps", x = "Steps (binwidth=1000)", y = "Frequency")
```

![](PA1_report_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
# Calculate and report the mean and median of the total number of steps taken per day.
meanSteps_PerDay <- mean(total_steps_per_day$steps, na.rm = TRUE)
medianSteps_PerDay <- median(total_steps_per_day$steps, na.rm = TRUE)
```
The mean of the total number of steps taken per day is: 10766.19  
The median of the total number of steps taken per day is: 10765


## What is the average daily activity pattern?
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

![](PA1_report_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
max_avg_steps_interval <- filter(avg_steps_per_interval, average==max(average))
max_average <- max_avg_steps_interval$average
max_interval <- max_avg_steps_interval$interval
```
The maximum average number of steps that have been taken in an interval: 206.17  
The interval where the maximum average number of steps have been taken is: 835  


## Imputing missing values
There are a number of days/intervals where there are missing values (coded as 𝙰NA). The presence of missing days may introduce bias into some calculations or summaries of the data. In this section the missing values are imputed to see what effect the missing values have on the calculations.  

```r
total_NAs <- sum(is.na(activityData$steps))
```
The number of missing values in the original dataset is 2304  

The strategy for dealing with missing values is to replace each missing value with the average number of steps for the interval for which the number of steps are missing. In this way the new calculated averages values are not affected. They should be the same as the averages calculated before the replacement of missing values.

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
To show the effect of the imputing strategy the histogram is shown below again, this time for the dataset with no missing values.


```r
# Calculate the total steps per day for the new dataset
total_steps_per_day_NoNAs <- activityData_NoNAs %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps))

# With this new dataset make a histogram of the total number of steps taken each day.
ggplot(total_steps_per_day_NoNAs, aes(x = steps)) +
  geom_histogram(fill = "steelblue", binwidth = 1000) +
  labs(title = "Frequency of daily Steps (missing values are replaced)", x = "Steps (binwidth=1000)", y = "Frequency")
```

![](PA1_report_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
# Comparing the mean and median with NAs and with NAs replaced.
meanSteps_PerDay_NoNAs <- mean(total_steps_per_day_NoNAs$steps, na.rm = TRUE)
medianSteps_PerDay_NoNAs <- median(total_steps_per_day_NoNAs$steps, na.rm = TRUE)
#print(paste("The mean of the total number of steps taken per day when missing values are replaced is:", meanSteps_PerDay_NoNAs))
#print(paste("The median of the total number of steps taken per day when missing values are replaced is:", medianSteps_PerDay_NoNAs))
```


## Are there differences in activity patterns between weekdays and weekends?



