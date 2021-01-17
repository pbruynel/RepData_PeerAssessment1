# Loading and preprocessing the data

# Unzip the activity.zip file
unzip("activity.zip")

# Read the activity.csv file
activityData <- read.csv("activity.csv")

# Question 1: What is mean total number of steps taken per day?
# Ignore NA
# 1. Calculate the total number of steps taken per day
library(dplyr)
total_steps_per_day <- activityData %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps))
# 2. Make a histogram of the total number of steps taken each day
library(ggplot2)
#hist(total_steps_per_day$steps, breaks=10)
ggplot(total_steps_per_day, aes(x = steps)) +
  geom_histogram(fill = "steelblue", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")

# 3. Calculate and report the mean and median of the total number of steps taken per day
meanSteps_PerDay <- mean(total_steps_per_day$steps, na.rm = TRUE)
medianSteps_PerDay <- median(total_steps_per_day$steps, na.rm = TRUE)
print(paste("The mean of the total number of steps taken per day is:", meanSteps_PerDay))
print(paste("The median of the total number of steps taken per day is:", medianSteps_PerDay))
# Use inline code for the report.


# Question 2: What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#    and the average number of steps taken, averaged across all days (y-axis)

# 2. Which 5-minute interval, on average across all the days in the dataset, 
#    contains the maximum number of steps?
