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
library(tidyverse)
avg_steps_per_interval <- activityData %>% 
  drop_na() %>% 
  group_by(interval) %>% 
  summarise(average = mean(steps, na.rm = TRUE))
ggplot(avg_steps_per_interval, aes(x = interval , y = average)) + 
  geom_line(color = "steelblue", size = 0.5) + 
  labs(title = "Daily average steps", x = "5-minute interval", y = "Daily average steps")

# 2. Which 5-minute interval, on average across all the days in the dataset, 
#    contains the maximum number of steps?
max_avg_steps_interval <- filter(avg_steps_per_interval, average==max(average))
print(paste("The maximum average number of steps per interval is", max_avg_steps_interval$average))
print(paste("The interval of the maximum average number of steps per interval is", max_avg_steps_interval$interval))
# Use inline code for the report.


# Question 3: Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset 
#    (i.e. the total number of rows with 𝙽𝙰NAs)
total_NAs <- sum(is.na(activityData$steps))
print(paste("The number of missing values is", total_NAs))
# Use inline code for the report.

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
#    The strategy does not need to be sophisticated. For example, you could use 
#    the mean/median for that day, or the mean for that 5-minute interval, etc.
# I will use the mean (average) for that interval as calculated before in avg_steps_per_interval.
# Replacing the missing values by the average values does not affect the new
# averages.

# 3. Create a new dataset that is equal to the original dataset but with the 
#    missing data filled in.
# The new dataset with the missing values replaced is called activityData_NoNAs.
maxIndex <- nrow(avg_steps_per_interval)
activityData_NoNAs <- activityData
for (i in 1:nrow(activityData_NoNAs))
{
  if (is.na(activityData_NoNAs[i,"steps"])==TRUE)
  {
    # Replace the NAs with the average for that interval.
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

# 4. Make a histogram of the total number of steps taken each day and Calculate 
#    and report the mean and median total number of steps taken per day. 
#    Do these values differ from the estimates from the first part of the assignment? 
#    What is the impact of imputing missing data on the estimates of the total daily number of steps?
# Repeat the calculations done in Question 1, but now for the dataset with no NAs.
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

# Comparing the mean and median with NAs and with NAs replaced.
print(paste("The mean of the total number of steps taken per day (NAs included) is:", meanSteps_PerDay))
print(paste("The median of the total number of steps taken per day  (NAs included) is:", medianSteps_PerDay))
meanSteps_PerDay_NoNAs <- mean(total_steps_per_day_NoNAs$steps, na.rm = TRUE)
medianSteps_PerDay_NoNAs <- median(total_steps_per_day_NoNAs$steps, na.rm = TRUE)
print(paste("The mean of the total number of steps taken per day when missing values are replaced is:", meanSteps_PerDay_NoNAs))
print(paste("The median of the total number of steps taken per day when missing values are replaced is:", medianSteps_PerDay_NoNAs))
# As expected the mean (average) number of steps per day is the same.
# The median has become the same as the mean by the chosen imputing strategy.
# The histograms are the same, except that the number of days with 10500-11500 steps 
# has increased with 8 (from 7 to 15). This can be explained as follows:
# There are 8 days with no step measurements for the whole day. All steps are NA
# for these days. These NAs are replaced with the average values. That means that 
# for each day with missing values a total number of steps is added of:
#   sum(avg_steps_per_interval$average)
# which is 10766. So there are 8 days more with 10766 steps, which accounts for
# the increase of 8 in the 10500-11500 range.
summary(total_steps_per_day$steps)  # shiws that there are 8 NAs
summary(total_steps_per_day_NoNAs$steps)

# Question 4: Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset 
# with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
#    indicating whether a given date is a weekday or weekend day.
# Add new column/variable daytype to activityData_NoNAs
library(lubridate)
activityData_NoNAs$daytype <- ifelse(wday(ymd(as.character(activityData_NoNAs$date))) %in% c(1,7), "weekend", "weekday")

# 2. Make a panel plot containing a time series plot (i.e. "type = "l") of the 
#    5-minute interval (x-axis) and the average number of steps taken, averaged 
#    across all weekday days or weekend days (y-axis). See the README file in 
#    the GitHub repository to see an example of what this plot should look like 
#    using simulated data.
avg_steps_per_interval_daytype <- activityData_NoNAs %>% 
  group_by(interval,daytype) %>% 
  summarise(average = mean(steps))

ggplot(avg_steps_per_interval_daytype, aes(interval, average, col = factor(daytype))) +
  facet_grid(daytype~.) +
  geom_line(show.legend = F) +
  labs(title = "Daily average steps", x = "5-minute interval", y = "Daily average steps")

