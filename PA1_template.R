library(ggplot2)

## Download and extract zip file
if (!file.exists("data")) {
  dir.create("data")
}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" ##testdata
download.file(fileUrl,destfile="./data/Factivity.zip")

unzip(zipfile="./data/Factivity.zip",exdir="./data")

## Loading and preprocessing the data
## Load the data (i.e. read.csv())
f_activity <- read.csv("data/activity.csv")

## Process/transform the data (if necessary) into a format suitable for your analysis
f_activity$date <- as.Date(f_activity$date,format="%Y-%m-%d")

## What is the mean total number of steps taken per day?
## Calculate the total number of steps taken per day.
totalSteps <- tapply(f_activity$steps,f_activity$date,sum,na.rm=TRUE)

## Make a histogram of the total number of steps taken each day.
qplot(totalSteps,geom="histogram",main="Total Number of Steps (per day)",
      xlab='Steps', 
      ylab='Frequency',
      binwidth=1000,
      fill=I("darkgreen"),
      colour=I("white"))
## Calculate and report the mean and median of the total number of steps taken per day
meanSteps <- mean(totalSteps) ##9354.23
medianSteps <- median(totalSteps) ##10395
print(meanSteps)
print(medianSteps)

## What is the average daily activity pattern?
## Make a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and 
## the average number of steps taken, averaged across all days (y-axis)
averageStepsInterval <- aggregate(steps ~ interval,f_activity,mean)
plot(averageStepsInterval$interval,averageStepsInterval$steps,type="l",col="darkgreen",
     main="Average Steps by Interval (5-minutes)",xlab="Interval",ylab="Steps")

## Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?
topInterval <- which.max(averageStepsInterval$steps)
print(averageStepsInterval[topInterval, ]$interval) ##Top Interval: 835
print(averageStepsInterval[topInterval, ]$steps) ##Top Steps: 206.1698

## Imputing missing values
## Note that there are a number of days/intervals where there are missing values.
## The presence of missing days may introduce bias into some calculations or 
## summaries of the data.
## Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with NAs)
missingValues <- f_activity[!complete.cases(f_activity), ]
nrow(missingValues) ##2304

for (i in 1:nrow(f_activity)) {
  if(is.na(f_activity$steps[i])) {
    value <- averageStepsInterval$steps[which(averageStepsInterval$interval == f_activity$interval[i])]
    f_activity$steps[i] <- value
  }
}

## Create a new dataset that is equal to the original dataset but with the missing data filled in.
imputeSteps <- aggregate(steps ~ date,f_activity,sum)
qplot(imputeSteps$steps, main="Steps Per Day (imputed)",
    binwidth=1000,
    xlab="Steps (per day)",
    ylab="Frequency",
    fill=I("darkgreen"),
    colour=I("white"))

## Mean and Median have NOT changed.
meanImputeSteps <- mean(imputeSteps) ##9354.23
medianImputeSteps <- median(imputeSteps) ##10395
print(meanSteps)
print(medianSteps)

## Are there differences in activity patterns between weekdays and weekends?
## Create a new factor variable in the dataset with two levels - "weekday" and 
## "weekend" indicating whether a given date is a weekday or weekend day.
weekDay <- function(date_val) {
  weekDays <- weekdays(as.Date(date_val, '%Y-%m-%d'))
  if  (!(weekDays == 'Saturday' || weekDays == 'Sunday')) {
    x <- 'Weekday'
  } else {
    x <- 'Weekend'
  }
  x
}

f_activity$day_type <- as.factor(sapply(f_activity$date,weekDay))
imputeSteps <- aggregate(steps ~ interval+day_type,f_activity,mean)

## Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval 
## (x-axis) and the average number of steps taken, averaged across all weekday days or weekend 
## days (y-axis). See the README file in the GitHub repository to see an example of what this 
## plot should look like using simulated data.
panelPlot <- ggplot(imputeSteps, aes(interval, steps)) +
  geom_line(stat="identity",aes(color=day_type))+
  facet_grid(day_type ~ ., scales="fixed", space="fixed") +
  labs(x="Interval", y=("Steps")) +
  ggtitle("Weekday vs. Weekend")
print(panelPlot)
