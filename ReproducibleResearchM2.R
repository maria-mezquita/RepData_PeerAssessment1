#Title: "Reproducible Research: Peer Assessment 1"

library(ggplot2)
library(scales)
library(Hmisc)

#Loading and pre-processing the data

if(!file.exists('activity.csv')){unzip('repdata_data_activity.zip')}
dataSet <- read.csv('activity.csv')
str(dataSet)

#Question 1: What is mean total number of steps taken per day?

sbDay <- tapply(dataSet$steps, dataSet$date, sum, na.rm=TRUE)

qplot(sbDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)

dailyMean <- mean(sbDay)
dailyMedian <- median(sbDay)

#daily mean = 9354.23 & daily median = 10395L

#Question 2: What is the average daily activity pattern?

avgDailyActiv <- aggregate(x=list(meanSteps=dataSet$steps), by=list(interval=dataSet$interval), FUN=mean, na.rm=TRUE)

ggplot(data=avgDailyActiv, aes(x=interval, y=meanSteps)) + geom_line() + xlab("5-minute interval") + ylab("Average of steps taken") 

mStp <- which.max(avgDailyActiv$meanSteps) 
timemStp <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgDailyActiv[mStp,'interval'])

#Imputing missing values

nbMissVal <- length(which(is.na(dataSet$steps)))

DSI <- dataSet
DSI$steps <- impute(dataSet$steps, fun=mean)

I <- tapply(DSI$steps, DSI$date, sum)
qplot(I, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)

dMeanImp <- mean(I)
dMedImp <- median(I)

#Are there differences in activity patterns between weekdays and weekends?

DSI$dateType <-  ifelse(as.POSIXlt(DSI$date)$wday %in% c(0,6), 'weekend', 'weekday')

AVG <- aggregate(steps ~ interval + dateType, data=DSI, mean)
ggplot(AVG, aes(interval, steps)) + geom_line() + facet_grid(dateType ~ .) + xlab("5-minute interval") + ylab("avarage number of steps")



