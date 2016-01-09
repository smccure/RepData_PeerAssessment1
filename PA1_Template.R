
## Loading and preprocessing the data
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")


## What is mean total number of steps taken per day?

total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
plot(total.steps, type="h", main="Total Daily Steps", 
     xlab="Date", ylab="Steps per Day", col="dodgerblue4", lwd=10)
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)


##What is the average daily activity pattern?

averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
plot(averages, type = "l", xlab="5-minute interval", 
     ylab="average number of steps taken averaged across all days", col ="lawngreen")

## maximum number of steps
averages[which.max(averages$steps),]


##Total NAs
sum(is.na(data$steps))


## Imputing missing values
# Replaced NAs with the mean value of its 5-minute interval
NA_value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
NA_data <- data
NA_data$steps <- mapply(NA_value, NA_data$steps, NA_data$interval)


## Plot of the total number of steps taken each day with new data. Also Mean & Median
total.steps <- tapply(NA_data$steps, NA_data$date, FUN=sum)
plot(total.steps, type="h", main="Histogram of Daily Steps", 
     xlab="Date", ylab="Steps", col="dodgerblue4", lwd=8)
mean(total.steps)
median(total.steps)



##Are there differences in activity patterns between weekdays and weekends?
daysofweek <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
NA_data$date <- as.Date(NA_data$date)
NA_data$day <- sapply(NA_data$date, FUN=daysofweek)


## Plot of 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.
library(lattice)
averages <- aggregate(steps ~ interval + day, data=NA_data, mean)
print(xyplot(steps ~ interval | day, layout=c(1,2), xlab="Interval" , ylab="Number of steps", 
       type="l", lty=1,data = averages))


