
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "repdata-data-activity.zip", mode="wb")
raw_data <- read.csv(unzip("repdata-data-activity.zip"), header = TRUE)

# raw_data <- read.csv("activity.csv", header = TRUE)


head(raw_data)
dim(raw_data)

subdata <- raw_data[complete.cases(raw_data), ]

head(subdata)
dim(subdata)

# totals <- aggregate(steps ~ date, data, sum, na.rm = TRUE)
totals_days <- aggregate(steps ~ date, subdata, sum)

hist(totals_days$steps,  col = "red", main = "Total Steps Per Day", 
     xlab = "Total Number of Dayly Steps", ylab = "Number of Days")

mean(totals_days$steps)
median(totals_days$steps)


totals_intervals <- aggregate(steps ~ interval, subdata, mean)

plot(totals_intervals$interval, totals_intervals$steps, type="l",
     xlab="Number of interval", ylab = "Average number of steps")

totals_intervals$interval[which.max(totals_intervals$steps)]
max(totals_intervals$steps)

sum(!complete.cases(data))

fill_steps <- numeric()
for (i in 1:nrow(data)) {
       observation <- data[i, ]
       if (is.na(observation$steps)) {
             steps <- totals_intervals[totals_intervals$interval == 
                                         observation$interval,]$steps
         } else {
               steps <- observation$steps
           }
       fill_steps <- c(fill_steps, steps)
   }

processed_data <- data
processed_data$steps <- fill_steps

totals_days_new <- aggregate(steps ~ date, processed_data, sum)

hist(totals_days_new$steps,  col = "red", main = "Total Steps Per Day", 
     xlab = "Total Number of Dayly Steps", ylab = "Number of Days")

mean(totals_days_new$steps)
median(totals_days_new$steps)

Sys.setlocale("LC_TIME", "English")
processed_data$date<-as.Date(processed_data$date,format = '%Y-%m-%d')
processed_data$day <- ifelse(weekdays(processed_data$date) 
                             %in% c("Saturday", "Sunday"),'weekend','weekday')
head(processed_data)

totals_intervals_dayweeks<-aggregate(steps ~ interval + day,processed_data, mean)
head(totals_intervals_dayweeks)

library(lattice)
xyplot(steps ~ interval | day, totals_intervals_dayweeks, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")