# Reading the data from C5W2(Course-5-Week-2) folder
Activity <- read.csv("./C5W2 Activity Monitoring/activity.csv", header = TRUE)
head(Activity)
tail(Activity)

## Q1. What is mean total number of steps taken per day?
#For this part of the assignment, you can ignore the missing values in the dataset.

#1. Calculate the total number of steps taken per day
#2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
#3. Calculate and report the mean and median of the total number of steps taken per day

Steps <- aggregate(steps ~ date, Activity, sum, na.rm = TRUE)
#Steps
hist(Steps$steps)
mean(Steps$steps)
median(Steps$steps)

##What is the average daily activity pattern?
#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Step_Avg_TI <- aggregate(steps ~ interval, Activity, mean, na.rm = TRUE)
#Step_Avg_TI
Step_Avg_TI[,2] <- round(Step_Avg_TI[,2], 6)
Max_steps <- round(max(Step_Avg_TI$steps), 6)
Max_interval <- Step_Avg_TI[,Step_Avg_TI$steps == Max_steps]
n <- nrow(Step_Avg_TI)
Max_interval <- Step_Avg_TI[which.max(Step_Avg_TI$steps),]$interval
plot(Step_Avg_TI$interval, Step_Avg_TI$steps, type = "l", xlab = "Various intervals", ylab = "Average number of steps", main = "Average number of steps taken, averaged across all days")
abline( v = Max_interval, col = "red")
Max_interval


##Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

missing_values <- is.na(Activity$steps)
sum(missing_values)

#Filling Missing values by the mean of the particular interval calulated in previous question

NA2MeanInterval<-function(interval){
    Step_Avg_TI[Step_Avg_TI$interval==interval,]$steps
}
Activity_Filled <- Activity
count=0
for(i in 1:nrow(Activity_Filled)){
  if(is.na(Activity_Filled[i,]$steps)){
    Activity_Filled[i,]$steps<-NA2MeanInterval(Activity_Filled[i,]$interval)
    count=count+1
  }
  
}
cat("Total ",count, "NA values were filled.\n\r")

  
Steps2<-aggregate(steps~date,data=activityFilled,sum)
#Steps2
hist(Steps2$steps)
mean(Steps2$steps)
median(Steps2$steps)

##Are there differences in activity patterns between weekdays and weekends?
#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

Activity_Filled$day <- ifelse(as.POSIXlt(as.Date(Activity_Filled$date))$wday%%6==0,
                          "weekend","weekday")
Activity_Filled$day <- factor(Activity_Filled$day,levels=c("weekday","weekend"))

Step_Avg_TI2 <- aggregate(steps~interval+day,Activity_Filled,mean)

library(lattice)
xyplot(steps~interval|factor(day),data=Step_Avg_TI2,aspect=1/2,type="l")
