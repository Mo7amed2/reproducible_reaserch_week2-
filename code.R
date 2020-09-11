getwd()

# Loading the data 
activity <- read.csv("activity.csv")

# Check the data what data looklike 
str(activity)
summary(activity)
head(activity)

# Process/transform the data into a format suitable for your analysis

act.complete <- na.omit(activity)  # data with eliminating missing values 

# Total number of steps 
act.day <- group_by(act.complete, date)
act.day <- summarize(act.day, steps=sum(steps))
summary(act.day)

# Histogram 
qplot(steps, data=act.day)

# Mean & Median 
mean(act.day$steps)
median(act.day$steps)

# Avarage for dialy active
# make the sampling 
act.int <- group_by(act.complete, interval)
act.int <- summarize(act.int, steps=mean(steps))

# plot 
ggplot(act.int, aes(interval, steps)) + geom_line()

## Imputing missing values
##1 Calculate and report the total number of missing values in the dataset
nrow(activity)-nrow(act.complete)

#Devise a strategy for filling in all of the missing values in the dataset
names(act.int)[2] <- "mean.steps"
act.impute <- merge(activity, act.int)

#Create a new dataset that is equal to the original dataset but with the missing data filled in
act.impute$steps[is.na(act.impute$steps)] <- act.impute$mean.steps[is.na(act.impute$steps)]

#Make a histogram of the total number of steps taken each day
act.day.imp <- group_by(act.impute, date)
act.day.imp <- summarize(act.day.imp, steps=sum(steps))

qplot(steps, data=act.day.imp)

mean(act.day.imp$steps)
median(act.day.imp$steps)

# Are there differences in activity patterns between weekdays and weekends
act.impute$dayofweek <- weekdays(as.Date(act.impute$date))
act.impute$weekend <-as.factor(act.impute$dayofweek=="Saturday"|act.impute$dayofweek=="Sunday")
levels(act.impute$weekend) <- c("Weekday", "Weekend")

# Make a panel plot containing a time series plot

act.weekday <- act.impute[act.impute$weekend=="Weekday",]
act.weekend <- act.impute[act.impute$weekend=="Weekend",]

# Then for each one, I find the mean number of steps across days for each 5 minute interval:
act.int.weekday <- group_by(act.weekday, interval)
act.int.weekday <- summarize(act.int.weekday, steps=mean(steps))
act.int.weekday$weekend <- "Weekday"
act.int.weekend <- group_by(act.weekend, interval)
act.int.weekend <- summarize(act.int.weekend, steps=mean(steps))
act.int.weekend$weekend <- "Weekend"

#I append the two data frames together, and I make the two time series plots:
act.int <- rbind(act.int.weekday, act.int.weekend)
act.int$weekend <- as.factor(act.int$weekend)
ggplot(act.int, aes(interval, steps)) + geom_line() + facet_grid(weekend ~ .)

  
