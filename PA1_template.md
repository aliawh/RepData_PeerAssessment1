# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity <- read.csv('C:/Users/aalhumaidhi/RepData_PeerAssessment1/activity/activity.csv')
```



## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

```r
total_steps_perday <- aggregate(activity$steps, list(activity$date), sum, na.rm = T)
  names(total_steps_perday) <- c('date','total_steps')
  total_steps_perday$total_steps <- as.numeric(total_steps_perday$total_steps)

hist(total_steps_perday$total_steps, 
     main = "Histogram of Total Number of Steps Per Day", 
     xlab = "Total Number of Steps Per Day (n=61)")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

2. **Calculate** and **report** the mean and median total number of steps taken per day 

```r
library(xtable)

mean_steps <- data.frame(Number_of_Days = nrow(total_steps_perday), 
                         Mean_Steps = mean(total_steps_perday$total_steps, 
                                           na.rm = T), 
                         Median_Steps = median(total_steps_perday$total_steps, 
                                               na.rm = T))

xt <- xtable(mean_steps)

print(xt, type="html",include.rownames=FALSE)
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Sun Jul 20 16:26:52 2014 -->
<TABLE border=1>
<TR> <TH> Number_of_Days </TH> <TH> Mean_Steps </TH> <TH> Median_Steps </TH>  </TR>
  <TR> <TD align="right">  61 </TD> <TD align="right"> 9354.23 </TD> <TD align="right"> 10395.00 </TD> </TR>
   </TABLE>


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
mean_steps_perinterval <- aggregate(activity$steps, list(activity$interval), mean, na.rm = T)
  names(mean_steps_perinterval) <- c('interval','mean_steps')

plot(x = mean_steps_perinterval$interval, 
     y = mean_steps_perinterval$mean_steps,
     main = "Mean Steps Per 5 minute Interval (n = 61)",
     xlab = "5-minute Interval",
     ylab = "Mean Steps",
     type = "l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
mean_steps_perinterval$interval[mean_steps_perinterval$mean_steps == max(mean_steps_perinterval$mean_steps)]
```

```
## [1] 835
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I am using the mean for the 5-minute interval over the full dataset to impute the missing values

```r
add_data <- merge(activity, mean_steps_perinterval, all = T)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_new <- add_data

activity_new$steps <- ifelse(is.na(activity_new$steps), 
                             activity_new$mean_steps, 
                             activity_new$steps)

activity_new <- subset(activity_new, select = -c(mean_steps))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
total_steps_perday_new <- aggregate(activity_new$steps, 
                                    list(activity_new$date), 
                                    sum, 
                                    na.rm = T)
  names(total_steps_perday_new) <- c('date', 'total_steps')
  total_steps_perday_new$total_steps <- as.numeric(total_steps_perday_new$total_steps)

hist(total_steps_perday_new$total_steps, 
     main = "Histogram of Total Number of Steps Per Day", 
     xlab = "Total Number of Steps Per Day (n=61)")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


```r
library(xtable)
mean_steps_new <- data.frame(Number_of_Days = nrow(total_steps_perday_new), 
                             Mean_Steps = mean(total_steps_perday_new$total_steps, 
                                               na.rm = T), 
                             Median_Steps = median(total_steps_perday_new$total_steps, 
                                                   na.rm = T))

xt_new <- xtable(mean_steps_new)

print(xt_new, type="html",
      include.rownames=FALSE)
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Sun Jul 20 16:26:53 2014 -->
<TABLE border=1>
<TR> <TH> Number_of_Days </TH> <TH> Mean_Steps </TH> <TH> Median_Steps </TH>  </TR>
  <TR> <TD align="right">  61 </TD> <TD align="right"> 10766.19 </TD> <TD align="right"> 10766.19 </TD> </TR>
   </TABLE>
Do these values differ from the estimates from the first part of the assignment?  
- Yes. The values of mean and median steps per day both increase and converge

What is the impact of imputing missing data on the estimates of the total daily number of steps?  
- The histogram shifts slightly to the right with lower frequency of the smallest bin and higher frequency of the mid-level bins

## Are there differences in activity patterns between weekdays and weekends?
For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activity_new$cat_weekday <- ifelse( weekdays(as.Date(activity_new$date)) %in% c("Saturday","Sunday"), "weekend", "weekday")
head(activity_new)
```

```
##   interval steps       date cat_weekday
## 1        0 1.717 2012-10-01     weekday
## 2        0 0.000 2012-11-23     weekday
## 3        0 0.000 2012-10-28     weekend
## 4        0 0.000 2012-11-06     weekday
## 5        0 0.000 2012-11-24     weekend
## 6        0 0.000 2012-11-15     weekday
```

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
mean_steps_perinterval_new <- aggregate(activity_new$steps, 
                                    list(activity_new$interval,
                                         activity_new$cat_weekday), 
                                    mean, 
                                    na.rm = T)
  names(mean_steps_perinterval_new) <- c('interval','cat_weekday','mean_steps')

library(ggplot2)
ggplot(mean_steps_perinterval_new, aes(x=interval, y = mean_steps))+
  geom_line()+
  facet_grid(cat_weekday ~ .)+
  theme_bw()+
  ggtitle("Mean Steps Per 5 minute Interval (n = 61)")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

