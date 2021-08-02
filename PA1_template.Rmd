Reproducible Research: Week 2 - Course Project 1
================================
## Loading libraries

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)

```

```{r}
library(ggplot2)
library(dplyr)
```


## Loading data  

1. Code for reading in the dataset and/or processing the data  

```{r}
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/activity.zip",method="curl")

unzip(zipfile="./data/activity.zip",exdir="./data")
activity <- read.csv("./data/activity.csv")
activity$date <- as.Date(activity$date)
```

## Exploratory analysis

2. Histogram of the total number of steps taken each day  

```{r}
steps_day <- activity %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps, na.rm = TRUE)) 


ggplot(data = steps_day) + geom_histogram(aes(total_steps), fill = 23) + xlab("Frequency") +ylab("Steps") +ggtitle("Histogram of the total number of steps taken each day")
```

## Descriptive statistics

3. Mean and median number of steps taken each day  

```{r}
paste0("The mean is: ", round(mean(steps_day$total_steps),2))
paste0("The median is: ", median(steps_day$total_steps))
```


## More graphs

4. Time series plot of the average number of steps taken  

```{r}
steps_interval <- activity %>%
        group_by(interval) %>%
        summarize(mean_steps = mean(steps, na.rm = TRUE)) 

ggplot(data = steps_interval, aes( y=  mean_steps, x = interval)) + geom_line(aes(color= 3)) + xlab("Interval") +ylab("Average number of steps") +ggtitle("Time series plot of the average number of steps taken")

```

5. The 5-minute interval that, on average, contains the maximum number of steps  

```{r}
paste0("The 5-minute interval that contains (on average) the maximum number of steps is:  ", steps_interval$interval[which.max(steps_interval$mean_steps)])
```

## Missing data

6. Code to describe and show a strategy for imputing missing data  

First, I check if there is any missing data and estimate how many there is. There are many method for imputing missing data. As the assignment specifies, it does not need to be a sophisticated method. I will use the average for the associated interval. 

In the next chunk I will:

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
* Create a new dataset that is equal to the original dataset but with the missing data filled in.
* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
sum(is.na(activity))

activity_noMissing <- activity  
for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                activity_noMissing$steps[i]<- steps_interval$mean_steps[activity_noMissing$interval[i] == steps_interval$interval]
        }
}

```


For the next item I will repeat previous work but applying it to the no missing values new data. 

```{r}
steps_day_NONA <- activity_noMissing %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps, na.rm = TRUE))

ggplot(data = steps_day) + geom_histogram(aes(total_steps), fill = 23) + xlab("Frequency") +ylab("Steps") +ggtitle("Histogram of the total number of steps taken each day (without NA)")
```


## Descriptive statistics

Mean and median number of steps taken each day. when compared, both the mean and the median increased after imputing the missing data. Also, the mean suffered a higher increase and it got the same value as the mean, that is, they are equal and less disturbed by outliers.

```{r}
paste0("The mean is: ", round(mean(steps_day_NONA$total_steps),2))
paste0("The median is: ", round(median(steps_day_NONA$total_steps),2))

```

## Analysis by weekends

8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekend

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. Make a panel plot containing a time series plot (i.e. type=“l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
activity_noMissing$date <- as.Date(activity_noMissing$date)
activity_noMissing$day <- ifelse(weekdays(activity_noMissing$date) %in% c("sábado", "domingo"), "weekend", "weekday")
activity_noMissing$day <- as.factor(activity_noMissing$day)


wday <- filter(activity_noMissing, activity_noMissing$day == "weekday")
wend <- filter(activity_noMissing, activity_noMissing$day == "weekend")

wday <- wday %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
wday$day <- "weekday"

wend <- wend %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
wend$day <- "weekend"

wday_Wend <- rbind(wday, wend)
wday_Wend$day <- as.factor(wday_Wend$day)


ggplot (wday_Wend, aes (interval, steps)) + geom_line(aes(color= 3)) + facet_grid (day~.) + 
        theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
        labs(y = "Steps") + labs(x = "Interval") + 
        ggtitle("weekday vs. weekend") 

```

