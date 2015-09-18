---
title: 'Reproducible Research: Peer Assessment 1'
author: "Nazri Othman"
date: "September 14, 2015"
output: html_document
---


# **Prepare the R environment**

Throughout this report when writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code.

Set the working directory accordingly then set echo equal to TRUE and results equal a 'hide' as global options for this document. 
```{r}

setwd("~/")
setwd("../RepData_PeerAssessment1")
library(knitr)
opts_chunk$set(echo = TRUE, warning = FALSE)

```

 
# **Loading and Preprocessing the Data**

```{r}

# Clear the workspace
rm(list=ls())

# Load the necessary library
library(ggplot2)

# Unzip the activity dataset
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}

# Load the activity dataset
activityData <- read.csv('activity.csv', stringsAsFactors=FALSE)

# Transform the date attribute to an actual date format
activityData$date <- as.Date(activityData$date, format="%Y-%m-%d")

# Check the data structure
str(activityData)

```

  


## **What is mean total number of steps taken per day?**
 
### *1. Compute total number of steps taken per day (NA values removed)*
```{r}

stepsPerDay <- aggregate(activityData$steps, list(date=activityData$date), sum, na.rm=TRUE)
names(stepsPerDay)[2] <- "steps"

# Check the dataset
head(stepsPerDay)

```
 
### *2. Plot a histogram of total number of steps taken per day*
```{r}

ggplot(data=stepsPerDay, aes(stepsPerDay$steps)) + geom_histogram(aes(fill=..count..), binwidth = 1000) + labs(title = "Histogram of Steps Taken per Day", x = "Number of Steps Taken per Day", y = "Frequency") + theme_bw() 

```
 
### *3. Calculate and report the mean and median for total number of steps taken per day*
```{r}

stepsPerDayMean <- mean(stepsPerDay$steps)
stepsPerDayMedian <- median(stepsPerDay$steps)

```
 
- The **Mean** = `r as.integer(round(stepsPerDayMean))` 
- The **Median** = `r as.integer(round(stepsPerDayMedian))`

  
  
# **What is the average daily activity pattern?**

### *1. Compute number of steps by intervals of 5-minutes*
```{r}

stepsMeanPerInterval <- aggregate(activityData$steps, list(interval=activityData$interval), mean, na.rm=TRUE)
names(stepsMeanPerInterval)[2] <- "stepsmean"

# Check the dataset
head(stepsMeanPerInterval)
```

### *2. Plot the time series of the average number of steps taken (averaged across all days) versus the 5-minute intervals*
```{r}

ggplot(stepsMeanPerInterval, aes(x=interval, y=stepsmean)) + geom_line(color="blue") + labs(title="Average Daily Activity Pattern", x="5-Minute Intervals", y="Mean Number of Steps") +  theme_bw()

```

### *3. Calculate the 5-minute interval with the containing the maximum number of steps*
```{r}

maxStepsInterval <- stepsMeanPerInterval[which.max(stepsMeanPerInterval$stepsmean),]

```

The max steps is **`r round(maxStepsInterval$stepsmean)`** at the **`r maxStepsInterval$interval`**th interval.

  

## **Imputing missing values**

### *1. Total number of missing values*
```{r}

missingValue <- as.numeric(sum(is.na(activityData$steps)))

```

The total number of missing values are **`r missingValue`**.


### *2. Strategy for filling in all of the missing values in the dataset*
```{r}

# merge activity dataset with steps mean dataset by interval
activityData <- merge(activityData, stepsMeanPerInterval, by="interval")

# fill in missing value (NA) with a value of steps mean per interval 
for (i in 1:17568) {
  
  if(is.na(activityData$steps[i])){
     
    activityData$steps[i] <- activityData$stepsmean[i]}
    
  }

# Check whether there are still NA 
summary(activityData)

```

### *3. Compute total number of steps taken per day (NA values replaced with Steps Mean per interval)*
```{r}

stepsPerDay <- aggregate(activityData$steps, list(date=activityData$date), sum)
names(stepsPerDay)[2] <- "steps"

# Check the dataset
head(stepsPerDay)

```
 
### *4. Plot a histogram of total number of steps taken per day*
```{r}

ggplot(data=stepsPerDay, aes(stepsPerDay$steps)) + geom_histogram(aes(fill=..count..), binwidth = 1000) + labs(title = "Histogram of Steps Taken per Day\n (with replaced NA value)", x = "Number of Steps Taken per Day", y = "Frequency") + theme_bw() 

```

Compute the mean and median
```{r}

stepsPerDayMean <- mean(stepsPerDay$steps)
stepsPerDayMedian <- median(stepsPerDay$steps)

```
 
The strategy filling in NA value with steps mean per interval gives a mean and median of **`r as.integer(round(stepsPerDayMean))`** and **`r as.integer(round(stepsPerDayMedian))`** respectively.

These values differ from the first part of the assignment. The impact of imputing the missing values is to have similar mean and median value.





## **Are there differences in activity patterns between weekdays and weekends?**

### *1. Compute the weekday and weekend*
```{r}

# Compute the weekdays from the date attribute
activityData$weekday <- weekdays(activityData$date)

# Compute the day type (weekend or weekday)
activityData$daytype <- ifelse(activityData$weekday == "Saturday" | activityData$weekday == "Sunday", "weekend", "weekday")

# Check the dataset
head(activityData)

```

### *2. Compute number of steps by intervals of 5-minutes*
```{r}

stepsMeanPerInterval <- aggregate(activityData$steps, list(interval=activityData$interval, daytype=activityData$daytype), mean)
names(stepsMeanPerInterval)[3] <- "stepsmean"

# Check the dataset
head(stepsMeanPerInterval)

```


### *3. Plot the time series of the average number of steps taken (averaged across all days) versus the 5-minute intervals*
```{r}

ggplot(stepsMeanPerInterval, aes(x=interval, y=stepsmean)) + geom_line(color="blue") + labs(title="Average Daily Activity Pattern\n (with replaced NA value)", x="5-Minute Intervals", y="Mean Number of Steps") + facet_wrap(~daytype, ncol=1) + theme_bw()

```

The steps taken during the weekend is slightly higher than weekday but both weekday and weekend activity start and finish more or less at similar time.
