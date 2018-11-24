---
title: "Rep_Research_Assignment#2"
author: "Umesh"
date: "11/22/2018"
output: html_document
---

```{r message=FALSE}
# install.packages("scales")
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse", "rvest", "readr", "data.table", 
              "gridExtra", "ggthemes", "reshape2", 
              "RColorBrewer", "readxl", "scales", "grid", 
              "rbokeh", "shiny", "ggpubr", "corrplot", 
              "lubridate", "knitr", "Hmisc")
ipak(packages)



activity <- read_csv("activity.csv")
```

## Total number of steps taken per day

```{r }
stepsperday <- activity%>%
    group_by(date)%>%
    summarise(StepsperDay = sum(steps, na.rm = TRUE))
```

## Histogram of steps taken per day

```{r}
ggplot(stepsperday, aes(x = StepsperDay)) + 
       geom_histogram(fill = "black", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)") +
        theme_classic()
```

## Mean and Median of the total number of steps taken per day

```{r }
steps_mean   <- mean(stepsperday$StepsperDay, na.rm=TRUE)
steps_median <- median(stepsperday$StepsperDay, na.rm=TRUE)

df<- merge(steps_mean, steps_median)
colnames(df) <- c("Mean", "Median")

kable(df)
```

## What is the average daily activity pattern
```{r }
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)

ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") +  
        theme_classic()

```

## 5-minute interval that contains the maximum number of steps?

```{r }
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])
```

## Number of missing values 

```{r echo=FALSE, results='asis', message=FALSE, warning=FALSE}
nrow(activity[is.na(activity$steps),])

```

## Strategy for filling in all of the missing values in the dataset
```{r }

activityDataImputed <- activity
activityDataImputed$steps <- impute(activity$steps, fun=mean)
```

## histogram of the total number of steps taken each day
```{r }
stepsByDayImputed <- tapply(activityDataImputed$steps, activityDataImputed$date, sum)

qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)+  
        theme_classic()
```
## Mean and median total number of steps taken per day
```{r}
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)
stepsByDayMeanImputed
stepsByDayMedianImputed
```

“weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` {r}
activityDataImputed$dateType <-  ifelse(as.POSIXlt(activityDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

```{r}
averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activityDataImputed, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps") +
    theme_classic()

```
