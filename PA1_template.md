---
title: "Reproducible Research: Peer Assessment 1"
author: "Poobalan"
date: "September 19, 2015"
output: html_document
keep_md: true
---

## Loading and preprocessing the data
Ensure the relevant libraries are loaded. For this research, the following libraries are used: **knitr**, **dplyr** and **ggplot2**.

```r
library(dplyr)
library(ggplot2)
library(dplyr)
```
Read data set as dataframe ***df***. 

```r
df <- read.csv("repdata-data-activity/activity.csv", header = TRUE)
```
View summary (**summary**),structure (**str**) and sample data (**head**) of dataframe.

```r
summary(df)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(df)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
Note that there are 2304 NAs in ***steps*** column as shown in summary.


## What is mean total number of steps taken per day?
Remove the NAs by selecting complete cases only. Removal of NAs via **complete.cases()** results in 15,264 rows in ***df.complete***. This corresponds with 17,568 rows in original dataset (***df***) minus 2,304 NAs.

```r
df.complete <- df[complete.cases(df),]
nrow(df.complete)
```

```
## [1] 15264
```
Select columns 1 (*steps*) and 2 (*date*) for the purpose of answering this question.

```r
df.complete.step.date <- df.complete[,c(1,2)]
```
Convert to **tbl_df** class of **dplyr**.

```r
tbl <- tbl_df(df.complete.step.date)
```
Calculate the sum of steps and name the column as ***totalsteps***. The sum is grouped by ***date***.

```r
tbl.sumdate <- tbl %>% group_by(date) %>% summarise(totalsteps = sum(steps))
```
The total steps per day as shown in ***tbl.sumdate*** is as follows:

|date       | totalsteps|
|:----------|----------:|
|2012-10-02 |        126|
|2012-10-03 |      11352|
|2012-10-04 |      12116|
|2012-10-05 |      13294|
|2012-10-06 |      15420|
|2012-10-07 |      11015|
|2012-10-09 |      12811|
|2012-10-10 |       9900|
|2012-10-11 |      10304|
|2012-10-12 |      17382|
|2012-10-13 |      12426|
|2012-10-14 |      15098|
|2012-10-15 |      10139|
|2012-10-16 |      15084|
|2012-10-17 |      13452|
|2012-10-18 |      10056|
|2012-10-19 |      11829|
|2012-10-20 |      10395|
|2012-10-21 |       8821|
|2012-10-22 |      13460|
|2012-10-23 |       8918|
|2012-10-24 |       8355|
|2012-10-25 |       2492|
|2012-10-26 |       6778|
|2012-10-27 |      10119|
|2012-10-28 |      11458|
|2012-10-29 |       5018|
|2012-10-30 |       9819|
|2012-10-31 |      15414|
|2012-11-02 |      10600|
|2012-11-03 |      10571|
|2012-11-05 |      10439|
|2012-11-06 |       8334|
|2012-11-07 |      12883|
|2012-11-08 |       3219|
|2012-11-11 |      12608|
|2012-11-12 |      10765|
|2012-11-13 |       7336|
|2012-11-15 |         41|
|2012-11-16 |       5441|
|2012-11-17 |      14339|
|2012-11-18 |      15110|
|2012-11-19 |       8841|
|2012-11-20 |       4472|
|2012-11-21 |      12787|
|2012-11-22 |      20427|
|2012-11-23 |      21194|
|2012-11-24 |      14478|
|2012-11-25 |      11834|
|2012-11-26 |      11162|
|2012-11-27 |      13646|
|2012-11-28 |      10183|
|2012-11-29 |       7047|
  
Build histogram of ***totalsteps*** which represents total steps per day.


```r
ggplot(tbl.sumdate,aes(totalsteps))+geom_histogram(binwidth=600,colour="red",fill="blue") + labs(title= "",y="Count",x="Total steps per day")
```

![plot of chunk build plot](figure/build plot-1.png) 

Based on plot above, total steps per day with highest counts are at around 10,000 steps. The distribution is similar to normal distribution except for high count for 0 steps and the high count for around 15,000 steps.
  
Calculate mean of steps per day using **mean()**

```r
prettyNum(mean(tbl.sumdate$totalsteps))
```

```
## [1] "10766.19"
```
mean steps per day is **10766.19**
  
Calculate median of steps per date using **median()**   

```r
prettyNum(median(tbl.sumdate$totalsteps))
```

```
## [1] "10765"
```
median for steps per day is **10765**


## What is the average daily activity pattern?

Using the dataframe ***df.complete*** (NAs removed), select the relevant columns (1 and 3), which are ***steps*** and ***interval*** and convert the resulting dataframe ***df.complete.step.interval*** into **tbl_df** form

```r
df.complete.step.interval <-df.complete[,c(1,3)]
tbl2 <- tbl_df(df.complete.step.interval)
```
Calculate the mean for steps using **mean()** and name the column as ***meansteps***. The mean value is grouped by ***interval***.

```r
tbl.suminterval <- tbl2 %>% group_by(interval) %>% summarise(meansteps = mean(steps))
```

Build line plot of average steps per interval across all days.

```r
ggplot(tbl.suminterval,aes(x=interval,y=meansteps)) +geom_line(color="red") + labs(title="Average steps per interval",x="Interval",y="Average steps")
```

![plot of chunk plot interval vs average steps](figure/plot interval vs average steps-1.png) 

Based on the plot above, the average daily pattern indicates 0 or minimal steps from 0000 to 500 hours, followed by  a sharp increase in steps between 730 and 1000 hours, peaking at over 200 steps. Beyond that, there is erratic steps averaging between 25 to 100 steps from 1000 to 2000 hours, and tapers off at end of day.

Find the highest average step using **Which.max** and extract the row into new table

```r
max_interval <- tbl.suminterval[which.max(tbl.suminterval$meansteps),]
max_interval
```

```
## Source: local data frame [1 x 2]
## 
##   interval meansteps
## 1      835  206.1698
```
the maximum steps occur for interval **835** which is **206.1698** steps.


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
