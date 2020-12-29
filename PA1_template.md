---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---




## Loading and preprocessing the data


Load the necessary libraries

```r
library(tidyverse)
library(janitor)
library(lubridate)
library(knitr)
```

Unzip and load the data

```r
#unzip the data
unzip('activity.zip')

#load the data
data = read_csv('activity.csv') %>%
      clean_names()
```
Check the data


```r
str(data)
```

```
## tibble [17,568 x 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
```


## What is mean total number of steps taken per day?

Calculate the total steps daily

```r
total_steps_daily = data %>%
      group_by(date) %>%
      summarise(total_steps = sum(steps, na.rm = FALSE))
```
Make a histogram of total number of steps taken each day


```r
plot_steps = total_steps_daily %>%
      ggplot(aes(total_steps))+
      geom_histogram(fill = 'steelblue')+
      labs(title = 'Histogram of step count',
           y = 'Frequency of count',
           x = '')+
      theme_minimal()

ggsave('plot_steps.png',plot_steps)
plot_steps
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Calculate and report the median and total number of steps taken per day

```r
steps_summary = total_steps_daily%>%
      summarise(mean_steps = mean(total_steps, na.rm = TRUE),
             median_steps = median(total_steps, na.rm = TRUE)) %>%
      as.list() %>%
      unlist

steps_summary
```

```
##   mean_steps median_steps 
##     10766.19     10765.00
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Calculate the total steps per interval

```r
steps_by_interval = data %>%
      group_by(interval) %>%
      summarise(total_steps = sum(steps, na.rm = TRUE))
```
Plot the number of steps per interval

```r
plot_steps_interval = steps_by_interval %>%
      ggplot(aes(interval, total_steps))+
      geom_line(color = 'steelblue', size = 1.5)+
      labs(title = 'Number of total steps by interval',
           y = 'total steps',
           x = 'interval (mins)')+
      theme_minimal()

ggsave('plot_steps_interval.png',plot_steps_interval)
```

```
## Saving 7 x 5 in image
```

```r
plot_steps_interval
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
Identify which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_max_count = steps_by_interval %>%
      filter(total_steps == max(total_steps)) %>%
      as.list() %>%
      unlist()

interval_max_count
```

```
##    interval total_steps 
##         835       10927
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
map_dbl(data, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Fill NA values using mean of group

```r
filled_data = data %>%
      mutate(steps = as.numeric(steps)) %>%
      group_by(date) %>%
      mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps)) %>%
      mutate(steps = coalesce(steps, 0))
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
filled_data %>%
      write_csv('complete_activity.csv')
```

Compute for the total number of steps per day using the complete data

```r
total_steps_daily_complete =  filled_data %>%
      group_by(date) %>%
      summarise(total_steps = sum(steps, na.rm = TRUE))
```

Make a histogram of total number of steps taken each day


```r
plot_steps_complete = total_steps_daily_complete %>%
      ggplot(aes(total_steps))+
      geom_histogram(fill = 'steelblue')+
      labs(title = 'Histogram of step count',
           y = 'Frequency of count',
           x = '')+
      theme_minimal()

ggsave('plot_steps_complete.png',plot_steps_complete)
plot_steps_complete
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
We can see that there are more zeros now.



```r
steps_summary_complete = total_steps_daily_complete %>%
      summarise(mean_steps = mean(total_steps),
             median_steps = median(total_steps)) %>%
      as.list() %>%
      unlist

steps_summary_complete
```

```
##   mean_steps median_steps 
##      9354.23     10395.00
```
What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
rbind(steps_summary_complete, steps_summary)
```

```
##                        mean_steps median_steps
## steps_summary_complete    9354.23        10395
## steps_summary            10766.19        10765
```

Imputing the data reduced the total mean steps while having similar median steps.

## Are there differences in activity patterns between weekdays and weekends?


Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


Using lubridate, find the day and identify if weekend or weekday

```r
by_week_data = filled_data %>%
      mutate(day = wday(date, label = TRUE)) %>%
      mutate(day_type = factor(ifelse(day == 'Sun' | day == 'Sat', 'weekend', 'weekday')))
```


Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

Plot the graph

```r
weekday_weekend_plot = by_week_data %>%
      group_by(interval, day_type) %>%
      summarise(total_steps = sum(steps, na.rm = TRUE)) %>%
      ggplot(aes(interval, total_steps))+
      geom_line(aes(color = day_type), size = 1.2)+
      facet_grid(day_type ~ ., scales = 'free')+
      labs(title = 'Daily total steps by day type',
           x = '',
           y = 'total steps',
           color = 'day type')+
            theme(legend.position = 'none')

ggsave('weekday_weekend_plot.png',weekday_weekend_plot)

weekday_weekend_plot
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->










