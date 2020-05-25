---
title: "PA1_template"
author: "Ankita Mandal"
date: "5/26/2020"
output: html_document
---
```{r}
library(knitr)
library(ggplot2)
library(data.table)
opts_chunk$set(echo = TRUE, results = 'hold')

data<- read.csv("C:/Users/USER/Documents/activity.csv", header = TRUE, sep=",")
data
data$date <- as.Date(data$date, format="%Y-%m-%d")
data$interval <- as.factor(data$interval)
per_day<- aggregate(steps~date, data=data, FUN = sum)
colnames(per_day)<- c("Date", "Steps")
head(per_day, 10)
ggplot(per_day, aes(x=Steps))+ 
  geom_histogram(fill="red", binwidth = 500)+
  labs(title= "Histogram of Steps Taken per day", x="Steps taken per day", y="Frequency")
mean_per_day<- mean(per_day$Steps)
mean_per_day
median_per_day<- median(per_day$Steps)
median_per_day
steps_interval<- aggregate(steps~interval, data=data, FUN= mean)
steps_interval$interval<- as.integer(levels(steps_interval$interval)[steps_interval$interval])
colnames(steps_interval) <- c("interval", "steps")
head(steps_interval, 10)

ggplot(steps_interval, aes(x=interval, y= steps))+
  geom_line(col="green", size=1)+
  labs(title = "Average Daily Activity Pattern", x="Interval", y="Steps")
na_values<- sum(is.na(data$steps))
na_values
newdataset<- data
indexes<- which(is.na(newdataset$steps))
for(i in indexes)
{
  newdataset$steps[i]<-with(steps_interval, steps[interval=newdataset$interval[i]])
}
head(newdataset, 10)
new_missing_values <- sum(is.na(newdataset$steps))
new_missing_values
new_steps<- aggregate(steps~date, data=newdataset, FUN=sum)
colnames(new_steps)<- c("date", "steps")
head(new_steps)
ggplot(new_steps, aes(x=steps))+
  geom_histogram(col="blue", binwidth = 1000)+
  labs(title = "Histogram of steps taken", x="Steps Per Day", y= "Frequency")
newmean_per_day<- mean(new_steps$steps)
newmean_per_day
newmedian_per_day<- median(new_steps$steps)
newmedian_per_day
dt <- data.table(newdataset)
dt[, weekday := ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")]
dt$weekday <- as.factor(dt$weekday)
dt$interval <- as.integer(levels(dt$interval)[dt$interval])
head(dt, 10)
stepsweekday <- aggregate(steps ~ interval+weekday, data = dt, FUN = mean)
ggplot(stepsweekday, aes(x = interval, y = steps)) + 
  geom_line(col = "blue", size = 1) + 
  facet_wrap(~ weekday, nrow=2, ncol=1) + 
  labs(x = "Interval", y = "Number of Steps")

```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
