RepData_PeerAssessment1
=======================
*Gdouz*

*February 10,2019*

###Loading and preprocessing the data

```r
setwd("C:\\Users\\14911\\Documents\\Statistics\\研选课\\JHU\\assignment\\RepData_PeerAssessment1")
#Load the data (i.e. read.csv())
act<-read.csv("activity.csv",sep=",",header=TRUE)
#Process/transform the data (if necessary) into a format suitable for your analysis
str(act)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
act$date<-as.character(act$date)
actd<-split(act,act$date)
acti<-split(act,act$interval)
#package ready
library(ggplot2)
library(lattice)
```

###What is mean total number of steps taken per day?(ignore NAs)

```r
sum_pd<-rep(0,61)
for(i in 1:61){
sum_pd[i]<-sum(sapply(actd[[i]]$steps,sum,na.rm=TRUE))}
#Make a histogram of the total number of steps taken each day
qplot(sum_pd)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
png(file="sum_pd.png")
qplot(sum_pd)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```r
dev.off()
```

```
## png 
##   2
```

```r
#Calculate and report the mean and median total number of steps taken per day
mean(sum_pd)
```

```
## [1] 9354.23
```

```r
median(sum_pd)
```

```
## [1] 10395
```

###What is the average daily activity pattern?

```r
mean_pi<-rep(0,288)
for(j in 1:288){
mean_pi[j]<-mean(sapply(acti[[j]]$steps,sum,na.rm=TRUE))}
interval<-act$interval[1:288]
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(interval,mean_pi,type="l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
png(file="mean_pi.png")
plot(interval,mean_pi,type="l")
dev.off()
```

```
## png 
##   2
```

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
interval[which(mean_pi==min(mean_pi),arr.ind=TRUE)]
```

```
##  [1]   40  120  155  200  205  215  220  230  240  245  300  305  310  315
## [15]  350  355  415  500 2310
```

###Imputing missing values

```r
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(act$steps))
```

```
## [1] 2304
```

```r
#Fill in all of the missing values in the dataset with the mean for that 5-minute interval
act_nona<-read.csv("activity.csv",sep=",",header=TRUE)
act_nona$steps[is.na(act$steps)]<-rep(mean_pi,61)[is.na(act$steps)]
act_nona$date<-as.character(act_nona$date)
a_n_d<-split(act_nona,act_nona$date)

#Make a histogram of the total number of steps taken each day 
sum_n_pd<-rep(0,61)
for(i in 1:61){
sum_n_pd[i]<-sum(sapply(a_n_d[[i]]$steps,sum,na.rm=TRUE))}
qplot(sum_n_pd) 
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
png(file="sum_n_pd.png")
qplot(sum_n_pd) 
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```r
dev.off()
```

```
## png 
##   2
```

```r
#Calculate and report the mean and median total number of steps taken per day
mean(sum_n_pd)
```

```
## [1] 10581.01
```

```r
median(sum_n_pd)
```

```
## [1] 10395
```
Mean total number of steps taken per day increases.  
Median total number of steps taken per day remains constant.

###Are there differences in activity patterns between weekdays and weekends?

```r
act_nona$date<-as.Date(act_nona$date)
#Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
act_nona$week<-rep("weekday",17568)
act_nona$week[weekdays(act_nona$date)%in%c("星期六","星期日")]<-"weekend"
act_nona$week<-factor(act_nona$week,levels=c("weekday","weekend"))

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
d_n<-split(act_nona,interval)
mean_nd_pi<-rep(0,288)
mean_ne_pi<-rep(0,288)
for(i in 1:288){
mean_nd_pi[i]<-mean(sapply(subset(d_n[[i]],week=="weekday")$steps,sum,na.rm=TRUE))
mean_ne_pi[i]<-mean(sapply(subset(d_n[[i]],week=="weekend")$steps,sum,na.rm=TRUE))}
wd<-cbind(interval,mean_nd_pi,rep(0,288))
we<-cbind(interval,mean_ne_pi,rep(1,288))
w<-data.frame(rbind(wd,we))
w[,3]<-factor(w[,3])
colnames(w)<-c("interval2","mean_de_pi","de")
attach(w)
```

```
## The following objects are masked from w (pos = 3):
## 
##     de, interval2, mean_de_pi
```

```r
xyplot(mean_de_pi~interval2|de,type="l",layout=c(1,2),xlab="Interval",ylab="Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
png("wdayorend.png")
xyplot(mean_de_pi~interval2|de,type="l",layout=c(1,2),xlab="Interval",ylab="Number of steps")
dev.off()
```

```
## png 
##   2
```

