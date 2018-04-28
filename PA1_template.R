
#  Project-1 Reproducibility course 

library(lattice) 
setwd("C:\\Users\\hassan\\Desktop\\TEST")
getwd()
#url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
#download.file(url, "ex1.csv", method='curl')
dir()
data<-read.csv("activity.csv",header=T,sep=",")
names(data)
dim(data)
#
# TOTAL STEPS PER DAY- PROBLEM 1-1 --- Total step
m11<-aggregate(steps ~ date ,data, sum, na.rm=TRUE)
hist(m11$steps,xlab = "date",ylab="Total steps",main="Total steps taken per day",
     col="blue")

#======================= Mean and median number of steps taken each day
#=======================================================================
#the mean of the total number of steps taken per day
meanh<-print(mean(m11$steps))
medianh<-print(median(m11$steps))
#========================================================
#========================================================

#=======What is the average daily activity pattern?
m22<-aggregate(steps ~interval, data,mean, na.rm=TRUE)

plot(x=m22$interval,y=m22$steps,type="l",
     xlab = "Interval time",ylab="Average steps",
     main="the average Total steps versus interval time",
     col="red")

Index<-max(m22$steps)
print( m22[ which (m22$steps==Index) , 1] )
#==========================
# Imputing missing values
print( sum_na<-sum(is.na (data$steps)) ) 
vec_na<-which(is.na (data$steps))

#data<-data.matrix(data)

new_data<-data
new_frame<-aggregate(steps ~ date ,data, mean, na.rm=TRUE) 
na<-mean(new_frame$steps)


for (i in 1:length(vec_na)) {
new_data[vec_na[i],1]<-na
            }
   

# check whether all Na data were filled 
sum_na<-sum(is.na (new_data[,1])) 
print(sum_na)
frame_imput<-aggregate(steps ~ date ,new_data, sum, na.rm=TRUE)
hist(frame_imput$steps ,xlab = "step",ylab="frequency",main="Total steps taken per day",
     col="green")

#the mean of the total number of steps taken per day
mean_imput<-print(mean(frame_imput$steps))
median_imput<-print(median(frame_imput$steps))


## Creating the histogram of total steps per day with/without imputing
dev.off()

#plot.new( xaxt = "n", yaxt = "n")  #activate the graphics device in R
hist(frame_imput$steps, breaks=5, xlab="Steps",main="Total steps per day", col="red")
hist(m11$steps, breaks=5, xlab="Steps",main="Total steps per day" , col="blue", add=T)
legend("topright", c("without Na-data", " with NA-data"), fill=c("red", "blue")  )

#=========== activity patterns between weekdays and weekends
new_data$date<-as.Date(new_data$date)
WD<-weekdays(new_data$date)
new_data$date<-WD

for (i in 1:length(WD)) {
        
        if(new_data[i,2]== "Saturday" | new_data[i,2]== "Sunday") {new_data[i,2]<-"weekend" }
            else {new_data[i,2]<-"weekday"}
}


meanmatrix<-aggregate(steps ~ interval+date ,new_data, mean)

xyplot(steps~interval | date, data=meanmatrix,facets= .~ date, type="l",layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")

