library(zoo)
library(TTR)
library(lattice)
library(ggplot2)
library(scales)

data <- read.csv("/home/qnguye/power-stats-total/total", header=FALSE)
pdf("/home/qnguye/power-stats-reports/IBM-servers-power-consumption.pdf",width=8.5,height=11)

names(data)[1]="datetime"
names(data)[2]="watt"
names(data)[3]="btu"
names(data)[4]="minwatt"
names(data)[5]="maxwatt"
names(data)[6]="maxnode"
names(data)[7]="N"

data$datetime = as.POSIXlt(data$datetime,tz="EST",format="%Y/%m/%d %H:%M:%S")
data$year = as.Date(cut(as.Date(data$datetime), breaks="year"))
data$quarter = as.Date(cut(as.Date(data$datetime), breaks="quarter"))
data$month = as.Date(cut(as.Date(data$datetime), breaks="month"))
data$week = as.Date(cut(as.Date(data$datetime), breaks="week"))
data$day = as.Date(cut(as.Date(data$datetime), breaks="day"))
data$hour = as.POSIXlt(cut(as.POSIXlt(data$datetime,format="%Y-%m-%d %H:%M:%S"), breaks="hour"),tz="EST",format="%Y-%m-%d %H:%M:%S")

attach(data)

dim(data)
summary(data)
sd(data$watt)
quantile(data$watt,0.95)

# get clean data after Nov 1
# data2 = data[hour > as.POSIXlt("2014-10-31 23:59:59"),]

# get data for last 45 days
data1 = data[hour > Sys.time() - 60*60*24*45,]

# get last 8 days
#data2 = data[as.POSIXct(hour) > Sys.time() - 60*60*24*8,]
data2 = data[hour > Sys.time() - 60*60*24*8,]
summary(data2)
sd(data2$watt)
quantile(data2$watt,0.95)

# get last 36 hours
#data3 = data[as.POSIXct(hour) > Sys.time() - 60*60*36,]
data3 = data[hour > Sys.time() - 60*60*36,]

###Plot total Watt for each hour

# Time Series Analysis
dt = as.POSIXlt(datetime)
data.z = zoo(x=watt,order.by=dt)
class(data.z)
head(index(data.z))
head(coredata(data.z))
sub_text = range(index(data.z))

# Summary by month/week/...
by(watt,week,summary)
by(watt,week,sum)
by(watt,month,summary)
by(watt,month,sum)
#tapply(watt,as.Date(month),summary)
#tapply(watt,as.Date(week),summary)
#tapply(watt,as.Date(month),sum)
#tapply(watt,as.Date(week),sum)

#library(ggplot2)
#library(scales)
# Plot weekly summary
#ggplot(data=data, aes(week,watt)) +
#  stat_summary(fun.y = sum, geom="bar") +
#  scale_y_continuous(labels = comma) +
#  scale_x_date(labels = date_format("%Y-%m"), breaks="1 week") +
#  labs(title="Weekly Power Consumption for all IBM Servers")
options(scipen=6)
barplot(tapply(watt,week,sum),
    main="Weekly Power Consumption",
    ylab="Watt"
)

# Time Series Analysis
dt = as.POSIXlt(datetime)
data.z = zoo(x=watt,order.by=dt)
class(data.z)
head(index(data.z))
head(coredata(data.z))
sub_text = range(index(data.z))
plot(data.z,
     xlab="Date Range",
     ylab="Watt",
     #xaxt='n',
     main="All IBM Servers Power Consumption", 
     sub=paste(sub_text,collapse="  to  ")
     )
data.z.SMA = SMA(data.z,n=20)
lines(data.z.SMA,col="red")

dataf36 = head(data,36)
sub_text = range(as.POSIXlt(dataf36$datetime))
plot(as.POSIXlt(dataf36$datetime), dataf36$watt,
     type='l',
     xlab="DateTime",
     ylab="Watts",
     #xaxt='n',
     main="IBM servers power consumption for first 36 hours", 
     sub=paste(sub_text,collapse="  to  "))
# summary statistics for first 36 hours
summary(dataf36$watt)

###Study for Oct 25

data25 = data[as.Date(data$datetime) == "2014-10-25", ]
sub_text = range(as.POSIXlt(data25$datetime))
plot(as.POSIXlt(data25$datetime), data25$watt,
     type='l',
     xlab="Hour",
     ylab="Watts",
     #xaxt='n',
     main="IBM servers power consumption for October 25, 2015", 
     sub=paste(sub_text,collapse="  to  "))
# summary statistics for Oct 25
summary(data25,watt)

###Study by day

# Boxplot by date
# last 45 days only
boxplot(data1$watt ~ as.Date(data1$datetime))

by(data1$watt,as.Date(data1$datetime),summary)

# will not print the date 
#par(mfrow=c(1,3))
#tapply(watt,as.Date(datetime),function(x)
#  plot(x,
#       type='l',
#       xlab="Hour",
#       ylab="Watts",
#       #xaxt='n'
#       main="IBM servers power consumption by day"
#       )
#)

qplot(datetime,watt,data=data1,geom="line", col="red") + facet_wrap(~day, scales='free_x', ncol=7)

###Study by week

par(mfrow=c(1,1))
boxplot(watt ~ week)
par(mfrow=c(1,3))
# will not print the week
#tapply(watt,as.Date(week),function(x) 
#  plot(x,
#       type='l',
#       xlab="Hour",
#       ylab="Watts",
#       #xaxt='n'
#       main="IBM servers power consumption by week"
#       )
#  )
qplot(datetime,watt,data=data,geom="line", col="red") + facet_wrap(~week, scales='free_x', ncol=4)

###Study by month

par(mfrow=c(1,1))
boxplot(watt ~ month)
par(mfrow=c(1,3))
# will not print the month
#tapply(watt,as.Date(month),function(x) 
#  plot(x,
#       type='l',
#       xlab="Hour",
#       ylab="Watts",
#       #xaxt='n'
#       main="IBM servers power consumption by month"
#       )
#  )
qplot(datetime,watt,data=data,geom="line", col="red") + facet_wrap(~month, scales='free_x', ncol=1)

###Decompose time series

par(mfrow=c(1,1))
#plot(decompose(ts(coredata(data.z), frequency=24, as.Date("2014-10-23 12:30"))),xlab="Day")
plot(decompose(ts(coredata(data.z), frequency=24, start=c(23,12))),xlab="Day")
title(sub=paste(sub_text,collapse="  to  "))

###Study for last 8 days

attach(data2)
dt = as.POSIXlt(datetime)
data.z = zoo(x=watt,order.by=dt)
class(data.z)
head(index(data.z))
head(coredata(data.z))
sub_text = range(index(data.z))
plot(data.z,
     xlab="Date Range",
     ylab="Watt",
     #xaxt='n',
     main="All IBM Servers Power Consumption (last 7 days)", 
     sub=paste(sub_text,collapse="  to  ")
     )
data.z.SMA = SMA(data.z,n=10)
lines(data.z.SMA,col="red")

###Study for last 36 hours

attach(data3)
dt = as.POSIXlt(datetime)
data.z = zoo(x=watt,order.by=dt)
class(data.z)
head(index(data.z))
head(coredata(data.z))
sub_text = range(index(data.z))
plot(data.z,
     xlab="Date Range",
     ylab="Watt",
     #xaxt='n',
     main="All IBM Servers Power Consumption (last 36 hours)", 
     sub=paste(sub_text,collapse="  to  ")
     )
data.z.SMA = SMA(data.z,n=5)
lines(data.z.SMA,col="red")

dev.off()
