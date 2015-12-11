library(zoo)
library(TTR)
library(ggplot2)
# load data
# each observation is one hour

data <- read.csv("/home/qnguye/power-stats-total/mge-ups-total", header=FALSE)
pdf("MGE-UPS-power-consumption.pdf",width=8.5,height=11)

names(data)[1]="datetime"
names(data)[2]="watt"
dim(data)

data$datetime = as.POSIXlt(data$datetime,tz="EST",format="%Y/%m/%d %H:%M:%S")
data$year = as.Date(cut(as.Date(data$datetime), breaks="year"))
data$quarter = as.Date(cut(as.Date(data$datetime), breaks="quarter"))
data$month = as.Date(cut(as.Date(data$datetime), breaks="month"))
data$week = as.Date(cut(as.Date(data$datetime), breaks="week"))
data$day = as.Date(cut(as.Date(data$datetime), breaks="day"))
data$hour = as.POSIXlt(cut(as.POSIXlt(data$datetime,format="%Y-%m-%d %H:%M:%S"), breaks="hour"), tz="EST", format="%Y-%m-%d %H:%M:%S")

attach(data)
table(as.Date(datetime))

# data1 = last 45 days
data1 = data[hour > Sys.time() - 60*60*24*45,]

# data2 = last 8 days
data2 = data[hour > Sys.time() - 60*60*24*8,]
summary(data2)
sd(data2$watt)
quantile(data2$watt,0.95)
# get last 36 hours
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

#library(ggplot2)
#library(scales)
# Plot weekly summary
#ggplot(data=data, aes(week,watt)) +
#  stat_summary(fun.y = sum, geom="bar") +
#  scale_y_continuous(labels = comma) +
#  scale_x_date(labels = date_format("%Y-%m"), breaks="1 week") +
#  labs(title="Weekly Power Consumption for MGE 225 KVA UPS")

options(scipen=6)
barplot(tapply(watt,week,sum),
    main="MGE UPS Weekly Power Consumption"    ,
    ylab="Watt",
)

by(watt,month,function(x) {
  c(
    mean=mean(x,na.rm=T), 
    median=median(x,na.rm=T),
    sd=sd(x,na.rm=T), 
    min=min(x,na.rm=T),
    max=max(x,na.rm=T), 
    quantile(x,c(0.5,0.95),na.rm=T),
    sum=sum(x,na.rm=T)
    )
  }
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
     main="MGE UPS power consumption", 
     sub=paste(sub_text,collapse="  to  ")
     )
data.z.SMA = SMA(data.z,n=10)
lines(data.z.SMA,col="red")

hist(coredata(data.z),breaks=20,
     xlab="Date Range",
     main="Histogram of MGE UPS power consumption", 
     sub=paste(sub_text,collapse="  to  "))
# summary statistics
boxplot(coredata(data.z), horizontal=T,
        xlab="Watt",
        main="Box plot of MGE UPS power consumption", 
        sub=paste(sub_text,collapse="  to  "))

summary(watt)
sd(watt)
quantile(watt,0.95)

###Study by day
# last 45 days only

# Boxplot by date
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

#plot(decompose(ts(coredata(data.z), frequency=24, as.Date("2014-10-23 12:30"))),xlab="Day")
plot(decompose(ts(coredata(data.z), frequency=24, start=c(23,12))),xlab="Day")
title(sub=paste(sub_text,collapse="  to  "))

attach(data2)
par(mfrow=c(1,1))
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
     main="MGE UPS power consumption (last 7 days)",
     sub=paste(sub_text,collapse="  to  ")
     )
data.z.SMA = SMA(data.z,n=5)
lines(data.z.SMA,col="red")

###Study for last 36 hours

attach(data3)
par(mfrow=c(1,1))
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
     main="MGE UPS power consumption (last 36 hours)", 
     sub=paste(sub_text,collapse="  to  ")
     )
data.z.SMA = SMA(data.z,n=5)
lines(data.z.SMA,col="red")

dev.off()
