library(zoo)
# load the dataset
data <- read.csv("/home/qnguye/power-stats-total/ibm-pdu-t2-total", 
                 strip.white=TRUE,na.strings=c("", "NA"),header=FALSE)
pdf("ibm-pdu-t2-power-consumption.pdf",width=8.5,height=11)
names(data)[1]="datetime"
names(data)[2]="pdu"
names(data)[3]="watt"
data$datetime = as.POSIXlt(data$datetime,tz="EST",format="%Y/%m/%d %H:%M:%S")
data$year = as.Date(cut(as.Date(data$datetime), breaks="year"))
data$quarter = as.Date(cut(as.Date(data$datetime), breaks="quarter"))
data$month = as.Date(cut(as.Date(data$datetime), breaks="month"))
data$week = as.Date(cut(as.Date(data$datetime), breaks="week"))
data$day = as.Date(cut(as.Date(data$datetime), breaks="day"))
data$hour = as.POSIXlt(cut(as.POSIXlt(data$datetime,tz="EST",format="%Y-%m-%d %H:%M:%S"), breaks="hour"), format="%Y-%m-%d %H:%M:%S")

###ib-1ra5 subset (IB core switch cabinet)

dataibra5 = data[data$pdu == "ib-1ra5",]
attach(dataibra5)

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
     main="IB 1RA5 Rack PDU Power Consumption", 
     sub=paste(sub_text,collapse="  to  ")
     )
summary(watt)
sd(watt)
quantile(watt,0.95)

#Decompose time series

#plot(decompose(ts(coredata(data.z), frequency=24, as.Date("2014-10-23 12:30"))),xlab="Day")
plot(decompose(ts(coredata(data.z), frequency=24, start=c(23,15))),xlab="Day")
title(sub=paste(sub_text,collapse="  to  "))

### lm-4r10 subset (Phase 2 LM nodes)

datalm4r10 = data[data$pdu == "lm-4r10",]
attach(datalm4r10)

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
     main="LM 4R10 Rack PDU Power Consumption", 
     sub=paste(sub_text,collapse="  to  ")
     )
summary(watt)
sd(watt)
quantile(watt,0.95)

#Decompose time series

#plot(decompose(ts(coredata(data.z), frequency=24, as.Date("2014-10-23 12:30"))),xlab="Day")
plot(decompose(ts(coredata(data.z), frequency=24, start=c(23,15))),xlab="Day")
title(sub=paste(sub_text,collapse="  to  "))

dev.off()
