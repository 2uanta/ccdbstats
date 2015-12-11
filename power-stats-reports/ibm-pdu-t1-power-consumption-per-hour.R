library(zoo)
# load the dataset
data <- read.csv("/home/qnguye/power-stats-total/ibm-pdu-t1-total", 
                 strip.white=TRUE,na.strings=c("", "NA"),header=FALSE)
pdf("ibm-pdu-t1-power-consumption.pdf",width=8.5,height=11)

names(data)[1]="datetime"
names(data)[2]="pdu"
names(data)[3]="watt"
data$datetime = as.POSIXlt(data$datetime,tz="EST",format="%Y/%m/%d %H:%M:%S")
data$year = as.Date(cut(as.Date(data$datetime), breaks="year"))
data$quarter = as.Date(cut(as.Date(data$datetime), breaks="quarter"))
data$month = as.Date(cut(as.Date(data$datetime), breaks="month"))
data$week = as.Date(cut(as.Date(data$datetime), breaks="week"))
data$day = as.Date(cut(as.Date(data$datetime), breaks="day"))
data$hour = as.POSIXlt(cut(as.POSIXlt(data$datetime,format="%Y-%m-%d %H:%M:%S"), breaks="hour"), tz="EST", format="%Y-%m-%d %H:%M:%S")

###st-1r01 subset (GSS cabinet)

datast1r01 = data[data$pdu == "st-1r01",]
attach(datast1r01)

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
     main="ST-1R01 Rack PDU Power Consumption", 
     sub=paste(sub_text,collapse="  to  ")
     )
summary(watt)

#Decompose time series

#plot(decompose(ts(coredata(data.z), frequency=24, as.Date("2014-10-23 12:30"))),xlab="Day")
plot(decompose(ts(coredata(data.z), frequency=24, start=c(23,15))),xlab="Day")
title(sub=paste(sub_text,collapse="  to  "))


###st-1r02 subset (GSS cabinet)

datast1r02 = data[data$pdu == "st-1r02",]
attach(datast1r02)

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
     main="ST-1R02 Rack PDU Power Consumption", 
     sub=paste(sub_text,collapse="  to  ")
     )
summary(watt)

#Decompose time series

#plot(decompose(ts(coredata(data.z), frequency=24, as.Date("2014-10-23 12:30"))),xlab="Day")
plot(decompose(ts(coredata(data.z), frequency=24, start=c(23,15))),xlab="Day")
title(sub=paste(sub_text,collapse="  to  "))


###st-1r04 subset (GSS cabinet)

datast1r04 = data[data$pdu == "st-1r04",]
attach(datast1r04)

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
     main="ST-1R04 Rack PDU Power Consumption", 
     sub=paste(sub_text,collapse="  to  ")
     )
summary(watt)

#Decompose time series

#plot(decompose(ts(coredata(data.z), frequency=24, as.Date("2014-10-23 12:30"))),xlab="Day")
plot(decompose(ts(coredata(data.z), frequency=24, start=c(23,15))),xlab="Day")
title(sub=paste(sub_text,collapse="  to  "))


###st-1r05 subset (GSS cabinet)

datast1r05 = data[data$pdu == "st-1r05",]
attach(datast1r05)

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
     main="ST-1R04 Rack PDU Power Consumption", 
     sub=paste(sub_text,collapse="  to  ")
     )
summary(watt)

#Decompose time series

#plot(decompose(ts(coredata(data.z), frequency=24, as.Date("2014-10-23 12:30"))),xlab="Day")
plot(decompose(ts(coredata(data.z), frequency=24, start=c(23,15))),xlab="Day")
title(sub=paste(sub_text,collapse="  to  "))

### All GSS cabinets

# aggregate would change the watt variable name to x unless we redefine it in a list
data_aggr = aggregate(list(watt=data$watt),by=list(hour=as.POSIXct(data$hour)),sum)
# data_aggr = aggregate(list(watt=data$watt),by=list(hour=as.POSIXct(data$hour,tz="EST",format="%Y/%m/%d %H:%M:%S")),sum)
# Before November we dont have ST-1R05 cabinet
# data_aggr2 = data_aggr[data_aggr$hour > as.POSIXlt("2014-10-31 23:59:59"),]
data_aggr2 = data_aggr[data_aggr$hour > as.POSIXlt("2014-10-31 23:59:59",tz="EST"),]
#data_aggr2=data_aggr[as.POSIXlt(data_aggr$hour) > as.POSIXlt("2014-10-31 23:59:59",tz="EST"),]
attach(data_aggr2)

dt = as.POSIXlt(hour)
data.z = zoo(x=watt,order.by=dt)
class(data.z)
head(index(data.z))
head(coredata(data.z),150)
sub_text = range(index(data.z))
plot(data.z,
     xlab="Date Range",
     ylab="Watt",
     #xaxt='n',
     main="All 4 GSS Racks PDU Power Consumption", 
     sub=paste(sub_text,collapse="  to  ")
     )
summary(watt)
sd(watt)
quantile(watt,0.95)

#Decompose time series

#plot(decompose(ts(coredata(data.z), frequency=24, as.Date("2014-10-23 12:30"))),xlab="Day")
#plot(decompose(ts(coredata(data.z), frequency=24, start=c(23,15))),xlab="Day")
#title(sub=paste(sub_text,collapse="  to  "))

dev.off()
