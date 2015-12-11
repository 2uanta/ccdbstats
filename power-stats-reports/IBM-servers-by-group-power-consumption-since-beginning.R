###By category

library(zoo)
library(lattice)
library(ggplot2)
# load the dataset
data <- read.csv("/home/qnguye/power-stats-total/total-by-category", 
                 strip.white=TRUE,na.strings=c("", "NA"),header=FALSE)
pdf("IBM-servers-by-group-power-consumption.pdf",width=8.5,height=11)

names(data)[1]="datetime"
names(data)[2]="category"
names(data)[3]="watt"
data$datetime = as.POSIXlt(data$datetime,tz="EST",format="%Y/%m/%d %H:%M:%S")
data$year = as.Date(cut(as.Date(data$datetime), breaks="year"))
data$quarter = as.Date(cut(as.Date(data$datetime), breaks="quarter"))
data$month = as.Date(cut(as.Date(data$datetime), breaks="month"))
data$week = as.Date(cut(as.Date(data$datetime), breaks="week"))
data$day = as.Date(cut(as.Date(data$datetime), breaks="day"))
data$hour = as.POSIXlt(cut(as.POSIXlt(data$datetime,format="%Y-%m-%d %H:%M:%S"), breaks="hour"),tz="EST",format="%Y-%m-%d %H:%M:%S")
attach(data)
table(category)
#by(watt,category,mean)
tapply(watt, category, mean)

#(p1 = ggplot(data=data, aes(x=as.POSIXct(hour), y=watt, colour=category)) + geom_line() + #geom_point())

(p1 = ggplot(data=data,
    aes(x=as.POSIXct(hour), y=watt, colour=category)) +   
    geom_line() +
    labs(x="Datetime") +
    labs(title="By Category")
)

barplot(by(watt,category,mean),
    main="Power consumption by category",              
    ylab="Watt"
)


###By row

# load the dataset
data <- read.csv("/home/qnguye/power-stats-total/total-by-row", 
                 strip.white=TRUE,na.strings=c("", "NA"),header=FALSE)
names(data)[1]="datetime"
names(data)[2]="row"
names(data)[3]="watt"
data$datetime = as.POSIXlt(data$datetime,tz="EST",format="%Y/%m/%d %H:%M:%S")
data$year = as.Date(cut(as.Date(data$datetime), breaks="year"))
data$quarter = as.Date(cut(as.Date(data$datetime), breaks="quarter"))
data$month = as.Date(cut(as.Date(data$datetime), breaks="month"))
data$week = as.Date(cut(as.Date(data$datetime), breaks="week"))
data$day = as.Date(cut(as.Date(data$datetime), breaks="day"))
data$hour = as.POSIXlt(cut(as.POSIXlt(data$datetime,format="%Y-%m-%d %H:%M:%S"), breaks="hour"),tz="EST",format="%Y-%m-%d %H:%M:%S")
attach(data)
table(row)
by(watt,row,summary)
by(watt,row,sd)
by(watt,row,quantile,0.95)
by(watt,row,mean)

###

(p1 = ggplot(data=data,
    aes(x=as.POSIXct(hour), y=watt, colour=row)) +   
    geom_line() +
    labs(x="Datetime") +
    labs(title="By Row")
)

barplot(by(watt,row,mean),
    main="Power consumption by row",              
    ylab="Watt"
)

###By rack

# load the dataset
data <- read.csv("/home/qnguye/power-stats-total/total-by-rack", 
                 strip.white=TRUE,na.strings=c("", "NA"),header=FALSE)
names(data)[1]="datetime"
names(data)[2]="rack"
names(data)[3]="watt"
data$datetime = as.POSIXlt(data$datetime,tz="EST",format="%Y/%m/%d %H:%M:%S")
data$year = as.Date(cut(as.Date(data$datetime), breaks="year"))
data$quarter = as.Date(cut(as.Date(data$datetime), breaks="quarter"))
data$month = as.Date(cut(as.Date(data$datetime), breaks="month"))
data$week = as.Date(cut(as.Date(data$datetime), breaks="week"))
data$day = as.Date(cut(as.Date(data$datetime), breaks="day"))
data$hour = as.POSIXlt(cut(as.POSIXlt(data$datetime,format="%Y-%m-%d %H:%M:%S"), breaks="hour"),tz="EST",format="%Y-%m-%d %H:%M:%S")


attach(data)
table(rack)
#by(watt,rack,mean)
tapply(watt, rack, mean)

###

(p1 = ggplot(data=data,
    aes(x=as.POSIXct(hour), y=watt, colour=rack)) +   
    geom_line() +
    labs(x="Datetime") +
    labs(title="By Rack")
)

barplot(by(watt,rack,mean),
    main="Power consumption by rack",              
    ylab="Watt"
)

###LM-4R10

datalm4r10 = data[rack == "lm-4r10",]
summary(datalm4r10$watt)
sd(datalm4r10$watt)
quantile(datalm4r10$watt,0.95)
#xyplot(datalm4r10$watt ~ as.POSIXct(datalm4r10$hour), type="l")
(p1 = ggplot(data=datalm4r10,
    aes(x=as.POSIXct(datalm4r10$hour), y=datalm4r10$watt)) +   
    geom_line() +
    labs(x="Datetime") +
    labs(title="lm-4r10")
)

###AW-4R12

dataaw4r12 = data[rack == "aw-4r12",]
summary(dataaw4r12$watt)
sd(dataaw4r12$watt)
quantile(dataaw4r12$watt,0.95)
#xyplot(dataaw4r12$watt ~ as.POSIXct(dataaw4r12$hour), type="l")
(p1 = ggplot(data=dataaw4r12,
    aes(x=as.POSIXct(dataaw4r12$hour), y=dataaw4r12$watt)) +   
    geom_line() +
    labs(x="Datetime") +
    labs(title="aw-4r12")
)

dev.off()
