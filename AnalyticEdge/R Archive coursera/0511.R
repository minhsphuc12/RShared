data=read.table("household_power_consumption.txt",header=TRUE,sep=';',na.strings='NA')
data$Date=as.Date(data$Date,format="%d/%m/%Y")
#subset data to selected dates
data1=subset(data, data$Date=="2007-02-02"|data$Date=="2007-02-01")
#call datasets 
library(datasets)

#plot 1
a=as.numeric(as.character(data1$Global_active_power))
k=hist(a,main="Global active power",
     xlab="Global Active Power(kilowatts)",col="red")
#export to device
dev.copy(png,file='plot1.png')
dev.off()

#plot 2
data1$spec=as.POSIXct(paste(data1$Date, data1$Time), format="%Y-%m-%d %H:%M:%S")
l=plot(data1$spec,data1$Global_active_power,type='s',ylab="Global Active Power(kilowatts")
#export to device
dev.copy(png,file='plot2.png')
dev.off()

#plot 3
#convert data from factor to numeric form
data1$Sub_metering_1=as.numeric(as.character(data1$Sub_metering_1))
data1$Sub_metering_2=as.numeric(as.character(data1$Sub_metering_2))
data1$Sub_metering_3=as.numeric(as.character(data1$Sub_metering_3))
m=plot(data1$spec,data1$Sub_metering_1,type='s',xlab='',ylab='Energy sub metering')
lines(data1$spec,data1$Sub_metering_2,col='red')
lines(data1$spec,data1$Sub_metering_3,col='blue')
#add lenged
legend('topright',legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'),lwd=0,col=c('black','red','blue'),bty='n',xjust=0,yjust=0)
#export to device
dev.copy(png,file='plot3.png')
dev.off()

#plot 4
data1$Voltage=as.numeric(as.character(data1$Voltage))
#set plotting space
par(mfrow=c(2,2))
l=plot(data1$spec,data1$Global_active_power,type='s',xlab='',ylab="Global Active Power(kilowatts")
n=plot(data1$spec,data1$Voltage,xlab='datetime',ylab='Voltage',type='s')
m=plot(data1$spec,data1$Sub_metering_1,type='s',xlab='',ylab='Energy sub metering')
lines(data1$spec,data1$Sub_metering_2,col='red')
lines(data1$spec,data1$Sub_metering_3,col='blue')
legend('topright',legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'),lwd=0,col=c('black','red','blue'),bty='n',xjust=0,yjust=0)
p=plot(data1$spec,data1$Global_reactive_power,xlab='datetime',ylab='Global_reactive_power',type='s')
#export to device
dev.copy(png,file='plot4.png')
dev.off()
