#Script to see how precipitation changes with different temperatures.
#Tim Kerr
#April 2016
#Aqualinc Research Ltd
#Prepared as part of project C16049 Impact of climate cycles and trends on Selwyn District water assets

#Load libraries
library(zoo)
library(reshape2)
library(lattice)
library(grid)
library(extrafont)
library(xts)

#Some usefull functions
as.year <- function(x) floor(as.numeric(as.yearmon(x)))

#Set directories
DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")
ReportDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Reports")

#Set the start and finish dates of interest
StartDate <- as.Date("1910-01-01","%Y-%m-%d")
EndDate <- as.Date("2015-11-30","%Y-%m-%d")

#StartDate <- as.Date("1935-01-01","%Y-%m-%d") #For Arthurs Pass
StartYear <- as.year(StartDate)
EndYear <- as.year(EndDate)

#********************************************************
#Plot the Lincoln Annual Temperature Data
#******************************************************
#Load in the temperature data
#FileName <- "4698 extended.csv"  #Darfield
#FileName <- "4836 extended.csv"  #Darfield
#FileName <- "4881 extended.csv"  #Lincoln
FileName <- "4843 extended.csv"  #Christchurch
#FileName <- "4513 extended.csv"  #Arthurs Pass

#MaxTData <- read.zoo(file.path(DataDirectory,"StationData","MaxT",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="NA",sep=",")
MaxTData <- read.zoo(file.path(DataDirectory,"StationData","MaxT",FileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,stringsAsFactors=FALSE,na.strings="",sep=",")

MaxTData.annual <- aggregate(MaxTData, as.year, mean)        #for temperature get annual averages
#MinTData <- read.zoo(file.path(DataDirectory,"StationData","MinT",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="NA",sep=",")
MinTData <- read.zoo(file.path(DataDirectory,"StationData","MinT",FileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,stringsAsFactors=FALSE,na.strings="",sep=",")

MinTData.annual <- aggregate(MinTData, as.year, mean)        #for temperature get annual averages
#limit to the 1910 to 2015 period
DatesOfInterest <- which((index(MaxTData.annual) >= StartYear) & (index(MaxTData.annual) <= EndYear))
MaxTData.annual <- MaxTData.annual[DatesOfInterest]
DatesOfInterest <- which((index(MinTData.annual) >= StartYear) & (index(MinTData.annual) <= EndYear))
MinTData.annual <- MinTData.annual[DatesOfInterest]

#png(file=file.path(ReportDirectory,"Climate","LincolnTemperatureTimeSeries.png"),width=15.5,height=5,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
par(mar=c(2,4,1,0),cex=1)
plot(MaxTData.annual,plot.type="single",
                     lty=c(1,2,3),xlab="",ylab="Mean annual daily T (o C)",col="red",lwd=2,
                     xlim=c(StartYear,2020),ylim=c(-2,16),xaxt="n")
lines(MinTData.annual, col = "blue")
axis(1,at=seq(from=StartYear,to=2020,by=10))
par(oldpar)
#dev.off()
  

#plot rainfall
#********************************************************
#Plot the Christchurch Annual rainfall Data
#******************************************************
#FileName <- "4843 extended.csv"  #Christchurch
#FileName <- "4513 extended.csv"  #Arthurs Pass
#FileName <- "4663 extended.csv"  #Castle Hill
#FileName <- "4698 extended.csv"  #Highbank
#FileName <- "4670 extended.csv"  #Coleridge
FileName <- "4881 extended.csv"  #Lincoln

#RainData <- read.zoo(file.path(DataDirectory,"StationData","Rainfall",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
RainData <- read.zoo(file.path(DataDirectory,"StationData","Rainfall",FileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,stringsAsFactors=FALSE,na.strings="NA",sep=",")

RainData.annual <- aggregate(RainData, as.year, sum)        #for temperature get annual averages

#png(file=file.path(ReportDirectory,"Climate","ChristchurchRainTimeSeries.png"),width=15.5,height=5,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
par(mar=c(2,4,1,0),cex=1)
CHchRainfallPlot <- plot(RainData.annual,plot.type="single",
                    lty=c(1,2,3),xlab="",ylab="Mean annual rainfall (mm)",col=rgb(49,132,155,255,maxColorValue=255),lwd=2,
                    xlim=c(StartYear,2020),xaxt="n")
axis(1,at=seq(from=StartYear,to=2020,by=10))
par(oldpar)
#dev.off()

DatesOfInterest <- which((index(RainData) >= StartDate) & (index(RainData) <= EndDate))
RainData <- RainData[DatesOfInterest,]
percent95 <- quantile(RainData,0.95,na.rm=TRUE)
larger95 <- RainData > percent95
RainData95.month <- aggregate(larger95, as.yearmon, sum)         #get monthly totals of days with more than the 95th percentile
RainData95.year <- aggregate(larger95,by=as.year,sum)


#Plot the annual precip and the annual average daily Maximum temperature in order of the max temperature
oldpar <- par()
par(mar = c(5,5,2,5))
PrecipMaxTemp <- as.data.frame(merge(MaxTData.annual,RainData.annual,all=FALSE))
OrderedData <- PrecipMaxTemp[with(PrecipMaxTemp, order(MaxTData.annual)),]
                        plot(OrderedData[,2],ylab="annual rainfall",xlab=NA,main="precip and MaxT")
par(new=T)                        
plot(OrderedData[,1],col="red",axes=F,ylab=NA,xlab=NA)
axis(side=4,col="red")
mtext(side=4,line=3,"annual average daily maximum temperature")
par(oldpar)

#Check for trends
lmodel <- lm(OrderedData[,2] ~ seq(1:nrow(OrderedData)))
print("Annual Rain by MaxT")
print(summary(lmodel))

#Plot the annual precip against and annual average daily minimum temperature in order of min temperature
oldpar <- par()
par(mar = c(5,5,2,5))
PrecipMinTemp <- as.data.frame(merge(MinTData.annual,RainData.annual,all=FALSE))
OrderedData <- PrecipMinTemp[with(PrecipMinTemp, order(MinTData.annual)),]
plot(OrderedData[,2],ylab="annual rainfall",xlab=NA,main="precip and MinT")
par(new=T)                        
plot(OrderedData[,1],col="red",axes=F,ylab=NA,xlab=NA)
axis(side=4,col="red")
mtext(side=4,line=3,"annual average daily maximum temperature")
par(oldpar)

#Check for trends
lmodel <- lm(OrderedData[,2] ~ seq(1:nrow(OrderedData)))
print("Annual Rain by MinT")
print(summary(lmodel))

#Plot the annual average daily Minimum temperature and the annual average daily Maximum temperature in order of Max temperature
oldpar <- par()
par(mar = c(5,5,2,5))
TempTemp <- as.data.frame(merge(MaxTData.annual,MinTData.annual,all=FALSE))
OrderedData <- TempTemp[with(TempTemp, order(MaxTData.annual)),]
plot(OrderedData[,2],ylab="annual average daily minimum temperature",xlab=NA,main="MinT and MaxT")
par(new=T)                        
plot(OrderedData[,1],col="red",axes=F,ylab=NA,xlab=NA)
axis(side=4,col="red")
mtext(side=4,line=3,"annual average daily maximum temperature")
par(oldpar)

#Check for trends
lmodel <- lm(OrderedData[,2] ~ seq(1:nrow(OrderedData)))
print("MinT by MaxT")
print(summary(lmodel))

#Plot the annual precip against the anual average daily Maximum temperature
oldpar <- par()
par(mar = c(5,5,2,5))
Precip95Temp <- as.data.frame(merge(MaxTData.annual,RainData95.year,all=FALSE))
OrderedData <- Precip95Temp[with(Precip95Temp, order(MaxTData.annual)),]
plot(OrderedData[,2],ylab="annual freq of 95th %iles",xlab=NA,main="95 %ile precip and MaxT",xaxt="n")
axis(side=1,at=1:nrow(OrderedData),labels=rownames(OrderedData),las=2)
par(new=T)                        
plot(OrderedData[,1],col="red",axes=F,ylab=NA,xlab=NA)
axis(side=4,col="red")
mtext(side=4,line=3,"annual average daily maximum temperature")

par(oldpar)

#Check for trends
lmodel <- lm(OrderedData[,2] ~ seq(1:nrow(OrderedData)))
print("Precip 95th by order of MaxT")
print(summary(lmodel))

#Check for relationships
lmodel <- lm(Precip95Temp[,2] ~ Precip95Temp[,1])
print("Precip 95th against MaxT")
print(summary(lmodel))

#Check for relationships
lmodel <- lm(PrecipMaxTemp[,2] ~ PrecipMaxTemp[,1])
print("Precip against MaxT")
print(summary(lmodel))
