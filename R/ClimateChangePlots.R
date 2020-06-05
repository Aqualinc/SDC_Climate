#Script to plot variables that have a trend 
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
library(FinTS)

#Some usefull functions
as.year <- function(x) floor(as.numeric(as.yearmon(x)))

#Set directories
ProjectDirectory <- "H:\\WL Projects\\WL20030_SDC Impacts of Climate Change on Infrastructure"
DataDirectory <- file.path(ProjectDirectory,"Data")
ReportDirectory <- file.path(ProjectDirectory,"Reports")

#********************************************************
#Plot the Sea Level Data
#******************************************************
#Load in Sea level data
#SeaLevel <- read.table(file.path(DataDirectory,"SeaLevel","LytteltonSeaLevelMonthly1924-2001.txt"),sep=";",na.strings="-99999")
SeaLevel <- read.table(file.path(DataDirectory,"SeaLevel","Lyttmsl_10_nomets.dat"),sep="")
SeaLevel <- zoo(SeaLevel[,2],order.by=as.yearmon(SeaLevel[,1]))
SeaLevelTrend <- lm(coredata(SeaLevel) ~ index(SeaLevel),data=SeaLevel)

png(file=file.path(ReportDirectory,"Climate","SeaLevelTimeSeries.png"),width=15.5,height=5,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
par(mar=c(2,4,1,0),cex=1)
SeaLevelPlot <- plot(SeaLevel,plot.type="single",
                          lty=c(1,2,3),xlab="",ylab="m above datum",col=rgb(49,132,155,255,maxColorValue=255),lwd=2,
                          xlim=c(1910,2020),xaxt="n")
axis(1,at=seq(from=1910,to=2020,by=10))
abline(SeaLevelTrend, col="red", lwd=2)
legend("topleft",paste("Trend =", round(as.numeric(SeaLevelTrend$coef[2])*1000,1),"\u00b1", round(coef(summary(SeaLevelTrend))[,"Std. Error"][2]*1000,1),"mm/year"),bty="n")
par(oldpar)
dev.off()

#********************************************************
#Plot the Lincoln Annual Temperature Data
#******************************************************
#Load in the temperature data
FileName <- "4881 extended.csv"
MaxTData <- read.zoo(file.path(DataDirectory,"StationData","MaxT",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
MaxTData.annual <- aggregate(MaxTData, as.year, mean)        #for temperature get annual averages

png(file=file.path(ReportDirectory,"Climate","LincolnTemperatureTimeSeries.png"),width=15.5,height=5,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
par(mar=c(2,4,1,0),cex=1)
LincolnTPlot <- plot(MaxTData.annual,plot.type="single",
                     lty=c(1,2,3),xlab="",ylab="Mean annual max T (??C)",col=rgb(49,132,155,255,maxColorValue=255),lwd=2,
                     xlim=c(1910,2020),xaxt="n")
axis(1,at=seq(from=1910,to=2020,by=10))
par(oldpar)
dev.off()
  

#plot evapotranspiration
#********************************************************
#Plot the Christchurch Annual ET Data
#******************************************************
FileName <- "4843 extended.csv"
PETData <- read.zoo(file.path(DataDirectory,"StationData","PET",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
PETData.annual <- aggregate(PETData, as.year, sum)        #for temperature get annual averages

png(file=file.path(ReportDirectory,"Climate","ChristchurchPETTimeSeries.png"),width=15.5,height=5,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
par(mar=c(2,4,1,0),cex=1)
CHchPETPlot <- plot(PETData.annual,plot.type="single",
                     lty=c(1,2,3),xlab="",ylab="Mean annual PET (mm)",col=rgb(49,132,155,255,maxColorValue=255),lwd=2,
                     xlim=c(1960,2020),xaxt="n")
axis(1,at=seq(from=1960,to=2020,by=10))
par(oldpar)
dev.off()


#plot rainfall
#********************************************************
#Plot the Christchurch Annual rainfall Data
#******************************************************
FileName <- "4843 extended.csv"
RainData <- read.zoo(file.path(DataDirectory,"StationData","Rainfall",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,stringsAsFactors=FALSE,na.strings="",sep=",")
RainData.annual <- aggregate(RainData, as.year, sum)        #for temperature get annual averages

png(file=file.path(ReportDirectory,"Climate","ChristchurchRainTimeSeries.png"),width=15.5,height=5,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
par(mar=c(2,4,1,0),cex=1)
CHchRainfallPlot <- plot(RainData.annual,plot.type="single",
                    lty=c(1,2,3),xlab="",ylab="Mean annual rainfall (mm)",col=rgb(49,132,155,255,maxColorValue=255),lwd=2,
                    xlim=c(1910,2020),ylim=c(300,1000),xaxt="n")
axis(1,at=seq(from=1910,to=2020,by=10))
par(oldpar)
dev.off()

#plot NIWA Temperature from the NIWA 7-station series
#********************************************************
#plot NIWA Temperature from the NIWA 7-station series
#******************************************************
FileName <- "NIWASevenStationNZT7_Adjusted_TMean_Web_Jan2016Composite.csv"
NIWAData.annual <- read.yearmon(file.path(DataDirectory,"StationData",FileName),format="%Y",header=TRUE,stringsAsFactors=FALSE,sep=",")
TemperatureTrend <- lm(coredata(NIWAData.annual) ~ index(NIWAData.annual),data=NIWAData.annual)

png(file=file.path(ReportDirectory,"Climate","NIWATimeSeries.png"),width=15.5,height=5,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
par(mar=c(2,4,1,0),cex=1)
NIWATemperaturePlot <- plot(NIWAData.annual,plot.type="single",
                         lty=c(1,2,3),xlab="",ylab="Temperature (\u00B0C)",col=rgb(49,132,155,255,maxColorValue=255),lwd=2,
                         xlim=c(1910,2020),xaxt="n")
axis(1,at=seq(from=1910,to=2020,by=10))
abline(TemperatureTrend, col="red", lwd=2)
legend("topleft",paste("Trend =", round(as.numeric(TemperatureTrend$coef[2]),3),"\u00b1", round(coef(summary(TemperatureTrend))[,"Std. Error"][2],3),"\u00B0C/year"),bty="n")
par(oldpar)
dev.off()
