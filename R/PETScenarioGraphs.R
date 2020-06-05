#Script to present PET original, detrended, climate change for the Christchurech Aero site
#Tim Kerr
#March 2016
#Aqualinc Research Ltd
#Prepared as part of project C16049 Impact of climate cycles and trends on Selwyn District water assets

#Load libraries
library(zoo)
library(reshape2)
library(lattice)
library(grid)
library(extrafont)
library(matrixStats)


#Some usefull functions
as.year <- function(x) floor(as.numeric(as.yearmon(x)))

#Set directories
DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")
ReportDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Reports")

#Set the start and finish dates of interest
StartDate <- as.Date("1910-01-01","%Y-%m-%d")
EndDate <- as.Date("2015-11-30","%Y-%m-%d")

#Load PET data
FileName <- "4843_19100101-20151130.csv"

SQPETData <- read.zoo(file.path(DataDirectory,"StationData","PET",FileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,sep=",")
GCCPETData <- read.zoo(file.path(DataDirectory,"AdjustedStationData","GCCModel","PET",FileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,sep=",")
DetrendPETData <- read.zoo(file.path(DataDirectory,"AdjustedStationData","Detrended","PET",FileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,sep=",")

PETAllTimeSeries <- merge(SQPETData,DetrendPETData,GCCPETData)
AnnualPET <- aggregate(PETAllTimeSeries,by=as.year,sum)
AnnualPET <- window(AnnualPET, end = as.yearmon("2014-12"))

GraphFileName <- "PETScenarioExample.png"
png(file=file.path(ReportDirectory,"Climate",GraphFileName),width=15,height=15,units="cm",res=600,family="Arial",pointsize=10)
plot(AnnualPET,plot.type="single",col=c("black","red","blue"),
     xlab="",
     ylab = "PET (mm)",
     xlim=as.yearmon(c("1910-01","2016-01")))
legend("bottomright",legend=c("Status Quo","Detrended","2046"),col=c("black","red","blue"),lty=1,bty="n")
dev.off()


