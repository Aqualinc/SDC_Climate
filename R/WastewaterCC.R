#Script to present Wastewater climate change variables
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


#Some usefull functions
as.year <- function(x) floor(as.numeric(as.yearmon(x)))

#Set directories
DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")
ReportDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Reports")

#Load GW and Rainfall data
FileName <- "DetrendedOut.csv"

#Get the CLimate indices data
load(file=file.path(DataDirectory,"ClimateIndices","ClimateIndices.RData"))

#Set the start and finish dates of interest
StartDate <- as.Date("1910-01-01","%Y-%m-%d")
EndDate <- as.Date("2015-11-30","%Y-%m-%d")

#Groundwater data
#for (ScenarioType in c("Detrended","Extrapolate","GCC")){
  for (ScenarioType in c("Detrended","GCC")){
#ScenarioType <- "SQ"
  #Load GW and Rainfall data
  FileName <- paste0(ScenarioType,"Out.csv")
GWSites <- c("M360424","M360599","M360217","M351080")
GWLocations <- c("Doyleston","Springston","Weedons","Templeton")
GWColNames <- paste0(ScenarioType,GWSites)
parameterData <- read.table(file.path(DataDirectory,"GroundwaterModel",FileName),header=TRUE,sep=",",colClasses=c("character",rep("numeric",9)))
GWAllTimeSeries <- zoo(parameterData[,-1], order.by = as.yearmon(parameterData[,1],format="%b-%Y"))
GWTimeSeries <- GWAllTimeSeries[,GWColNames]   #Just get the sites we are interested in
#browser()
if(ScenarioType == "Detrended"){GWAllScenarios <- GWTimeSeries}
else {GWAllScenarios <- merge(GWAllScenarios,GWTimeSeries)}
}

#Calculate monthly normalised monthly anomalies. Note that normalisation is done with respect to the month, not all data. That is, if there is greater variability in january compared with june, they are scaled with repect the January's and June's respectively
#Months <- as.character(format(index(GWTimeSeries),"%b"))          #get the months
#Years <- as.character(format(index(GWTimeSeries),"%Y"))             #get the years

#GWTimeSeriesLong <- cbind(as.data.frame(GWTimeSeries),Months,Years)
#for (site in colnames(GWTimeSeries)){
#  MonthWide <- dcast(GWTimeSeriesLong,Years ~ Months, value.var = site)
#  MonthScaled <- cbind(MonthWide[,1,drop=FALSE],scale(MonthWide[,-1]))
#  MonthLong <- melt(MonthScaled,id.vars=c("Years"),variable.name = "Month",value.name=paste("Anom",site,sep="."))
#  YearMonth <- as.yearmon(paste(MonthLong$Month,MonthLong$Years))
#  GWAnomaly <- zoo(MonthLong[,3], order.by = YearMonth)
#  GWTimeSeries <- merge(GWTimeSeries,GWAnomaly)
#  names(GWTimeSeries)[ncol(GWTimeSeries)]<- paste0("Anom.",site)
#}

#Calculate annual statistics
AnnualENSO <- aggregate(Indices[,12,drop=FALSE],by=as.numeric(format(index(Indices),"%Y")),mean)  
AnnualIPO <- aggregate(Indices[,19,drop=FALSE],by=as.numeric(format(index(Indices),"%Y")),mean)
AnnualSAM <- aggregate(Indices[,20,drop=FALSE],by=as.numeric(format(index(Indices),"%Y")),mean)

#Calculate annual maximum GW levels
yearData <- aggregate(GWAllScenarios,by=as.numeric(format(index(GWAllScenarios),"%Y")),max)

#Add the index data to the parameter data
yearData <- merge(AnnualENSO,AnnualIPO,AnnualSAM,yearData,all=c(FALSE,FALSE,FALSE,TRUE)) #attach the climate indices
names(yearData)[1:3] <- c("ENSO","IPO","SAM")                 #name the climate index columns


#GWPlotDetrended<-hist(yearData[,4],breaks=20,plot=F)
#GWPlotDetrended<-hist(yearData[,4],breaks=20,plot=F)
#GWPlotDetrended<-hist(yearData[,4],breaks=20,plot=F)



#plot(GWPlot, freq=TRUE, ylab="Relative Frequency",main="",las=1,xlab="Ground water level (m)",
#     xlim=c(floor(min(yearData[,4],na.rm=TRUE)),ceiling(max(yearData[,4],na.rm=TRUE))))

#GWDensity <- density(yearData[,4],na.rm=TRUE)
#lines(GWDensity,col="red")
#print(GWPlot)
#dev.off()

#**********************************************
#Plot Histograms of the wells for each CC scenario
#***************************************************
#Start by setting the output filename and type
png(file=file.path(ReportDirectory,"Climate","WastewaterGW-Histograms.png"),width=11,height=6,units="cm",res=600,family="Arial",pointsize=10)

oldpar <- par()
ScatterPlot <- layout(matrix(seq(1:8),2,4),widths=lcm(c(4,2,2,2)),heights=lcm(c(1.9,3.6)))
#browser()
par(mar=c(0,4,0,0),cex=1)
hist(yearData[,4],breaks=seq(min(yearData[,4]),max(yearData[,4]),l=21),yaxt="n",xaxt="n",ylab="Detrended",las=1,main="",ylim=c(0,17),xlim=c(20,22.2))
axis(2,at=c(0,5.25,10.5,15.75),labels=c(0,0.05,0.1,0.15))
#hist(yearData[,8],breaks=20,yaxt="n",xaxt="n",ylab="Extrapolated",las=1,main="",ylim=c(0,17),xlim=c(20,22.2))
#axis(2,at=c(0,5.25,10.5,15.75),labels=c(0,0.05,0.1,0.15))
par(mar=c(4,4,0,0))
hist(yearData[,8],breaks=seq(min(yearData[,8]),max(yearData[,8]),l=21),yaxt="n",ylab="GCC",xlab=GWSites[1],las=1,main="",ylim=c(0,17),xlim=c(20,22.2))
axis(2,at=c(0,5.25,10.5,15.75),labels=c(0,0.05,0.1,0.15))
par(mar=c(0,0,0,0))
hist(yearData[,5],breaks=seq(min(yearData[,5]),max(yearData[,5]),l=21),xaxt="n",yaxt="n",ann=FALSE,main="",ylim=c(0,17),xlim=c(12.6,14.3))

#hist(yearData[,9],breaks=20,xaxt="n",yaxt="n",ann=FALSE,main="",ylim=c(0,17),xlim=c(12.6,14.3))
par(mar=c(4,0,0,0))
hist(yearData[,9],breaks=seq(min(yearData[,9]),max(yearData[,9]),l=21),yaxt="n",xlab=GWSites[2],main="",ylim=c(0,17),xlim=c(12.6,14.3))
par(mar=c(0,0,0,0))
hist(yearData[,6],breaks=seq(min(yearData[,6]),max(yearData[,6]),l=21),xaxt="n",yaxt="n",ann=FALSE,main="",ylim=c(0,17),xlim=c(31,45))
#hist(yearData[,10],breaks=seq(min(yearData[,10]),max(yearData[,10]),l=21),xaxt="n",yaxt="n",ann=FALSE,main="",ylim=c(0,17),xlim=c(31,45))
par(mar=c(4,0,0,0))
hist(yearData[,10],breaks=seq(min(yearData[,10]),max(yearData[,10]),l=21),yaxt="n",xlab=GWSites[3],main="",ylim=c(0,17),xlim=c(31,45))
par(mar=c(0,0,0,0))
hist(yearData[,7],breaks=seq(min(yearData[,7]),max(yearData[,7]),l=21),xaxt="n",yaxt="n",ann=FALSE,main="",ylim=c(0,17),xlim=c(43,54))
#hist(yearData[,11],breaks=20,xaxt="n",yaxt="n",ann=FALSE,main="",ylim=c(0,17),xlim=c(43,54))
par(mar=c(4,0,0,0))
hist(yearData[,11],breaks=seq(min(yearData[,11]),max(yearData[,11]),l=21),yaxt="n",xlab=GWSites[4],main="",ylim=c(0,17),xlim=c(43,54))
par(oldpar)
mtext("Groundwater level (metres above mean sea level)",line=4.2,side=1,at=0.5)
mtext("frequency of occurrence",line=3.2,side=2,at=0.5)
print(ScatterPlot)
dev.off()
