#Script to present Wastewater variables
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
FileName <- "SQOut.csv"

#Get the CLimate indices data
load(file=file.path(DataDirectory,"ClimateIndices","ClimateIndices.RData"))

#Set the start and finish dates of interest
StartDate <- as.Date("1910-01-01","%Y-%m-%d")
EndDate <- as.Date("2015-11-30","%Y-%m-%d")

#Set the different IPO phase years
IPOPosYears <- c(1922:1944,1979:1998)
IPONegYears <- c(1946:1977,2000:2010)

#Groundwater data
ScenarioType <- "SQ"
GWSites <- c("M360424","M360599","M360217","M351080")
GWLocations <- c("Doyleston","Springston","Weedons","Templeton")
GWColNames <- paste0(ScenarioType,GWSites)
parameterData <- read.table(file.path(DataDirectory,"GroundwaterModel",FileName),header=TRUE,sep=",")
GWAllTimeSeries <- zoo(parameterData[,-1], order.by = as.yearmon(parameterData[,1],format="%b-%Y"))
GWTimeSeries <- GWAllTimeSeries[,GWColNames]   #Just get the sites we are interested in

#Rainfall data
RainfallSites <- c(25821,4663,4670,4674,4834,4935,4881,4843)
RainfallLocations <- c("Arthurs Pass","Castle Hill","Lake Coleridge","Mt Torlesse","Homebush","Leeston","Lincoln","Christchurch Aero")
Rain95ths <- rep(NA,length(RainfallSites))
#loop for all the stations
for (DataAgentNumber in RainfallSites){
#for (DataAgentNumber in c(25821,4663)){
  loopCount <- which(RainfallSites==DataAgentNumber)
  FileName <- paste0(DataAgentNumber," extended.csv")
  parameterData <- read.zoo(file.path(DataDirectory,"StationData","Rainfall",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  #limit to the 1910 to 2015 period
  DatesOfInterest <- which((index(parameterData) >= StartDate) & (index(parameterData) <= EndDate))
  parameterData <- parameterData[DatesOfInterest,]
  percent95 <- quantile(parameterData,0.95)
  Rain95ths[loopCount]<- round(percent95,1)
  print(paste("The 95th percentile for",DataAgentNumber,"is",round(percent95,1),"mm"))
  larger95 <- parameterData > percent95
  parameterData.month <- aggregate(larger95, as.yearmon, sum)         #get monthly totals of days with more than the 95th percentile
  parameterData.year <- aggregate(larger95,by=as.year,sum)
  if(which(RainfallSites==DataAgentNumber)==1){RainData.Year <- parameterData.year}
  else {RainData.Year <- merge(RainData.Year,parameterData.year)}
  #Check for a trend
  lmRain <- lm(coredata(parameterData.year[,c(site),drop=FALSE]) ~ index(parameterData.year), data=parameterData.year)
  summary(lmRain)$r.squared
  if(summary(lmRain)$coefficients[2,4]<0.05){ print(paste("Trend of",summary(lmRain)$coefficients[2,2],"m/year."))
  } else {print("No trend.")}
}
colnames(RainData.Year)<- paste0("R",RainfallSites)



#Calculate monthly normalised monthly anomalies. Note that normalisation is done with respect to the month, not all data. That is, if there is greater variability in january compared with june, they are scaled with repect the January's and June's respectively
Months <- as.character(format(index(GWTimeSeries),"%b"))          #get the months
Years <- as.character(format(index(GWTimeSeries),"%Y"))             #get the years

GWTimeSeriesLong <- cbind(as.data.frame(GWTimeSeries),Months,Years)
for (site in colnames(GWTimeSeries)){
  MonthWide <- dcast(GWTimeSeriesLong,Years ~ Months, value.var = site)
  MonthScaled <- cbind(MonthWide[,1,drop=FALSE],scale(MonthWide[,-1]))
  MonthLong <- melt(MonthScaled,id.vars=c("Years"),variable.name = "Month",value.name=paste("Anom",site,sep="."))
  YearMonth <- as.yearmon(paste(MonthLong$Month,MonthLong$Years))
  GWAnomaly <- zoo(MonthLong[,3], order.by = YearMonth)
  GWTimeSeries <- merge(GWTimeSeries,GWAnomaly)
  names(GWTimeSeries)[ncol(GWTimeSeries)]<- paste0("Anom.",site)
}

#Calculate annual statistics
AnnualENSO <- aggregate(Indices[,12,drop=FALSE],by=as.numeric(format(index(Indices),"%Y")),mean)  
AnnualIPO <- aggregate(Indices[,19,drop=FALSE],by=as.numeric(format(index(Indices),"%Y")),mean)
AnnualSAM <- aggregate(Indices[,20,drop=FALSE],by=as.numeric(format(index(Indices),"%Y")),mean)

#Calculate annual maximum GW levels
yearData <- aggregate(GWTimeSeries,by=as.numeric(format(index(GWTimeSeries),"%Y")),max)

#Add the index data to the parameter data
yearData <- merge(AnnualENSO,AnnualIPO,AnnualSAM,yearData,all=c(FALSE,FALSE,FALSE,TRUE)) #attach the climate indices
names(yearData)[1:3] <- c("ENSO","IPO","SAM")                 #name the climate index columns

RainData.Year <- merge(AnnualENSO,AnnualIPO,AnnualSAM,RainData.Year,all=c(FALSE,FALSE,FALSE,TRUE)) #attach the climate indices
names(RainData.Year)[1:3] <- c("ENSO","IPO","SAM")                 #name the climate index columns

#Check Groundwater for trends
for (site in colnames(GWTimeSeries[,1:4])){
  #any trend?
  years <- as.numeric(index(yearData))
  lmGW <- lm(coredata(yearData[,c(site),drop=FALSE]) ~ years, data=yearData)
  summary(lmGW)$r.squared
  if(summary(lmGW)$coefficients[2,4]<0.05){ print(paste("Trend of",summary(lmGW)$coefficients[2,2],"m/year."))
  } else {print("No trend.")}
}

#Check Groundwater for differences between IPO phases
GWIPOPos <- yearData[which(index(yearData) %in% IPOPosYears)]
GWIPONeg <- yearData[which(index(yearData) %in% IPONegYears)]
for (index in c(4:7)){
  print(t.test(GWIPOPos[,index],GWIPONeg[,index]))
}

#**************************************************
#Plot scatter plots of GW data vs climate indices
#***************************************************
#Start by setting the output filename and type
png(file=file.path(ReportDirectory,"Climate","WastewaterGW-IndexScatter.png"),width=11,height=7,units="cm",res=600,family="Arial",pointsize=10)

oldpar <- par()
ScatterPlot <- layout(matrix(seq(1:8),2,4),widths=lcm(c(4,2,2,2)),heights=lcm(c(2,4)))
par(mar=c(0,4,0,0),cex=1)
plot(x=yearData[,4],y=yearData[,1],xaxt="n",ylab="ENSO",las=1,yaxp=c(-1,1,2))
#plot(x=yearData[,4],y=yearData[,2],xaxt="n",ylab="IPO",las=1,yaxp=c(-0.5,0.5,2))
par(mar=c(4,4,0,0))
plot(x=yearData[,4],y=yearData[,3],ylab="SAM",xlab=GWSites[1],las=1,yaxp=c(-1,1,2))
par(mar=c(0,0,0,0))
plot(x=yearData[,5],y=yearData[,1],xaxt="n",yaxt="n",ann=FALSE)
#plot(x=yearData[,5],y=yearData[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=yearData[,5],y=yearData[,3],yaxt="n",xlab=GWSites[2])
par(mar=c(0,0,0,0))
plot(x=yearData[,6],y=yearData[,1],xaxt="n",yaxt="n",ann=FALSE)
#plot(x=yearData[,6],y=yearData[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=yearData[,6],y=yearData[,3],yaxt="n",xlab=GWSites[3])
par(mar=c(0,0,0,0))
plot(x=yearData[,7],y=yearData[,1],xaxt="n",yaxt="n",ann=FALSE)
#plot(x=yearData[,7],y=yearData[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=yearData[,7],y=yearData[,3],yaxt="n",xlab=GWSites[4])
par(oldpar)

print(ScatterPlot)
dev.off()
#********************************************
#Plot scatter plots of Rainfall vs climate indices
#*********************************************
#Start by setting the output filename and type
png(file=file.path(ReportDirectory,"Climate","WastewaterRain-IndexScatter.png"),width=15.5,height=7.5,units="cm",res=600,family="Arial",pointsize=10)




oldpar <- par()
ScatterPlot <- layout(matrix(seq(1:24),3,8),widths=lcm(c(3.3,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5)),heights=lcm(c(1.5,1.5,3.3)))
par(mar=c(0,4,0,0),cex=1)
plot(x=RainData.Year[,4],y=RainData.Year[,1],xaxt="n",ylab="ENSO",las=1,yaxp=c(-1,1,2))
plot(x=RainData.Year[,4],y=RainData.Year[,2],xaxt="n",ylab="IPO",las=1,yaxp=c(-0.5,0.5,2))
par(mar=c(4,4,0,0))
plot(x=RainData.Year[,4],y=RainData.Year[,3],ylab="SAM",xlab=RainfallSites[1],las=1,yaxp=c(-1,1,2))
par(mar=c(0,0,0,0))
plot(x=RainData.Year[,5],y=RainData.Year[,1],xaxt="n",yaxt="n",ann=FALSE)
plot(x=RainData.Year[,5],y=RainData.Year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RainData.Year[,5],y=RainData.Year[,3],yaxt="n",xlab=RainfallSites[2])
par(mar=c(0,0,0,0))
plot(x=RainData.Year[,6],y=RainData.Year[,1],xaxt="n",yaxt="n",ann=FALSE)
plot(x=RainData.Year[,6],y=RainData.Year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RainData.Year[,6],y=RainData.Year[,3],yaxt="n",xlab=RainfallSites[3])
par(mar=c(0,0,0,0))
plot(x=RainData.Year[,7],y=RainData.Year[,1],xaxt="n",yaxt="n",ann=FALSE)
plot(x=RainData.Year[,7],y=RainData.Year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RainData.Year[,7],y=RainData.Year[,3],yaxt="n",xlab=RainfallSites[4])
par(mar=c(0,0,0,0))
plot(x=RainData.Year[,8],y=RainData.Year[,1],xaxt="n",yaxt="n",ann=FALSE)
plot(x=RainData.Year[,8],y=RainData.Year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RainData.Year[,8],y=RainData.Year[,3],yaxt="n",xlab=RainfallSites[5])
par(mar=c(0,0,0,0))
plot(x=RainData.Year[,9],y=RainData.Year[,1],xaxt="n",yaxt="n",ann=FALSE)
plot(x=RainData.Year[,9],y=RainData.Year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RainData.Year[,9],y=RainData.Year[,3],yaxt="n",xlab=RainfallSites[6])
par(mar=c(0,0,0,0))
plot(x=RainData.Year[,10],y=RainData.Year[,1],xaxt="n",yaxt="n",ann=FALSE)
plot(x=RainData.Year[,10],y=RainData.Year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RainData.Year[,10],y=RainData.Year[,3],yaxt="n",xlab=RainfallSites[7])
par(mar=c(0,0,0,0))
plot(x=RainData.Year[,11],y=RainData.Year[,1],xaxt="n",yaxt="n",ann=FALSE)
plot(x=RainData.Year[,11],y=RainData.Year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RainData.Year[,11],y=RainData.Year[,3],yaxt="n",xlab=RainfallSites[8])
par(oldpar)

print(ScatterPlot)
dev.off()


#********************************************
#Generate a graph of the groundwater timeseries
#*********************************************
#Start by setting the output filename and type
png(file=file.path(ReportDirectory,"Climate","WastewaterGroundwaterTimeSeries.png"),width=15,height=12,units="cm",res=600,family="Arial",pointsize=10)
#find the maximum range of the GW then build a list of y axis limits for each site that matches the range
#This is done so that the plots are all on the same scale on the y-axis.
colMax <- function(data) sapply(data, max, na.rm = TRUE)              #define a function to find the coulmn maximum of a dataframe
colMin <- function(data) sapply(data, min, na.rm = TRUE)              #define a function to find the coulmn maximum of a dataframe
MaxRange <- max(colMax(yearData[,4:7])-colMin(yearData[,4:7]))*1.2    #Calculate the largest range of each of the time series and increase by 20 %
LowLimits <- colMeans(yearData[,4:7],na.rm=TRUE) - MaxRange / 2       #Calculate 
HighLimits <- colMeans(yearData[,4:7],na.rm=TRUE) + MaxRange / 2
limitsMatrix <- as.matrix(data.frame(LowLimits,HighLimits))
limitsList <-lapply(seq_len(nrow(limitsMatrix)), function(i) limitsMatrix[i,])

#Prepare names of each line graph
panelLabels<-paste0(GWLocations," (",GWSites,")")
GWPlot <-xyplot(yearData[,4:7],fontfamily="Arial",main="",xlab="",ylab="Groundwater level (metres above mean sea level)",
       col.line=rgb(49,132,155,255,maxColorValue=255),lwd=2,
#       ylim=limitsList,
#       scales=list(y=list(rot=0)),
        strip=FALSE,
       panel = function(...) {
  grid.text(panelLabels[panel.number()], .17, .90, gp=gpar(cex=1)) 
  panel.xyplot(...)
})

print(GWPlot)
dev.off()

#*******************************************
#Generate a graph of the rainfall timeseries
#******************************************
#Start by setting the output filename and type
png(file=file.path(ReportDirectory,"Climate","WastewaterRainfallTimeSeries.png"),width=15,height=15,units="cm",res=600,family="Arial",pointsize=10)

#Prepare names of each line graph
panelLabels<-paste0(RainfallLocations," (",RainfallSites,")")
#create the plot
RainPlot <-xyplot(RainData.Year,fontfamily="Arial",main="",xlab="",ylab="Days per year with more than the 95th percentile daily rain",
                  layout=c(1,NA),
                col.line=rgb(49,132,155,255,maxColorValue=255),lwd=2,
                ylim=c(5,35),
                scales=list(y=list(rot=0,tick.number=3)),
                strip=FALSE,
                panel = function(...) {
                  grid.text(panelLabels[panel.number()], .01, .85,just="left", gp=gpar(cex=1))
                  grid.text(paste("95percentile:",Rain95ths[panel.number()],"mm"), .99, .85,just="right", gp=gpar(cex=1))
                  panel.xyplot(...)
                })

print(RainPlot) #send it to the PNG output thingumy
dev.off()       #turn off the PNG output thingumy


