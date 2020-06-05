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
library(xts)


#Some usefull functions
as.year <- function(x) floor(as.numeric(as.yearmon(x)))

#a peak finding function
peaks<-function(series) { 
  z <- embed(as.vector(series), 3) 
  v<- max.col(z,"first") == 2 
  result <- c(FALSE, v,FALSE) 
} 

#A function to read in NIWA sub daily data downloaded from https://hydrowebportal.niwa.co.nz/ and generate daily data
SubDailyToDailyNIWAData <- function(SubDailyInFileName,DailyOutFileName){
  Data <- read.zoo(SubDailyInFileName,
                   FUN=function(Y) {format(as.Date(Y),"%Y%m%d")},
                   sep=",",
                   header=TRUE,
                   stringsAsFactors = FALSE,
                   aggregate = function(x) {round(mean(x),3)},
                   colClasses=c("character","numeric",rep("NULL",4)))
  write.zoo(Data,DailyOutFileName,sep=",",col.names=c("Date","Flow (m3/s)"),quote=FALSE)
}

#Set directories
DataDirectory <- "D:\\Projects\\Aqualinc\\projects\\SDC 2020\\Data"
ReportDirectory <- "D:\\Projects\\Aqualinc\\projects\\SDC 2020\\Reports"

#DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\WL Projects\\WL20030_SDC Impacts of Climate Change on Infrastructure\\Data")
#ReportDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\WL Projects\\WL20030_SDC Impacts of Climate Change on Infrastructure\\Reports")

#Load GW and Rainfall data
FileName <- "SQOut.csv"

#Get the Climate indices data
load(file=file.path(DataDirectory,"ClimateIndices","ClimateIndices.RData"))

#Set the start and finish dates of interest
StartDate <- as.Date("1910-01-01","%Y-%m-%d")
EndDate <- as.Date("2019-07-01","%Y-%m-%d")

#Set the different IPO phase years
IPOPosYears <- c(1922:1944,1979:1998)
IPONegYears <- c(1946:1977,2000:2010)


#Get the Riverflow data
Waimak <- read.zoo(file.path(DataDirectory,"RiverFlow","Waimakariri Otarama daily flow out.csv"),sep=",",format("%Y%m%d"),colClasses=c("character","numeric"),na.strings="GAP")
Selwyn <- read.zoo(file.path(DataDirectory,"RiverFlow","68001 Selwyn Whitecliff.csv"),sep=",",format("%Y%m%d"),header=TRUE,colClasses=c("character","numeric"),na.strings="GAP")
Doyleston <- read.zoo(file.path(DataDirectory,"RiverFlow","68320 Doyleston.csv"),sep=",",format("%Y%m%d"),colClasses=c("character","numeric"),na.strings="GAP")
Rakaia <- read.zoo(file.path(DataDirectory,"RiverFlow","68526 Rakaia at Fighting Hill.csv"),sep=",",format("%Y%m%d"),header=TRUE,colClasses=c("character","numeric"),na.strings="GAP")
#Stick it all together
RiverData <- merge(Waimak,Rakaia,Selwyn,Doyleston)

#Get the 95th percentils
ninety5ths        <-  apply(RiverData,2,function(x) quantile(x,0.95,na.rm=TRUE))
#Create an annual series of the number of times the 95th percentile is exceeded each year
RiverData.year <- sapply(1:ncol(RiverData), function(x) aggregate(RiverData[,x] > ninety5ths[x],by=as.year,sum))
RiverData.year <- zoo(RiverData.year, order.by=unique(as.year(index(RiverData))))
colnames(RiverData.year) <- colnames(RiverData)

#*******************************************
#Generate a graph of the river timeseries
#******************************************
#Start by setting the output filename and type
png(file=file.path(ReportDirectory,"Climate","WaterRiverTimeSeries.png"),width=15,height=15,units="cm",res=600,family="Arial",pointsize=10)
my.panel <- function(x, ...) {
  lines(x, ...)
  panel.number <- parent.frame()$panel.number
  abline(h=ninety5ths[panel.number], col = "red", lty="solid", lwd=1.5 )
}
plot(RiverData,panel=my.panel,
     main="",xlab="",
     col=rgb(49,132,155,255,maxColorValue=255),lwd=0.5,)
dev.off()


  #Check for a trend
for (site in c(1:4)){
  lmRiver <- lm(coredata(RiverData.year[,c(site),drop=FALSE]) ~ index(RiverData.year), data=RiverData.year)
  #print(summary(lmRiver))
  if(summary(lmRiver)$coefficients[2,4]<0.05){ print(paste("Trend of",summary(lmRiver)$coefficients[2,2],"days per year."))
  } else {print("No trend.")}
}

#Check River flows for differences between IPO phases
RiverIPOPos <- RiverData.year[which(index(RiverData.year) %in% IPOPosYears)]
RiverIPONeg <- RiverData.year[which(index(RiverData.year) %in% IPONegYears)]
for (index in c(1:4)){
  print(t.test(RiverIPOPos[,index],RiverIPONeg[,index]))
}

#*******************************************
#Generate a plot of the number of days of high river flow
#******************************************
png(file=file.path(ReportDirectory,"Climate","WaterRiverHighFlowDaysTimeSeries.png"),width=15,height=15,units="cm",res=600,family="Arial",pointsize=10)
my.panel <- function(x, ...) {
  lines(x, ...)
  panel.number <- parent.frame()$panel.number
  #abline(h=ninety5ths[panel.number], col = "red", lty="solid", lwd=1.5 )
}
plot(RiverData.year,panel=my.panel,
     main="",xlab="",
     col=rgb(49,132,155,255,maxColorValue=255),lwd=0.5,)
dev.off()

#Groundwater data
ScenarioType <- "SQ"
GWSites <- c("L350180","L361226","L360092","L360124")
GWLocations <- c("Racecourse Hill","Hororata","Charing Cross","Bankside")
GWColNames <- paste0(ScenarioType,GWSites)
parameterData <- read.table(file.path(DataDirectory,"GroundwaterModel",FileName),header=TRUE,sep=",")
GWAllTimeSeries <- zoo(parameterData[,-1], order.by = as.yearmon(parameterData[,1],format="%b-%Y"))
GWTimeSeries <- GWAllTimeSeries[,GWColNames]   #Just get the sites we are interested in

#Rainfall data
RainfallSites <- c(25821,4663,4670,4674,4698,4836,4722,4935,4881,4843)
RainfallLocations <- c("Arthurs Pass","Castle Hill","Lake Coleridge","Mt Torlesse","Hororata","Darfield","Te Pirita Mead","Leeston","Lincoln","Christchurch Aero")
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

#Check Rain for differences between IPO phases
RainIPOPos <- RainData.Year[which(index(RainData.Year) %in% IPOPosYears)]
RainIPONeg <- RainData.Year[which(index(RainData.Year) %in% IPONegYears)]
for (index in c(4:ncol(RainData.Year))){
  print(t.test(RainIPOPos[,index],RainIPONeg[,index]))
}

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

#Calculate annual minimum GW levels
yearData <- aggregate(GWTimeSeries,by=as.numeric(format(index(GWTimeSeries),"%Y")),min)

#Add the index data to the parameter data
yearData <- merge(AnnualENSO,AnnualIPO,AnnualSAM,yearData,all=c(FALSE,FALSE,FALSE,TRUE)) #attach the climate indices
names(yearData)[1:3] <- c("ENSO","IPO","SAM")                 #name the climate index columns

RainData.Year <- merge(AnnualENSO,AnnualIPO,AnnualSAM,RainData.Year,all=c(FALSE,FALSE,FALSE,TRUE)) #attach the climate indices
names(RainData.Year)[1:3] <- c("ENSO","IPO","SAM")                 #name the climate index columns

RiverData.year <-merge(AnnualENSO,AnnualIPO,AnnualSAM,RiverData.year,all=c(FALSE,FALSE,FALSE,TRUE)) #attach the climate indices
names(RiverData.year)[1:3] <- c("ENSO","IPO","SAM")                 #name the climate index columns

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
png(file=file.path(ReportDirectory,"Climate","WaterGW-IndexScatter.png"),width=11,height=7,units="cm",res=600,family="Arial",pointsize=10)

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
#Plot scatter plots of Rainfall vs ENSO and SAM
#*********************************************
#Start by setting the output filename and type
png(file=file.path(ReportDirectory,"Climate","WaterRain-IndexScatter.png"),width=13,height=7,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
ScatterPlot <- layout(matrix(seq(1:10),2,5),widths=lcm(c(4,2,2,2,2)),heights=lcm(c(2,4)))
par(mar=c(0,4,0,0),cex=1)
plot(x=RainData.Year[,4],y=RainData.Year[,1],xaxt="n",ylab="ENSO",las=1,yaxp=c(-1,1,2))
#plot(x=RainData.Year[,4],y=RainData.Year[,2],xaxt="n",ylab="IPO",las=1,yaxp=c(-0.5,0.5,2))
par(mar=c(4,4,0,0))
plot(x=RainData.Year[,4],y=RainData.Year[,3],ylab="SAM",xlab=RainfallSites[1],las=1,yaxp=c(-1,1,2))
par(mar=c(0,0,0,0))
plot(x=RainData.Year[,6],y=RainData.Year[,1],xaxt="n",yaxt="n",ann=FALSE)
#plot(x=RainData.Year[,6],y=RainData.Year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RainData.Year[,6],y=RainData.Year[,3],yaxt="n",xlab=RainfallSites[2])
par(mar=c(0,0,0,0))
plot(x=RainData.Year[,8],y=RainData.Year[,1],xaxt="n",yaxt="n",ann=FALSE)
#plot(x=RainData.Year[,8],y=RainData.Year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RainData.Year[,8],y=RainData.Year[,3],yaxt="n",xlab=RainfallSites[3])
par(mar=c(0,0,0,0))
plot(x=RainData.Year[,10],y=RainData.Year[,1],xaxt="n",yaxt="n",ann=FALSE)
#plot(x=RainData.Year[,10],y=RainData.Year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RainData.Year[,10],y=RainData.Year[,3],yaxt="n",xlab=RainfallSites[4])
par(mar=c(0,0,0,0))
plot(x=RainData.Year[,12],y=RainData.Year[,1],xaxt="n",yaxt="n",ann=FALSE)
#plot(x=RainData.Year[,12],y=RainData.Year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RainData.Year[,12],y=RainData.Year[,3],yaxt="n",xlab=RainfallSites[8])
par(oldpar)

print(ScatterPlot)
dev.off()

#********************************************
#Plot scatter plots of Streamflow vs ENSO and SAM
#*********************************************
#Start by setting the output filename and type
png(file=file.path(ReportDirectory,"Climate","WaterRiver-IndexScatter.png"),width=13,height=7,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
ScatterPlot <- layout(matrix(seq(1:10),2,5),widths=lcm(c(4,2,2,2,2)),heights=lcm(c(2,4)))
par(mar=c(0,4,0,0),cex=1)
plot(x=RiverData.year[,4],y=RiverData.year[,1],xaxt="n",ylab="ENSO",las=1,yaxp=c(-1,1,2))
#plot(x=RiverData.Year[,4],y=RiverData.Year[,2],xaxt="n",ylab="IPO",las=1,yaxp=c(-0.5,0.5,2))
par(mar=c(4,4,0,0))
plot(x=RiverData.year[,4],y=RiverData.year[,3],ylab="SAM",xlab=colnames(RiverData.year)[4],las=1,yaxp=c(-1,1,2))
par(mar=c(0,0,0,0))
plot(x=RiverData.year[,5],y=RiverData.year[,1],xaxt="n",yaxt="n",ann=FALSE)
#plot(x=RiverData.year[,5],y=RiverData.year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RiverData.year[,5],y=RiverData.year[,3],yaxt="n",xlab=colnames(RiverData.year)[5])
par(mar=c(0,0,0,0))
plot(x=RiverData.year[,6],y=RiverData.year[,1],xaxt="n",yaxt="n",ann=FALSE)
#plot(x=RiverData.year[,6],y=RiverData.year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RiverData.year[,6],y=RiverData.year[,3],yaxt="n",xlab=colnames(RiverData.year)[6])
par(mar=c(0,0,0,0))
plot(x=RiverData.year[,7],y=RiverData.year[,1],xaxt="n",yaxt="n",ann=FALSE)
#plot(x=RiverData.year[,7],y=RiverData.year[,2],xaxt="n",yaxt="n",ann=FALSE)
par(mar=c(4,0,0,0))
plot(x=RiverData.year[,7],y=RiverData.year[,3],yaxt="n",xlab=colnames(RiverData.year)[7])
par(oldpar)

print(ScatterPlot)
dev.off()

#********************************************
#Generate a graph of the groundwater timeseries
#*********************************************
#Start by setting the output filename and type
png(file=file.path(ReportDirectory,"Climate","WaterGroundwaterTimeSeries.png"),width=15,height=12,units="cm",res=600,family="Arial",pointsize=10)
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
  grid.text(panelLabels[panel.number()], .01, .85, gp=gpar(cex=1),just="left") 
  panel.xyplot(...)
})

print(GWPlot)
dev.off()

#*******************************************
#Generate a graph of the rainfall timeseries
#******************************************
#Start by setting the output filename and type
png(file=file.path(ReportDirectory,"Climate","WaterRainfallTimeSeries.png"),width=15,height=15,units="cm",res=600,family="Arial",pointsize=10)

#Prepare names of each line graph
panelLabels<-paste0(RainfallLocations," (",RainfallSites,")")
#create the plot
RainPlot <-xyplot(RainData.Year[,4:13],fontfamily="Arial",main="",xlab="",ylab="Days per year with more than the 95th percentile daily rain",
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


