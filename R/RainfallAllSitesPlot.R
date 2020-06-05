#Script to present Wastewater variables
#Tim Kerr
#April 2016
#Aqualinc Research Ltd
#Prepared as part of project C16049 Impact of climate cycles and trends on Selwyn District water assets

#Load libraries
library(zoo)
library(reshape2)
#library(lattice)
#library(grid)
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

#Set directories
DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")
ReportDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Reports")

#Load GW and Rainfall data
#FileName <- "SQOut.csv"

#Get the Climate indices data
load(file=file.path(DataDirectory,"ClimateIndices","ClimateIndices.RData"))

#Get the rainfall site names
RainfallSites <- read.table(file.path(DataDirectory,"StationData","Rainfall","SDCplus10RainSites.txt"),sep="\t",skip=1)[,1]

#Set the start and finish dates of interest
StartDate <- as.Date("1910-01-01","%Y-%m-%d")
EndDate <- as.Date("2015-11-30","%Y-%m-%d")

#Set the different IPO phase years
IPOPosYears <- c(1922:1944,1979:1998)
IPONegYears <- c(1946:1977,2000:2010)

Rain95ths <- rep(NA,length(RainfallSites))
#loop for all the stations
for (DataAgentNumber in RainfallSites){
#for (DataAgentNumber in c(25821,4663)){
  loopCount <- which(RainfallSites==DataAgentNumber)
  FileName <- paste0(DataAgentNumber," extended.csv")
  parameterData <- read.zoo(file.path(DataDirectory,"StationData","Rainfall","Extended",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  #limit to the 1910 to 2015 period
  DatesOfInterest <- which((index(parameterData) >= StartDate) & (index(parameterData) <= EndDate))
  parameterData <- parameterData[DatesOfInterest,]
  percent95 <- quantile(parameterData,0.95,na.rm=TRUE)
  Rain95ths[loopCount]<- round(percent95,1)
  larger95 <- parameterData > percent95
  parameterData.month <- aggregate(larger95, as.yearmon, sum)         #get monthly totals of days with more than the 95th percentile
  parameterData.year <- aggregate(larger95,by=as.year,sum)
  if(which(RainfallSites==DataAgentNumber)==1){RainData.Year <- parameterData.year}
  else {RainData.Year <- merge(RainData.Year,parameterData.year)}
}
colnames(RainData.Year)<- paste0("R",RainfallSites)


#Calculate annual statistics
AnnualENSO <- aggregate(Indices[,12,drop=FALSE],by=as.numeric(format(index(Indices),"%Y")),mean)  
AnnualIPO <- aggregate(Indices[,19,drop=FALSE],by=as.numeric(format(index(Indices),"%Y")),mean)
AnnualSAM <- aggregate(Indices[,20,drop=FALSE],by=as.numeric(format(index(Indices),"%Y")),mean)

RainData.Year <- merge(AnnualENSO,AnnualIPO,AnnualSAM,RainData.Year,all=c(FALSE,FALSE,FALSE,TRUE)) #attach the climate indices
names(RainData.Year)[1:3] <- c("ENSO","IPO","SAM")                 #name the climate index columns


MeanRain <- rowMeans(RainData.Year[,c(-3:-1)],na.rm=TRUE)
#Check for a trend
lmRain <- lm(MeanRain ~ index(RainData.Year))
summary(lmRain)$r.squared
if(summary(lmRain)$coefficients[2,4]<0.05){ print(paste("Trend of",summary(lmRain)$coefficients[2,2],"m/year."))
} else {print("No trend.")}

#Check Rain for differences between IPO phases
RainIPOPos <- MeanRain[which(index(RainData.Year) %in% IPOPosYears)]
RainIPONeg <- MeanRain[which(index(RainData.Year) %in% IPONegYears)]
print(t.test(RainIPOPos,RainIPONeg))

#Check for a relationship to ENSO
lmRainENSO <- lm(MeanRain ~ RainData.Year[,1])
summary(lmRainENSO)

#Check for a relationship to SAM
lmRainENSO <- lm(MeanRain ~ RainData.Year[,3])
summary(lmRainENSO)

#*******************************************
#Generate a graph of the rainfall timeseries
#******************************************
#Start by setting the output filename and type
png(file=file.path(ReportDirectory,"Climate","RainfallTimeSeries.png"),width=15,height=15,units="cm",res=600,family="Arial",pointsize=10)

#Prepare names of each line graph
#panelLabels<-paste0(RainfallLocations," (",RainfallSites,")")
#create the plot
plot(RainData.Year[,c(-3:-1)],plot.type="single",col="grey",ylab="Frequency of high rainfall (days/year)")
lines(y=MeanRain,x=index(RainData.Year),lwd=2,col=rgb(49,132,155,255,maxColorValue=255),type="l")
abline(lmRain, col="red", lwd=2)
legend("topleft",paste("Trend =", round(as.numeric(lmRain$coef[2]),3),"\u00b1", round(coef(summary(lmRain))[,"Std. Error"][2],3),"days/year"),bty="n")


#print(RainPlot) #send it to the PNG output thingumy
dev.off()       #turn off the PNG output thingumy


