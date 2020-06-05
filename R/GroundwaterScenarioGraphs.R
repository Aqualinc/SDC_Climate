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
library(matrixStats)


#Some usefull functions
as.year <- function(x) floor(as.numeric(as.yearmon(x)))

#Set directories
DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")
ReportDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Reports")

#Load GW and Rainfall data
#FileName <- "DetrendedOut.csv"

#Get the CLimate indices data
load(file=file.path(DataDirectory,"ClimateIndices","ClimateIndices.RData"))

#Set the start and finish dates of interest
StartDate <- as.Date("1910-01-01","%Y-%m-%d")
EndDate <- as.Date("2015-11-30","%Y-%m-%d")
GWSites <- c("M360424","M360599","M360217","M351080","L350180","L361226","L360092","L360124","M351000")
NoOfSites <- length(GWSites)
#GWSites <- c("M351000")
GWLocations <- c("Doyleston","Lincoln","Rolleston","Christchurch - West Melton","Darfield","Te Pirita","Charing Cross","Bankside","West Melton")
#GWLocations <- c("West Melton")
Scenarios <- c("SQ","Detrended","GCC","CPWEffect","Dryland")

#Groundwater data
for (ScenarioType in Scenarios){
#  for (ScenarioType in c("Detrended","GCC")){
#ScenarioType <- "SQ"
  #Load GW and Rainfall data
  FileName <- paste0(ScenarioType,"Out.csv")

GWColNames <- paste0(ScenarioType,GWSites)
parameterData <- read.table(file.path(DataDirectory,"GroundwaterModel",FileName),header=TRUE,sep=",",colClasses=c("character",rep("numeric",NoOfSites)))
GWAllTimeSeries <- zoo(parameterData[,-1], order.by = as.yearmon(parameterData[,1],format="%b-%Y"))
GWTimeSeries <- GWAllTimeSeries[,GWColNames]   #Just get the sites we are interested in
#browser()
if(ScenarioType == "SQ"){GWAllScenarios <- GWTimeSeries}
else {GWAllScenarios <- merge(GWAllScenarios,GWTimeSeries,fill=NA)}
}

for (well in seq(1,NoOfSites)){
  wellName <- GWLocations[well]
  GraphFileName <- paste0("GWat",paste0(GWLocations,"_",GWSites)[well],".png")
png(file=file.path(ReportDirectory,"Groundwater",GraphFileName),width=15,height=15,units="cm",res=600,family="Arial",pointsize=10)
#plotIndices <- c(well,well+8,well+16,well+24,well+32)
plotIndices <- well + seq(from=0,length.out=length(Scenarios), by=NoOfSites)
plot(GWAllScenarios[,plotIndices],plot.type="single",col=c("black","red","blue","green","orange"),
     xlab="",
     ylab = "Groundwater level (m amsl)",
     xlim=as.yearmon(c("1970-01","1980-01")))
legend("topleft",legend=Scenarios,col=c("black","red","blue","green","orange"),lty=1,bty="n")
dev.off()
}

#Calculate interesting statistics
#create empty dataframe ready for populating
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)

AnnualMaxs <- aggregate(GWAllScenarios,by=as.year,max)
AnnualMins <- aggregate(GWAllScenarios,by=as.year,min)

StatsTableFileName <- paste0("GWStatsTables.csv")
if (file.exists(file.path(ReportDirectory,"Groundwater",StatsTableFileName))) file.remove(file.path(ReportDirectory,"Groundwater",StatsTableFileName))
#AllGWStats <- list()
for (well in seq(1,NoOfSites)){
GWStats <- data.frame(matrix(ncol=7,nrow=length(Scenarios)))
colnames(GWStats) <- c("average","AnnualMax90th","AnnualMax96th","max","AnnualMin10th","AnnualMin4th","min")
rownames(GWStats) <- Scenarios
#well <- 1
scenarioIndices <- well + seq(from=0,length.out=length(Scenarios), by=NoOfSites)
#scenarioIndices <- c(well,well+8,well+16,well+24,well+32)   #This needs to be manually changed for different numbers of scenarios. Ideally it would calculate this automatically
GWStats$average <- colMeans(GWAllScenarios[,scenarioIndices])
GWStats$max <- colMax(GWAllScenarios[,scenarioIndices])
GWStats$min <- colMin(GWAllScenarios[,scenarioIndices])

GWStats$AnnualMax90th <- colQuantiles(AnnualMaxs[,scenarioIndices],probs=0.9)
GWStats$AnnualMax96th <- colQuantiles(AnnualMaxs[,scenarioIndices],probs=0.96)
GWStats$AnnualMin10th <- colQuantiles(AnnualMins[,scenarioIndices],probs=0.1)
GWStats$AnnualMin4th <- colQuantiles(AnnualMins[,scenarioIndices],probs=0.04)
#AllGWStats[[well]]<- GWStats
write.table(paste0(GWLocations,"_",GWSites)[well], file=file.path(ReportDirectory,"Groundwater",StatsTableFileName), col.names=FALSE,row.names=FALSE, sep=",", append=TRUE, quote=FALSE)
write.table(round(GWStats,1), file=file.path(ReportDirectory,"Groundwater",StatsTableFileName), col.names=NA, sep=",", append=TRUE, quote=FALSE)
}
#now write this to a csv with a line between each and a line with the well number on it.

