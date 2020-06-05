#Prepare new precip and temperature series
#Detrended
#Climate changed

#Tim Kerr

#set libraries
library(zoo)
library(signal)

#Set directories
DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")

#Get the CLimate indices data
load(file=file.path(DataDirectory,"ClimateIndices","ClimateIndices.RData"))

#Set the start and finish dates of interest
StartDate <- as.Date("1910-01-01")
EndDate <- as.Date("2015-11-30")

#set the parameter of interest
parameterType <- "MinT"
#parameterType <- "MaxT"
#parameterType <- "Rainfall"
trends <- c(0.0100355, 0.0100795, 0.0103269, 0.0083211, 0.0101256, 0.0121704, 0.0026520) #trends  for MinT
#trends <- c(0.0100795,0.01652786,0.018591695,0.0184450796676363,0.00526578625083956,0.0174465881458899,0.0018172015908158)  #trends for MaxT, oC per year
AgentNumbers <- c(39066,4882,4881,4843,4836,4764,4698)

#loop for each station
for (index in seq(1:length(AgentNumbers))){
  #Get a data series
  #DataAgentNumber <- 39066
  DataAgentNumber <- AgentNumbers[index]
  FileName <- paste0(DataAgentNumber," extended.csv")
  parameterData <- read.zoo(file.path(DataDirectory,"StationData",parameterType,FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  colnames(parameterData) <- parameterType
  
  #Limit to the period of interest
  DatesOfInterest <- which((index(parameterData) >= StartDate) & (index(parameterData) <= EndDate))
  parameterData <- parameterData[DatesOfInterest]

  TrendAmount <- (as.numeric(EndDate - index(parameterData)))/365.25 * trends[index]
  
  
  DeTrend <- parameterData[,parameterType] + TrendAmount
  parameterData <- merge(parameterData,DeTrend)
  colnames(parameterData) <- c(parameterType,"DeTrend")
  
  #polyfit(as.numeric(index(DeTrend2015)),DeTrend2015,n=1)
  #polyfit(as.numeric(index(DeTrend2015)),parameterData,n=1)
  #Write output
  OutFilename <- paste0(DataAgentNumber,"_",format(StartDate,"%Y%m%d"),"-",format(EndDate,"%Y%m%d"),".csv")
  write.zoo(parameterData[,"DeTrend",drop=FALSE],file = file.path(DataDirectory,"AdjustedStationData","Detrended",parameterType,OutFilename),index.name="Date",quote=FALSE,col.names=TRUE,sep=",")
  

}