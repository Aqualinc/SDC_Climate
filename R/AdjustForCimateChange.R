#Adjust detrended data for climate change

#Tim Kerr
# March 2016

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
#load(file=file.path(DataDirectory,"ClimateIndices","ClimateIndices.RData"))

#Set the start and finish dates of interest
StartDate <- as.Date("1910-01-01")
EndDate <- as.Date("2015-11-30")

#****************************
# For MfE adjustment uncomment the following line and one of the two after that
#*************************
AdjDirectory <- "Adj2046MfE"
#trendRate  <- 0.018 #degrees per year this is the MfE temperature rate based on the MfE guidance of 0.9 oC between 1990 and 2040
#trendRate  <- -0.0002 #percent rainfall per year this is the MfE rainfall rate based on the MfE guidance of -1 % between 1990 and 2040
trendRate <- 1.2 #mm per year based on the downscaled A1B scenario as given in section 2.4 of Bright (2011),"Projected Effects..." for the Canterbury Plains of a change of 60 mm annual PET between 1990 and 2040

#****************************
# For observed trend adjustment uncomment the following line and one of the two after that
#*************************
#AdjDirectory <- "Adj2046Extrapolated"
#trendRate <- c(0.0100355, 0.0100795, 0.0103269, 0.0083211, 0.0101256, 0.0121704, 0.0026520) #this is observed change of MinT from 1910-2015
#trendRate <- c(0.0175523, 0.0165279, 0.0185917, 0.0184451, 0.0052658, 0.0174466, 0.0018172) #this is observed change of MaxT from 1910-2015


Offsets <- trendRate  * (2046 - 2016)

#set the parameter of interest
#parameterType <- "MinT"
#parameterType <- "MaxT"
#parameterType <- "Rainfall"
parameterType <- "PET"
if (parameterType == "Rainfall"){
  AgentNumbers <- c(4698,4722,4834,4836,4843,4881,4935)   #for rainfall
  } else if (parameterType == "PET") {
    AgentNumbers <- c(37332,4764,4698,4843,4881,17609)  #For PET
    } else {
  AgentNumbers <- c(39066,4882,4881,4843,4836,4764,4698)  #For MaxT and MinT
}

#loop for each station
for (index in seq(1:length(AgentNumbers))){
  #Get a data series
  #DataAgentNumber <- 39066
  DataAgentNumber <- AgentNumbers[index]
  if (parameterType == "Rainfall"){
    FileName <- paste0(DataAgentNumber," extended.csv")
    parameterData <- read.zoo(file.path(DataDirectory,"StationData",parameterType,FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
    } else if (parameterType == "PET"){
      FileName <- paste0(DataAgentNumber,"_",format(StartDate,"%Y%m%d"),"-",format(EndDate,"%Y%m%d"),".csv")
      parameterData <- read.zoo(file.path(DataDirectory,"StationData",parameterType,FileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,sep=",")
      
    } else {
  FileName <- paste0(DataAgentNumber,"_",format(StartDate,"%Y%m%d"),"-",format(EndDate,"%Y%m%d"),".csv")
  parameterData <- read.zoo(file.path(DataDirectory,"AdjustedStationData","Detrended",parameterType,FileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,sep=",")
  }
  colnames(parameterData) <- parameterType
  
  #Limit to the period of interest
  DatesOfInterest <- which((index(parameterData) >= StartDate) & (index(parameterData) <= EndDate))
  parameterData <- parameterData[DatesOfInterest]
  
  #adjust the data
  if(length(Offsets) > 1){Offset <- Offsets[index]} else {Offset = Offsets}
  if (parameterType == "Rainfall"){
    Data2046 <- parameterData[,parameterType] * Offset
    } else {
    Data2046 <- parameterData[,parameterType] + Offset
    }
  parameterData <- merge(parameterData,Data2046)
  colnames(parameterData) <- c(parameterType,"Data2046")
  
  #write to file
  write.zoo(parameterData[,"Data2046",drop=FALSE],file = file.path(DataDirectory,"AdjustedStationData",AdjDirectory,parameterType,FileName),index.name="Date",quote=FALSE,col.names=TRUE,sep=",")
  
  
}