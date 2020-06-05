#Re create Brett Painter PET trend
#Tim Kerr
#March 2016
#Aqualinc Research Ltd.

#set libraries
library(zoo)
library(signal)
library(lme4)
library(pracma)

#Set directories
DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")

#Get the CLimate indices data
load(file=file.path(DataDirectory,"ClimateIndices","ClimateIndices.RData"))
parameterType <- "PET"
#initialise a loop counter
counter <- 0
#loop for each station
as.year <- function(x) floor(as.numeric(as.yearmon(x)))

for (DataAgentNumber in c(4881)){              #PET sites
  counter <- counter + 1
  FileName <- paste0(DataAgentNumber," extended.csv")
  parameterData <- read.zoo(file.path(DataDirectory,"StationData",parameterType,FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  colnames(parameterData) <- parameterType
  #get annual totals
  #Convert  data to year values
  
    parameterData.year <- aggregate(parameterData, as.year, sum)         #get year totals

  years <- format(index(parameterData.year),"%Y")
  
  #Create a dataframe of the annual data, MEIxtearly and the "TPI" index
  #CombinedData <- merge(parameterData.month,Indices[,c(12,18)])
  #colnames(CombinedData) <- c("Data",colnames(CombinedData[,c(2,3)]))
  
  #get rid of all the NA's
 # CombinedData <- CombinedData[complete.cases(CombinedData)]
  
  #limit to the 1910 to 2015 period
  #DatesOfInterest <- which((index(CombinedData) >= StartDate) & (index(CombinedData) <= EndDate))
  #CombinedData <- CombinedData[DatesOfInterest]
}