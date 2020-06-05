#Script to carry out multiple linear regression of time series vs ENSO, IPO, SAM indices and a trend.

#Tim Kerr

#set libraries
library(zoo)
library(signal)

#Set directories
DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")

#Get the CLimate indices data
load(file=file.path(DataDirectory,"ClimateIndices","ClimateIndices.RData"))

#Set the start and finish dates of interest
StartDate <- as.yearmon("1910-01")
EndDate <- as.yearmon("2015-11")

#Create a filter used for low pass filtering the IPO/PDO indices
cf <- cheby1(2,Rp=0.1,W=1/156,type="low")  #second order 13 year low pass filter with 0.1 dB of band pass ripple

#set the parameter of interest
#parameterType <- "MinT"
#parameterType <- "MaxT"
parameterType <- "Rainfall"

#loop for each station
for (DataAgentNumber in c(4513)){
  #Get a data series
  #DataAgentNumber <- 39066
  FileName <- paste0(DataAgentNumber," extended.csv")
  parameterData <- read.zoo(file.path(DataDirectory,"StationData",parameterType,FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  colnames(parameterData) <- parameterType
  #MinTData <- read.zoo(file.path(DataDirectory,"StationData","MinT",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  #colnames(MinTData) <- "MinT"
  
  #Convert temperature data to month values
  #temperature.month <- aggregate(MinTData, as.yearmon, mean)
  if (parameterType == "Rainfall"){
    parameterData.month <- aggregate(parameterData, as.yearmon, sum)         #for rain get monthly totals
    parameterData.month <- parameterData.month ^ 0.5                         #square root transform data if the data is rainfall
    } else {
    parameterData.month <- aggregate(parameterData, as.yearmon, mean)        #for temperature get monthly averages
    }
  #if (parameterType == "Rainfall"){parameterData.month <- parameterData.month ^ 0.5}   
  months <- format(index(parameterData.month),"%m")
  #convert to monthly anomalies
  ReferencePeriodData <- index(parameterData.month) < as.yearmon("1990-01-01") | index(parameterData.month) > as.yearmon("1999-12-31")
  ReferencePeriodOnly <- replace(parameterData.month[,1],ReferencePeriodData,NA)
  anomaly <- parameterData.month-ave(ReferencePeriodOnly,months, FUN=function(x) mean(x, na.rm=TRUE))
  #anomaly <- scale(parameterData.month)
  
  #*********************************
  #Process ENSO indices
  #**********************************
  #Create a dataframe of temperature, Nino3.4Had, SOI, MEIxt, MEIxtearly, CEI
  ENSO_data <- merge(anomaly,Indices[,c(5,8,11,12,13)])
  colnames(ENSO_data) <- c("Data",colnames(ENSO_data[,2:6]))
  #get rid of all the NA's
  ENSO_data <- ENSO_data[complete.cases(ENSO_data)]
  #limit to the 1910 to 2015 period
  DatesOfInterest <- which((index(ENSO_data) >= StartDate) & (index(ENSO_data) <= EndDate))
  ENSO_data <- ENSO_data[DatesOfInterest]
  
  plot(ENSO_data)
   
  #normalise each series
  normalisedData <- scale(ENSO_data)
  plot(normalisedData)
  
  #create a trend series
  normalisedData$trend <- as.numeric(index(normalisedData))
  
  #Model to find the best ENSO index
  ENSO.lm <- lm(Data ~ Nino3.4Had + SOI + MEIxt + MEIxtearly + CEI,data=normalisedData)
  print(summary(ENSO.lm))
  
  #model to find the trend after considering MEIxtearly
  trial.lm <- lm(Data ~MEIxtearly + trend,data=normalisedData)
  print(summary(trial.lm))
  
  #Plot the dotty plots
  old.par <- par(mfrow=c(2,3))
  plot(x=normalisedData$Nino3.4Had,y=normalisedData[,1])
  plot(x=normalisedData$SOI,y=normalisedData[,1])
  plot(x=normalisedData$MEIxt,y=normalisedData[,1])
  plot(x=normalisedData$MEIxtearly,y=normalisedData[,1])
  plot(x=normalisedData$CEI,y=normalisedData[,1])
  par(old.par)
  
  #Plot the density functions
  old.par <- par(mfrow=c(2,3))
  plot(density(normalisedData[,1]))
  plot(density(normalisedData$Nino3.4Had))
  plot(density(normalisedData$SOI))
  plot(density(normalisedData$MEIxt))
  plot(density(normalisedData$MEIxtearly))
  plot(density(normalisedData$CEI))
  par(old.par)
  
  #Plot the Q-Q plots against a normal distribution
  old.par <- par(mfrow=c(2,3))
  qqnorm(normalisedData[,1],ylab = parameterType)
  qqnorm(normalisedData$Nino3.4Had,ylab="Nino")
  qqnorm(normalisedData$SOI,ylab="SOI")
  qqnorm(normalisedData$MEIxt,ylab="MEIxt")
  qqnorm(normalisedData$MEIxtearly,ylab="MEIxtearly")
  qqnorm(normalisedData$CEI,ylab="CEI")
  par(old.par)
  
  #test for normality
  print(shapiro.test(as.vector(normalisedData[,1])))
  
  #do forwards and backwards stepwise regression
  step <- stepAIC(ENSO.lm, direction="both")
  print(step)
  
  
}
