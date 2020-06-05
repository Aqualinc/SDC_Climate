#Mixed effects model of MEIxtearly and the various IPO indices
#Tim Kerr

#set libraries
library(zoo)
library(signal)
library(lme4)

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
parameterType <- "MinT"
#parameterType <- "MaxT"
#parameterType <- "Rainfall"

#initialise a loop counter
counter <- 0
#loop for each station
#for (DataAgentNumber in c(4698,4722,4834,4836,4843,4881,4935,4513,4591)){
  for (DataAgentNumber in c(39066,4882,4881,4843,4836,4764,4698)){
  counter <- counter + 1
  FileName <- paste0(DataAgentNumber," extended.csv")
  parameterData <- read.zoo(file.path(DataDirectory,"StationData",parameterType,FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  colnames(parameterData) <- parameterType
  
  #Convert temperature data to month values
  if (parameterType == "Rainfall"){
    parameterData.month <- aggregate(parameterData, as.yearmon, sum)         #for rain get monthly totals
    parameterData.month <- parameterData.month ^ 0.5                         #square root transform data if the data is rainfall
  } else {
    parameterData.month <- aggregate(parameterData, as.yearmon, mean)        #for temperature get monthly averages
  }
  #parameterData.month <- aggregate(parameterData, as.yearmon, mean)
  #if (parameterType == "Rainfall"){parameterData.month <- parameterData.month ^ 0.5}   #square root transform data if the data is rainfall
  months <- format(index(parameterData.month),"%m")
  #convert to monthly anomalies
  ReferencePeriodData <- index(parameterData.month) < as.yearmon("1990-01-01") | index(parameterData.month) > as.yearmon("1999-12-31")
  ReferencePeriodOnly <- replace(parameterData.month[,1],ReferencePeriodData,NA)
  anomaly <- parameterData.month-ave(ReferencePeriodOnly,months, FUN=function(x) mean(x, na.rm=TRUE)) 
  
  #*********************************
  #Get IPO 
  #**********************************
  #Create a dataframe of data, MEIxtearly and the IPO indices
  IPO_data <- merge(anomaly,Indices[,c(12,17,18,19)])
  colnames(IPO_data) <- c("Data",colnames(IPO_data[,2:5]))
  #get rid of all the NA's
  IPO_data <- IPO_data[complete.cases(IPO_data)]
  #limit to the 1910 to 2015 period
  DatesOfInterest <- which((index(IPO_data) >= StartDate) & (index(IPO_data) <= EndDate))
  IPO_data <- IPO_data[DatesOfInterest]
  
  plot(IPO_data)
  #normalise each series
  normalisedData <- scale(IPO_data)
  plot(normalisedData)
  
  #categorise the IPO indices into +ve(true) and -ve (false) phases
  Phases <- normalisedData[,3:5] > 0
  IPO_PhaseData <- merge(normalisedData[,1:2],Phases)
  
  #create a trend series
  IPO_PhaseData$trend <- as.numeric(index(IPO_PhaseData))
  
  IPO.lm <- lm(Data ~ MEIxtearly ,data=as.data.frame(IPO_PhaseData))
  print(summary(IPO.lm))
  
  #IPO.ME <- lmer(Data ~ MEIxtearly + (1|IPO13), data=as.data.frame(IPO_PhaseData))
  #IPO.ME.null <- lmer(Data ~ MEIxtearly + (1|TPI13), data=as.data.frame(IPO_PhaseData),REML=FALSE)
  #IPO.ME <- lmer(Data ~ MEIxtearly + (1|TPI13), data=as.data.frame(IPO_PhaseData),REML=FALSE)
  #IPO.1 <- lm(Data ~ 1, data=as.data.frame(IPO_PhaseData))
  #IPO.2 <- lm(Data ~ MEIxtearly, data=as.data.frame(IPO_PhaseData))
  #IPO.3 <- lm(Data ~ MEIxtearly*TPI13, data=as.data.frame(IPO_PhaseData))
  #IPO.4 <- lm(Data ~ MEIxtearly+TPI13, data=as.data.frame(IPO_PhaseData))
  IPO.5 <- lm(Data ~ MEIxtearly*PDO13 + trend, data=as.data.frame(IPO_PhaseData))
  #print(anova(IPO.1,IPO.2))
  #print(anova(IPO.2,IPO.3))
  #print(anova(IPO.2,IPO.4))
  print(summary(IPO.5))
  
  old.par <- par(mfrow=c(2,3))
  boxplot(Data ~ IPO13,data=IPO_PhaseData,xlab = "IPO phase")
  boxplot(Data ~ TPI13,data=IPO_PhaseData,xlab = "TPI phase")
  boxplot(Data ~ PDO13,data=IPO_PhaseData,xlab = "PDO phase")
  par(old.par)
  
  #Generate box plots for the two phases
  
  #Plot the dotty plots
  old.par <- par(mfrow=c(2,3))
  plot(x=normalisedData$IPO13,y=normalisedData[,1])
  plot(x=normalisedData$TPI13,y=normalisedData[,1])
  plot(x=normalisedData$PDO13,y=normalisedData[,1])
  par(old.par)
  
  #Plot the density functions
  old.par <- par(mfrow=c(2,3))
  plot(density(normalisedData[,1]))
  plot(density(normalisedData$IPO13))
  plot(density(normalisedData$TPI13))
  plot(density(normalisedData$PDO13))
  par(old.par)
  
  #Plot the Q-Q plots against a normal distribution
  old.par <- par(mfrow=c(2,3))
  qqnorm(normalisedData[,1],ylab = parameterType)
  qqnorm(normalisedData$IPO13,ylab="IPO")
  qqnorm(normalisedData$TPI13,ylab="TPI")
  qqnorm(normalisedData$PDO13,ylab="PDO")
  par(old.par)
  
  #test for normality
  #print(shapiro.test(as.vector(normalisedData[,1])))
  
  #do forwards and backwards stepwise regression
  #step <- stepAIC(IPO.lm, direction="both")
  #print(step)
  
  #Create a model on the actual data, rather than the normalised anomaly
  #Climate.model <- lm(Data ~ MEIxtearly*PDO13 + trend, data=as.data.frame(IPO_PhaseData))
  
  if (counter == 1) 
    {TrendOut <- data.frame(Agent=DataAgentNumber,Parameter=parameterType,trend=summary()$coef["trend","Estimate"])
    }
    else {TrendOut <- rbind(TrendOut,c(DataAgentNumber,parameterType,summary(IPO.5)$coef["trend","Estimate"]))
    }
}
print(TrendOut)