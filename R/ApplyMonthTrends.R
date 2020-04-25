#Prepare new PET series
#Detrended
#Climate changed
# Ammended August 2016 to include data to 30th June 2016, and to adjust to 2048 (instead of 2046)
# Used again in 2020 for another update

#Tim Kerr

#set libraries
library(zoo)
library(signal)

#Set directories
#DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")
DataDirectory <- ("D:\\Projects\\Aqualinc\\projects\\SDC 2020\\Data") #Copies on Tim's computer

#Set the start and finish dates of interest
StartDate <- as.Date("1910-01-01")
EndDate <- as.Date("2019-06-30")

#set the parameter of interest
parameterType <- "PET"

AgentNumbers <- c(37332,17609,4881,4843,4764,4698)           #PET sites

#loop for each station
for (index in seq(1:length(AgentNumbers))){
#for (index in c(6)){   #for testing
  #Get the data series
  DataAgentNumber <- AgentNumbers[index]
  #FileName <- paste0(DataAgentNumber,"_","19100101","-","20151130",".csv")
  FileName <- paste0(DataAgentNumber,"_","19100101","-","20190630",".csv")
  parameterData <- read.zoo(file.path(DataDirectory,"StationData",parameterType,FileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,sep=",")
  
  #Limit to the period of interest
  DatesOfInterest <- which((index(parameterData) >= StartDate) & (index(parameterData) <= EndDate))
  parameterData <- parameterData[DatesOfInterest]
  
  #Convert data to month values
  parameterData.month <- aggregate(parameterData, as.yearmon, mean)        #for temperature and PET get monthly averages
  months <- format(index(parameterData.month),"%m")
  
  #Calculate the monthly anomalies of the observed data
  ReferencePeriodData <- index(parameterData.month) < as.yearmon(StartDate) | index(parameterData.month) > as.yearmon(EndDate)
  ReferencePeriodOnly <- replace(parameterData.month,ReferencePeriodData,NA)
  anomaly <- parameterData.month-ave(ReferencePeriodOnly,months, FUN=function(x) mean(x, na.rm=TRUE)) 
  CombinedData <- merge(parameterData.month,anomaly,all=FALSE)
  colnames(CombinedData) <- c("PETMonthAve","Anomaly")
  
  #Now calculate the trend for each month
  months <- as.numeric(format(index(anomaly),"%m"))
  years <- as.numeric(format(index(anomaly),"%Y"))
  anomaly <- merge(anomaly,months,years)
  anomalyYbyMonth <- reshape(as.data.frame(anomaly),idvar="years",timevar="months",direction="wide")
  MonthTrends <- apply(anomalyYbyMonth[,2:13],2, function(x) summary(lm(x ~ years,data=anomalyYbyMonth))$coefficients[2,c(1,4)])
  MonthTrends[1,MonthTrends[2,]>0.001] <- 0   #set the statistically non-significant trends (at p=0.001) to 0

  #For the daily data series get the month value
  daymonths <- data.frame(index=1:length(parameterData),month=as.numeric(format(index(parameterData),"%m")))
  
  #create a column of the trend value
  trendLookup <- data.frame(month= seq(1,12,1),trend = MonthTrends[1,],row.names=NULL) #create a lookup table, include a sequence to enable resorting in date order
  trends <- merge(y=trendLookup,x=daymonths, by='month')                               #combine the lookup table to the daily data
  trends <- trends[order(trends$index),]                                               #get it back into date order
  
  #Calculate the difference due to the trend between any day and the "EndDate"
  TrendAmount <- (as.numeric(EndDate - index(parameterData)))/365.25 * trends$trend
  
  #Add the trend amount to the original data
  DeTrend <- parameterData[,parameterType] + TrendAmount
  parameterData <- merge(parameterData,DeTrend)
  colnames(parameterData) <- c(parameterType,"DeTrend")
  
  #Add on the climate change estimates
  #Make a look up table of climate change offset to 2048 c/o the values from Figure 2.2. in Bright(2011) converted to a 30 year shift
  #GCCAdjustLookup <-data.frame(month=seq(1,12,1),Adjust=c(3.9,2.1,1.8,1.5,0.6,0,0.3,0.6,1.8,2.1,2.4,3.3)/c(31,28,31,30,31,30,31,31,30,31,30,31)) #These values are for 30 years i.e. to 2046
  GCCAdjustLookup <-data.frame(month=seq(1,12,1),Adjust=c(4.16,2.24,1.92,1.6,0.64,0,0.32,0.64,1.92,2.24,2.56,3.52)/c(31,28,31,30,31,30,31,31,30,31,30,31)) #These values are for 32 years, i.e. to 2048
  
  GCCAdjusts <- merge(y=GCCAdjustLookup,x=daymonths, by='month')                               #combine the lookup table to the daily data
  GCCAdjusts <- GCCAdjusts[order(GCCAdjusts$index),]                                            #get it back into date order
  GCCModel  <- parameterData[,"DeTrend"] + GCCAdjusts[,"Adjust"]                                            #Add the climate change adjustment to the detrended data
  parameterData <- merge(parameterData,GCCModel)                                              #Add it to the zoo
  
  #Add on the Extrapolated changes
  ExtAdjustLookup <-  data.frame(month=seq(1,12,1),Adjust=MonthTrends[1,]*32)                  #Note that this take it out to 2048
  ExtAdjusts <- merge(y=ExtAdjustLookup,x=daymonths, by='month')                               #combine the lookup table to the daily data
  ExtAdjusts <- ExtAdjusts[order(ExtAdjusts$index),]                                            #get it back into date order
  Extrapolate  <- parameterData[,"DeTrend"] + ExtAdjusts[,"Adjust"]                                            #Add the climate change adjustment to the detrended data
  parameterData <- merge(parameterData,Extrapolate)                                              #Add it to the zoo
  
  #Write output
  OutFilename <- paste0(DataAgentNumber,"_",format(StartDate,"%Y%m%d"),"-",format(EndDate,"%Y%m%d"),".csv")
  write.zoo(parameterData[,"DeTrend",drop=FALSE],file = file.path(DataDirectory,"AdjustedStationData","Detrended",parameterType,OutFilename),index.name="Date",quote=FALSE,col.names=TRUE,sep=",")
  write.zoo(parameterData[,"GCCModel",drop=FALSE],file = file.path(DataDirectory,"AdjustedStationData","GCCModel",parameterType,OutFilename),index.name="Date",quote=FALSE,col.names=TRUE,sep=",")
  write.zoo(parameterData[,"Extrapolate",drop=FALSE],file = file.path(DataDirectory,"AdjustedStationData","Extrapolate",parameterType,OutFilename),index.name="Date",quote=FALSE,col.names=TRUE,sep=",")
  

}