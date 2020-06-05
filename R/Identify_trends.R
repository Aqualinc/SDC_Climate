#Script to identify the trend

#Tim Kerr
#March 2016
#Aqualinc Research Ltd.

#set libraries
if (!require(zoo)) install.packages("zoo"); library(zoo)
if (!require(signal)) install.packages("signal"); library(signal)  
if (!require(lme4)) install.packages("lme4"); library(lme4)  
if (!require(pracma)) install.packages("pracma"); library(pracma)
if (!require(lubridate)) install.packages("lubridate"); library(lubridate)

#Useful function to handle different date formats
Date_Format_Fix <- function(x) {
  parse_date_time(x, orders = c("dmy","ymd"), tz = "Etc/GMT-12")
}

#Set directories
#DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")
#DataDirectory <- ("H:\\WL Projects\\WL20030_SDC Impacts of Climate Change on Infrastructure\\Data") #From Tim's home connection
DataDirectory <- ("D:\\Projects\\Aqualinc\\projects\\SDC 2020\\Data") #Copies on Tim's computer

#Get the Climate indices data
#load(file=file.path(DataDirectory,"ClimateIndices","ClimateIndices.RData"))

#Set the start and finish dates of interest
StartDate <- as.yearmon("1910-01")
EndDate <- as.yearmon("2019-06")

#Create a filter used for low pass filtering the IPO/PDO indices
#cf13 <- cheby1(2,Rp=0.1,W=1/156,type="low")  #second order 13 year low pass filter with 0.1 dB of band pass ripple
#cf0.5 <- cheby1(2,Rp=0.1,W=1/6,type="low")  #second order 13 year low pass filter with 0.1 dB of band pass ripple

#set the parameter of interest
parameterType <- "MinT"
#parameterType <- "MaxT"
#parameterType <- "PET"
#parameterType <- "Rainfall"

#initialise a loop counter
counter <- 0
#loop for each station
#for (DataAgentNumber in c(4698,4722,4834,4836,4843,4881,4935)){    #Rainfall sites
for (DataAgentNumber in c(39066,4882,4881,4843,4836,4764,4698)){              #Temperature sites
#  for (DataAgentNumber in c(37332,17609,4881,4843,4764,4698)){              #PET sites
#    for (DataAgentNumber in c(4698)){              #PET sites
 #browser()
  counter <- counter + 1
  #open the time series
  if (parameterType == "PET"){
    FileName <- paste0(DataAgentNumber,"_","19100101","-","20151130",".csv")
    parameterData <- read.zoo(file.path(DataDirectory,"StationData",parameterType,FileName),colClasses = c("character","numeric"), FUN = parse_date,drop=FALSE,header=TRUE,sep=",")
  } else if (parameterType == "Rainfall"){
  FileName <- paste0(DataAgentNumber," extended.csv")
  parameterData <- read.zoo(file.path(DataDirectory,"StationData",parameterType,FileName),colClasses = c("character","numeric",rep("NULL",9)), FUN = parse_date,drop=FALSE,header=TRUE,stringsAsFactors=FALSE,na.strings="",sep=",")
  } else {
    FileName <- paste0(DataAgentNumber," extended.csv")
    parameterData <- read.zoo(file.path(DataDirectory,"StationData",parameterType,FileName),colClasses = c("character","numeric"), FUN = Date_Format_Fix,  drop=FALSE,header=TRUE,stringsAsFactors=FALSE,na.strings="",sep=",")
  }
  

  colnames(parameterData) <- parameterType
  
  #Convert data to month values
  if (parameterType == "Rainfall"){
    parameterData.month <- aggregate(parameterData, as.yearmon, sum)         #for rain get monthly totals
    parameterData.month <- parameterData.month ^ 0.5                         #square root transform data if the data is rainfall
  } else if (parameterType == "PET"){
    parameterData.month <- aggregate(parameterData, as.yearmon, sum)         #for PET get monthly totals
  }else {
    parameterData.month <- aggregate(parameterData, as.yearmon, mean)        #for temperature and PET get monthly averages
  }
  months <- format(index(parameterData.month),"%m")
  
  #Create a dataframe of the monthly data, MEIxtearly and the "TPI" index
  #CombinedData <- merge(parameterData.month,Indices[,c(12,18)])
  #colnames(CombinedData) <- c("Data",colnames(CombinedData[,c(2,3)]))
  
  CombinedData <- parameterData
  
  #get rid of all the NA's
  CombinedData <- CombinedData[complete.cases(CombinedData)]
  
  #limit to the period of interest
  DatesOfInterest <- which((index(CombinedData) >= StartDate) & (index(CombinedData) <= EndDate))
  CombinedData <- CombinedData[DatesOfInterest]
  
  #detrend the monthly data
  browser()
  Detrend <- detrend(as.vector(CombinedData$Data))
  CombinedData <- merge(CombinedData,Detrend)
  colnames(CombinedData) <- c(colnames(CombinedData[,1:3]),"Detrend")
  
  #Calculate the monthly anomalies of the observed data
  ReferencePeriodData <- index(parameterData.month) < as.yearmon("1910-01-01") | index(parameterData.month) > as.yearmon("2019-12-31")
  ReferencePeriodOnly <- replace(CombinedData[,"Data"],ReferencePeriodData,NA)
  anomaly <- CombinedData$Data-ave(ReferencePeriodOnly,months, FUN=function(x) mean(x, na.rm=TRUE)) 
  CombinedData <- merge(CombinedData,anomaly,all=FALSE)
  colnames(CombinedData) <- c(colnames(CombinedData[,1:4]),"Anomaly")
  
  #Now calculate the trend for each month
  months <- as.numeric(format(index(anomaly),"%m"))
  years <- as.numeric(format(index(anomaly),"%Y"))
  anomaly <- merge(anomaly,months,years)
  anomalyYbyMonth <- reshape(as.data.frame(anomaly),idvar="years",timevar="months",direction="wide")
  #monthTrends<-apply(anomalyYbyMonth[,2:13],2,function(x) polyfit(anomalyYbyMonth$years,x,n=1))
  
  MonthTrends <- apply(anomalyYbyMonth[,2:13],2, function(x) summary(lm(x ~ years,data=anomalyYbyMonth))$coefficients[2,c(1,4)])
  
  #MonthTrends[1,MonthTrends[2,]>0.001] <- 0   #set the statistically non-significant trends (at p=0.001) to 0
  ##Plot here each month together with its trend line
  layout(matrix(1:1),respect=FALSE)
  par(mar=c(4.4,4.4,4.4,4.4))
  plot(x=anomalyYbyMonth$years,y=anomalyYbyMonth$anomaly.1,type="l")
  abline()
  
  # #calculate a 13 year low pass filtered version of the anomalies
  # Filt_13yr <- scale(filtfilt(cf13,CombinedData$Anomaly))
  # Filt_6mo <- scale(filtfilt(cf0.5,CombinedData$Anomaly))
  # CombinedData <- merge(CombinedData,Filt_13yr,Filt_6mo)
  # colnames(CombinedData) <- c(colnames(CombinedData[,1:5]),"Anomaly13yr","Anomaly6mo")
  # 
  # #plot it
  #   #plot(CombinedData[,c(1,5,7,2,6,3)],nc=1,xlab="",main=DataAgentNumber)
  # layout(matrix(1:5, ncol = 1), widths = 1, heights = c(1.5,1.5,1.5,1.5,1.5), respect = FALSE)
  # old.par <- par(mar = c(0,4.1,4.1,2.1))
  # plot(CombinedData[,1],xaxt='n',ylab="Observed",main=DataAgentNumber)
  # par(mar=c(0,4.1,0,2.1))
  # plot(CombinedData[,7],xaxt='n',ylab = "anomaly",xlab="")
  # plot(CombinedData[,2],xaxt='n',ylab = "MEI",xlab="")
  # colors <- c("blue","red")[(CombinedData[,6] > 0) + 1]
  # barplot(CombinedData[,6],col=colors,border=colors,xaxt='n',ylab="Anomaly smoothed")
  # par(mar=c(4.1,4.1,0,2.1))
  # colors <- c("blue","red")[(CombinedData[,3] > 0) + 1]
  # barplot(CombinedData[,3],col=colors,border=colors,ylab="TPI")
  # par(old.par)
  # 
  # #categorise the IPO indices into +ve(true) and -ve (false) phases
  # Phases <- CombinedData[,3,drop=FALSE] > 0
  # IPO_PhaseData <- merge(CombinedData[,1:2],Phases)
  # 
  # #create a trend series
  # IPO_PhaseData$trend <- as.numeric(index(IPO_PhaseData))
  # 
  # Climate.model <- lm(Data ~ TPI13*MEIxtearly + trend ,data=as.data.frame(IPO_PhaseData))
  # print(summary(Climate.model))
  # 
  # if (counter == 1) 
  # {TrendOut <- data.frame(Agent=DataAgentNumber,Parameter=parameterType,trend=summary(Climate.model)$coef["trend","Estimate"])
  # }
  # else {TrendOut <- rbind(TrendOut,c(DataAgentNumber,parameterType,summary(Climate.model)$coef["trend","Estimate"]))
  #}
}
#print(TrendOut)


