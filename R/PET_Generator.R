# Script to generate a daily evapotranspiration series from daily max and min temperatures
#and then adjust to match the Penman ET estimates from a reduced time period
# Tim Kerr
# March 2016
#Updated April 2020


#Load libraries
if (!require(zoo)) install.packages("zoo"); library(zoo)
if (!require(lubridate)) install.packages("lubridate"); library(lubridate)
if (!require(Evapotranspiration)) install.packages("Evapotranspiration"); library(Evapotranspiration)   #Provides the function to generate ET from daily temperature.

#Useful function to handle different date formats
Date_Format_Fix <- function(x) {
  parse_date_time(x, orders = c("dmy","ymd"), tz = "Etc/GMT-12")
}

#Set data directory
#DataDirectory <- "\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data\\StationData"
DataDirectory <- ("D:\\Projects\\Aqualinc\\projects\\SDC 2020\\Data\\StationData") #Copies on Tim's computer

T_AgentNumbers <- c(39066,4764,4698,4843,4881,4836)  #These are the agent numbers for the temperature sites used to estimate the McGiuinnessBordne PET
PET_AgentNumbers <- c(37332,4764,4698,4843,4881,17609)    #These are the Penman PET sites used to "adjust" the McGuinnessBordne PET estimates
elevations <- c(15,160,230,37,11,195)
latitudes <- c(-43.7615,-43.7935,-43.575,-43.493,-43.648,-43.493)
StartDate <- as.Date("1910-01-01")
EndDate <- as.Date("2019-06-30")

for (index in seq(1:length(T_AgentNumbers))){
#for (index in c(3)){
  #Load temperature data
  DataAgentNumber <- T_AgentNumbers[index]
  #DataAgentNumber <- 4881
  FileName <- paste0(DataAgentNumber," extended.csv")
  #MaxTData <- read.zoo(file.path(DataDirectory,"MaxT",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  #MaxTData <- read.zoo(file.path(DataDirectory,"MaxT",FileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  MaxTData <- read.zoo(file.path(DataDirectory,"MaxT",FileName),colClasses = c("character","numeric"), FUN = Date_Format_Fix,  drop=FALSE,header=TRUE,stringsAsFactors=FALSE,na.strings="",sep=",")
  
  colnames(MaxTData) <- "MaxT"
  #MinTData <- read.zoo(file.path(DataDirectory,"MinT",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  #MinTData <- read.zoo(file.path(DataDirectory,"MinT",FileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  MinTData <- read.zoo(file.path(DataDirectory,"MinT",FileName),colClasses = c("character","numeric"), FUN = Date_Format_Fix,  drop=FALSE,header=TRUE,stringsAsFactors=FALSE,na.strings="",sep=",")
  colnames(MinTData) <- "MinT"
  
  #Merge the Max and min and restrict to the period of interest.
  MaxMinTData <- merge(MaxTData,MinTData)
  DatesOfInterest <- which((index(MaxMinTData) >= StartDate) & (index(MaxMinTData) <= EndDate))
  MaxMinTData <- MaxMinTData[DatesOfInterest]
  
  #Load Penman PET data
  FileName <- paste0(PET_AgentNumbers[index]," extended.csv")
  #PenmanPETData <- read.zoo(file.path(DataDirectory,"PET",FileName),format="%Y-%m-%d",colClasses = c("Date","numeric",rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  PenmanPETData <- read.zoo(file.path(DataDirectory,"PET",FileName),colClasses = c("character","numeric"), FUN = Date_Format_Fix,  drop=FALSE,header=TRUE,stringsAsFactors=FALSE,na.strings="",sep=",")
  
  colnames(PenmanPETData) <- "Penman"
  
  #Create a date index as required by the ET.McGuinnessBordne function.
  Jindex <- zoo(x=seq(1:nrow(MaxMinTData)), order.by = index(MaxMinTData))
  
  #Combine the date, index, max and min data together in a list as required by the ET.McGuinnessBordne function
  TData <- list(Date.daily=index(MaxMinTData),J=Jindex,Tmax=MaxMinTData$MaxT,Tmin=MaxMinTData$MinT)
  
  #Get the default values for the "constants" list and edit them to match the location of interest
  data("constants")
  #constants$Elev <- 200
  constants$Elev <- elevations[index]
  constants$lambda <- 2.45
  #latitude <- -42.2
  latitude <- latitudes[index]
  constants$lat_rad <- latitude * pi / 180
  constants$Gsc <- 0.0820
  
  #Calculate the PET
  PET <- ET.McGuinnessBordne(TData,constants, ts="daily")
  McGuBoPETDaily <- PET$ET.Daily
  
  #Combine the McGuinnessBordne estimates with the Penman as columns in a single zoo series
  combined <- merge(PenmanPETData,McGuBoPETDaily)
  colnames(combined)<- c("Penman","McGuinnessBordne")
  
  #restrict to the period of interest
  combined <- window(combined,start=StartDate,end =EndDate)
  
  #Create a non linear model that relates the McGuinnessBordne estimates to the Penman estimates
  nlmodel <- nls(formula = Penman ~a + b * McGuinnessBordne^(c),data = as.data.frame(combined),start = list(a=0.1,b=1,c=1))
  
  #Apply the model to create an adjusted McGuinnessBordne series
  combined$McGAdj <- predict(nlmodel,newdata=as.data.frame(combined))
  combined$McGAdj <- ifelse(combined$McGAdj <0,0,combined$McGAdj)    #A quick check to set negative values to 0
  
  
  #Write output
  OutFilename <- paste0(PET_AgentNumbers[index],"_",format(StartDate,"%Y%m%d"),"-",format(EndDate,"%Y%m%d"),".csv")
  write.zoo(combined[,"McGAdj",drop=FALSE],file = file.path(DataDirectory,"PET",OutFilename),index.name="Date",quote=FALSE,col.names=TRUE,sep=",")
  
  #Generate a plot of the McGuiness vs Penman with the non linear adjustment line
  plot(x=combined$McGuinnessBordne,y=combined$Penman,ylim=c(0,10),xlim=c(0,10))
  abline(0,1,col="red")
  testData <- data.frame(McGuinnessBordne=seq(0,10,0.01))
  lines(testData$McGuinnessBordne,predict(nlmodel,newdata=testData),col="green",lwd=2)
  
  
  #Generate a plot of the AdjMcGu vs Penman
  plot(x=combined$McGAdj,y=combined$Penman)
  abline(0,1,col="red")
  #if (index == 1) {PETAll <- PET$ET.Daily} else {PETAll <- merge(PETAll,PET$ET.Daily) }
  
}
#colnames(PETAll) <- paste0("A",T_AgentNumbers)    #label the columns with agent number but prefix with an "A" to avoid having the labels as a number which can cause trouble

#So now I need to load in the observed PET and compare
#DataAgentNumber <- 4881
#FileName <- paste0(DataAgentNumber," extended.csv")
#PETData <- read.zoo(file.path(DataDirectory,"PET",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
#colnames(PETData) <- "PET"

#combined <- merge(PETData,PETAll$A4881)
#colnames(combined)<- c("Penman","McGuinnessBordne")
##plot(x=combined$McGuinnessBordne,y=combined$Penman)

###fit a linear model
##model = lm(formula = combined$Penman ~ combined$McGuinnessBordne, x = TRUE, y= TRUE)
##abline(model,col="red")

##fit a non-linear regression
#nlmodel <- nls(formula = Penman ~a + b * McGuinnessBordne^(c),data = as.data.frame(combined),start = list(a=0.1,b=1,c=1))

##plot the non-linear regression line
##testData <- data.frame(McGuinnessBordne=seq(0,10,0.01))
##lines(testData$McGuinnessBordne,predict(nlmodel,newdata=testData),col="green",lwd=2)
##PET_19100101_20151231 <- predict(nlmodel,newdata=as.data.frame(combined))

#combined$McGAdj <- predict(nlmodel,newdata=as.data.frame(combined))

#OutFilename <- paste0(DataAgentNumber,"_",format(StartDate,"%Y%m%d"),"-",format(EndDate,"%Y%m%d"),".csv")
#write.zoo(combined[,"McGAdj",drop=FALSE],file = file.path(DataDirectory,"PET",OutFilename),index.name="Date",quote=FALSE,col.names=TRUE,sep=",")

