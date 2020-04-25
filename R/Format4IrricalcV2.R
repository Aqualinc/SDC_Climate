#Script to combine PET and Rainfall for input to Irricalc

#Tim Kerr
#Aqualinc
#March 2016
#Updated April 2020

#This script reads in two .csv files, sticks their second columns together, adds another column to the beginning and makes the new column have the same word in it


#load libraries
if (!require(zoo)) install.packages("zoo"); library(zoo)
if (!require(lubridate)) install.packages("lubridate"); library(lubridate)

#Useful function to handle different date formats
Date_Format_Fix <- function(x) {
  parse_date_time(x, orders = c("dmy","ymd"), tz = "Etc/GMT-12")
}

#Set directories
#DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")
DataDirectory <- ("D:\\Projects\\Aqualinc\\projects\\SDC 2020\\Data") #Copies on Tim's computer

#Set scenario type
#scenario <- "DeTrended"
#scenario <- "GCCModel"
scenario <- "Extrapolate"
#scenario <- "StatusQuo"
if (scenario == "StatusQuo") DataSubdirectory <- DataSubdirectory <- "StationData" else DataSubdirectory <- file.path("AdjustedStationData",scenario)

zoneAgents <- data.frame(zones = 13:19, RainAgents = c(4935,4722,4698,4843,4881,4836,4834),PETAgents=c(37332,4764,4698,4843,4881,17609,17609))

#loop through each zone

for (zone in 1:7){

  #Set agent numbers of data
  PETAgentNumber <- zoneAgents[zone,"PETAgents"]
  RainfallAgentNumber <- zoneAgents[zone,"RainAgents"]
  
  #define the name needed for the first column
  ZoneName <- paste0("Zone",zoneAgents[zone,"zones"],substr(scenario,1,3))
  
  #load the data files into R
  PETFileName <- paste0(PETAgentNumber,"_19100101-20190630.csv")
  PETData <- read.zoo(file.path(DataDirectory,DataSubdirectory,"PET",PETFileName),colClasses = c("Date","numeric"),FUN = Date_Format_Fix, drop=FALSE,header=TRUE,sep=",")
  
  RainfallFileName <- paste0(RainfallAgentNumber," extended.csv")
  RainfallData <- read.zoo(file.path(DataDirectory,"StationData","Rainfall",RainfallFileName),colClasses = c("character","numeric"),quote="", FUN = Date_Format_Fix,  drop=FALSE,header=TRUE,stringsAsFactors=FALSE,na.strings="",sep=",")

  #Combine the two sets of data
  Combined <- merge(RainfallData[,1],PETData[,1],all=FALSE)
  
  #Prepare the column of names ready for the output
  NameColumn <- rep(ZoneName,nrow(Combined))
  
  #Prepare the two columns of data
  DataColumns <- round(as.data.frame(coredata(Combined)),2)
  
  #Stick it all together complete with dates and in the right order
  OutData <- cbind(NameColumn,index(Combined),DataColumns)
  
  #Define the output file name
  OutFileName <- paste0(ZoneName,".csv")
  
  #write to file
  write.table(OutData,file = file.path(DataDirectory,DataSubdirectory,"Irricalc",OutFileName),quote=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
}