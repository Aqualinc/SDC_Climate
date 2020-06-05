#Example R script for Aya

#Tim Kerr
#Aqualinc
#March 2016

#This script reads in two .csv files, sticks their second columns together, adds another column to the beginning and makes the new column have the same word in it


#load libraries
library(zoo)

#Set directories
DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data")

#Set agent numbers of data
PETAgentNumber <- 4698
RainfallAgentNumber <- 4698

#define the name needed for the first column
ZoneName <- "Zone15"

#load the data files into R
PETFileName <- paste0(PETAgentNumber,"_20151120-20151129.csv")
PETData <- read.zoo(file.path(DataDirectory,"PracticeData","PET",PETFileName),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=TRUE,sep=",")

RainfallFileName <- paste0(RainfallAgentNumber," extended.csv")
RainfallData <- read.zoo(file.path(DataDirectory,"PracticeData","Rainfall",RainfallFileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,stringsAsFactors=FALSE,na.strings="",sep=",")

#COmbined the two sets of data
Combined <- merge(RainfallData[,1],PETData[,1])

#Prepare the column of names ready for the output
NameColumn <- rep(ZoneName,nrow(Combined))

#Prepare the two columns of data
DataColumns <- round(as.data.frame(coredata(Combined)),2)

#Stick it all together complete with dates and in the right order
OutData <- cbind(NameColumn,index(Combined),DataColumns)

#Define the output file name
OutFileName <- paste0(ZoneName,".csv")

#write to file
write.table(OutData,file = file.path(DataDirectory,"PracticeData","Irricalc",OutFileName),quote=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
