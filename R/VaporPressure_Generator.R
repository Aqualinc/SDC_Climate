#Script to generate vapor pressure timeseries
#
#Issues
#It is necesary to check the observed data for nodata otherwise it can get written to the output file.
#This has happened where the station radiaion data have additional dates without values at the end of the file
#I think it just requires a filter for NA's when generating "PossibleRadiation" in fact the comment associated with that sugests that it needs to be done but it was never actually implemented



#libraries
#library(wavelets)
library(Hmisc)  #needed for the yearDays function
library(lubridate)
library(zoo)

#set data directory
DataDirectory <- "\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data\\StationData"

for (DataAgentNumber in c(4764,4698,4843,4881,4836)){
  cat(DataAgentNumber,"\n")
  #Load data
  #DataAgentNumber <- 4843
  FileName <- paste0(DataAgentNumber," extended.csv")
  RadData <- read.zoo(file.path(DataDirectory,"VaporP",FileName),format="%Y-%m-%d",colClasses = c("Date",NA,rep("NULL",9)), drop=FALSE,header=TRUE,,stringsAsFactors=FALSE,na.strings="",sep=",")
  colnames(RadData) <- "VaporP"
  RadData$JulDay <- yday(index(RadData))
  
  
  #Pre-allocate a daily zoo from 1/1/1910 to 31/12/2015
  StartDate <- as.Date("1910-01-01")
  EndDate <- as.Date("2015-12-31")
  #StartDate <- as.Date("1910-01-01")
  #EndDate <- as.Date("1914-12-31")
  StartYear <- as.numeric(format(StartDate,"%Y"))
  EndYear <- as.numeric(format(EndDate,"%Y"))
  RaddComplete <- zoo(VaporP <- 0,seq(from=StartDate,to=EndDate,by="day"))
  JulDay <- yday(index(RaddComplete))
  RaddComplete <- cbind(VaporP=RaddComplete,JulDay)
  #Get the day of year for each day
  for (year in seq(from=StartYear,to=EndYear)){
    #find the number of days in the year
    maxday <- yearDays(as.Date(paste0(year,"-01-01")))
    #loop through each day
    for (index in seq(1:maxday)){
      #Create the date
      CurrentDate <- as.Date(paste(year,index),format="%Y %j")
      #If it is 366, then set it to 365
      day <- index
      if (index == 366){day<- 365}
      
      #get all the non-NA vapor pressure values for that day number from the observations
      PossibleVaporP <- RadData$VaporP[which(RadData$JulDay == day)]
      
      #randomly select one
      vaporP <- sample(PossibleVaporP,1)
      #set that as the value for the day.
      RaddComplete$VaporP[CurrentDate] <- format(round(vaporP,2),nsmall=2)
    }
    cat(year," ")
  }
  cat("\n")
  OutFilename <- paste0(DataAgentNumber,"_",format(StartDate,"%Y%m%d"),"-",format(EndDate,"%Y%m%d"),".csv")
  write.zoo(RaddComplete[,"VaporP",drop=FALSE],file = file.path(DataDirectory,"VaporP",OutFilename),index.name="Date",quote=FALSE,col.names=TRUE,sep=",")
}
