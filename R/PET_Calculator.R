#R script for calculating PET from sunshine hour and radiation

#Aya Kashima
#Aqualinc
#March 2016

#This script reads in two .csv files, sticks their second columns together, adds another column to the beginning and makes the new column have the same word in it


#load libraries
library(zoo)
library(Evapotranspiration) #Provides the function to generate ET from radiation and sunshine hour

#Set directories
DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data\\PETDataAnalysis")


#load the data files into R

RadiationData <- read.zoo(file.path(DataDirectory,"Radiation4843.csv"),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=FALSE,sep=",")
SunshineData <- read.zoo(file.path(DataDirectory,"Sunshine4843.csv"),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=FALSE,sep=",")
MaxTData <- read.zoo(file.path(DataDirectory,"MaxT4843.csv"),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=FALSE,sep=",")
MinTData <- read.zoo(file.path(DataDirectory,"MinT4843.csv"),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=FALSE,sep=",")
WindData <- read.zoo(file.path(DataDirectory,"Wind4843.csv"),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=FALSE,sep=",")
RHMaxData <- read.zoo(file.path(DataDirectory,"RHMax4843.csv"),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=FALSE,sep=",")
RHMinData <- read.zoo(file.path(DataDirectory,"RHMin4843.csv"),format="%Y-%m-%d",colClasses = c("Date","numeric"), drop=FALSE,header=FALSE,sep=",")

#Get the default values for the "data" list and edit them to match the location of interest
data("processeddata")
data$Rs <- RadiationData
data$n <- SunshineData
data$Tmax <- MaxTData
data$Tmin <- MinTData
data$uz <- WindData
data$RHmax <- RHMaxData
data$RHmin <- RHMinData

#Get the default values for the "constants" list and edit them to match the location of interest
data("constants")
constants$Elev <- 30
constants$lambda <- 2.45
latitude <- -43.493
constants$lat_rad <- latitude * pi / 180
constants$Gsc <- 0.0820
constants$z <- 37
constants$sigma <- 4.903e-9
constants$as <- 0.25
constants$bs <- 0.5

#Calculate Penman PET
results <- ET.Penman(data, constants, ts="daily", solar="data", wind="yes", windfunction_ver = "1948",alpha = 0.25, z0 = 0.001)
RadiationET <- results$ET.Daily
results <- ET.Penman(data, constants, ts="daily", solar="sunshine hours", wind="yes", windfunction_ver = "1948",alpha = 0.25, z0 = 0.001)
SunshineET <- results$ET.Daily

plot(RadiationET, xlab="", ylab="PET", ylim=c(-0.2, 12))
lines(SunshineET,col="red")
legend("top", inset=0,legend=c("Radiation-based PET", "Sunshine hour-based PET"), col=c("black","red"),lty=1:1, cex=1.0, box.lty=0, bg="transparent")
#Prepare the column of names ready for the output
#NameColumn <- rep(ZoneName,nrow(Combined))

#Prepare the two columns of data
#DataColumns <- round(as.data.frame(coredata(Combined)),2)

#Stick it all together complete with dates and in the right order
#OutData <- cbind(NameColumn,index(Combined),DataColumns)

#Define the output file name
#OutFileName <- paste0(ZoneName,".csv")

#write to file
write.table(RadiationET,file = file.path(DataDirectory,"RadiationET"),quote=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
write.table(SunshineET,file = file.path(DataDirectory,"SunshineET"),quote=FALSE,col.names=FALSE,row.names=FALSE,sep=",")
