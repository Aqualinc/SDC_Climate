#R script to read in Climate indices data and reformat them into R zoo objects
#Tim Kerr
#2016

# Future changes
# Why not get it all direct frm the websites?

#Load libraries
if (!require(zoo)) install.packages('zoo'); library(zoo)
if (!require(reshape2)) install.packages('reshape2'); library(reshape2)
if (!require(quantmod)) install.packages('quantmod'); library(quantmod)
if (!require(Hmisc)) install.packages('Hmisc'); library(Hmisc)
if (!require(signal)) install.packages('signal'); library(signal)

#Set directories
#DataDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\WL Projects\\WL20030_SDC Impacts of Climate Change on Infrastructure\\Data\\ClimateIndices")
DataDirectory <- ("D:\\Projects\\Aqualinc\\projects\\SDC 2020\\Data\\ClimateIndices") #Rainfall.NZ server

#Create a filter used for low pass filtering the IPO/PDO indices
cf <- cheby1(2,Rp=0.1,W=1/156,type="low")  #second order 13 year low pass filter with 0.1 dB of band pass ripple

#********************
#    El Nino Indices
#*******************
#Start with reading in the ElninoERSSTv4 data
FileName <- "ElNinoERSSTv4_011950-012016.txt"
ElNinoERSSTv4 <- read.table(file.path(DataDirectory,FileName),header = TRUE, sep = "")
#Build a column of dates
Dates <- as.yearmon(paste(ElNinoERSSTv4$YR,ElNinoERSSTv4$MON,sep="-"))
#Create the Zoo series
ElNinoERSSTv4Indices <- zoo(ElNinoERSSTv4[,c("NINO1.2","NINO3","NINO4","NINO3.4")],Dates)

#*******************
# MEI
#******************
#Read in the MEI data
FileName <- "MEI011950-012016.txt"
MEI_wide <- read.table(file.path(DataDirectory,FileName),header = TRUE, fill=T)
#Convert from short form to long
MEI_long <- melt(MEI_wide, id.vars="YEAR")
#Create two month name columns from the "variable" column
MEI_long$EndMonth <- substr(MEI_long$variable,4,6)

#Build a column of dates
Dates <- as.yearmon(paste(MEI_long$YEAR,MEI_long$EndMonth,sep="-"),format="%Y-%b")

#Create the Zoo series
MEI_Indices <- zoo(MEI_long[,c("value"),drop=FALSE],Dates)
colnames(MEI_Indices) <- "MEI"

#Create the early series (lagged back by 1)
MEI_early <- lag(MEI_Indices,1)
colnames(MEI_early) <- "MEIearly"
MEI_Indices <- merge(MEI_Indices,MEI_early)

#*******************
# MEI Extended
#******************
#Read in the MEIExt data
FileName <- "MEIExt011871-122005.txt"
MEI_wide <- read.table(file.path(DataDirectory,FileName),header = TRUE, fill=T)
#Convert from short form to long
MEI_long <- melt(MEI_wide, id.vars="YEAR")
#Create two month name columns from the "variable" column
MEI_long$EndMonth <- substr(MEI_long$variable,4,6)

#Build a columns of dates
Dates <- as.yearmon(paste(MEI_long$YEAR,MEI_long$EndMonth,sep="-"),format="%Y-%b")

#Create the Zoo series
MEIExt_Indices <- zoo(MEI_long[,c("value"),drop=FALSE],Dates)
colnames(MEIExt_Indices) <- "MEIExt"

#Create the early series (lagged back by 1)
MEIExt_early <- lag(MEIExt_Indices,1)
colnames(MEIExt_early) <- "MEIExtearly"
MEIExt_Indices <- merge(MEIExt_Indices,MEIExt_early)

#*******************
# Nino3.4Hadley
#******************
#Read in the Nino3.4 Hadley data
FileName <- "Nino3-4HadlSST011870-022020.txt"
NinoHad_wide <- read.table(file.path(DataDirectory,FileName),header = TRUE, fill=T)
#Convert from short form to long
NinoHad_long <- melt(NinoHad_wide, id.vars="Year")

#Build a column of dates
Dates <- as.yearmon(paste(NinoHad_long$Year,NinoHad_long$variable,sep="-"),format="%Y-%b")

#Create the Zoo series
NinoHad_Indices <- zoo(NinoHad_long[,c("value"),drop=FALSE],Dates)
colnames(NinoHad_Indices) <- "Nino3.4Had"

#*******************
# ONI
#******************
#Read in the ONI data
FileName <- "ONI011950-122015.txt"
ONI_wide <- read.table(file.path(DataDirectory,FileName),header = TRUE, fill=T)
#Convert from short form to long
ONI_long <- melt(ONI_wide, id.vars="Year")

#Build a column of dates
Dates <- as.yearmon(paste(ONI_long$Year,ONI_long$variable,sep="-"),format="%Y-%b")

#Create the Zoo series
ONI_Indices <- zoo(ONI_long[,c("value"),drop=FALSE],Dates)
colnames(ONI_Indices) <- "ONI"

#*******************
# TNI
#******************
#Read in the TNI data
FileName <- "TNI011948-012016.txt"
TNI_wide <- read.table(file.path(DataDirectory,FileName),header = TRUE, fill=T, na.strings = "-99.990")
#Convert from short form to long
TNI_long <- melt(TNI_wide, id.vars="Year")

#Build a column of dates
Dates <- as.yearmon(paste(TNI_long$Year,TNI_long$variable,sep="-"),format="%Y-%b")

#Create the Zoo series
TNI_Indices <- zoo(TNI_long[,c("value"),drop=FALSE],Dates)
colnames(TNI_Indices) <- "TNI"

#*******************
# SOI
#******************
#Read in the SOI data
FileName <- "SOI-BOM011876-042020.txt"
SOI_wide <- read.table(file.path(DataDirectory,FileName),header = TRUE, fill=T, stringsAsFactors = FALSE, colClasses= "numeric",na.strings ="-")

#Convert from short form to long
SOI_long <- melt(SOI_wide, id.vars="Year")

#Build a column of dates
Dates <- as.yearmon(paste(SOI_long$Year,SOI_long$variable,sep="-"),format="%Y-%b")

#Create the Zoo series
SOI_Indices <- zoo(SOI_long[,c("value"),drop=FALSE],Dates)
colnames(SOI_Indices) <- "SOI"
#**********************
# CEI
#*********************
#Calculate CEI
#CEI = centred 11 month running mean of SOI / 10 - centred 5 month running mean of Nino3.4 anomaly
CEI <- merge(SOI_Indices,NinoHad_Indices)
months <- format(index(CEI),"%m")
#Need to calculate anomaly of Nino
#Get the 1981 to 2010 data
#Get the month average for that period
#Calculate the difference from month average

#Anomaly <- Nino3.4Had - ave(replace(Nino3.4Had, index(Year
ReferencePeriodIndices <- index(CEI) < as.yearmon("1950-01-01") | index(CEI) > as.yearmon("1979-12-31")
ReferencePeriodOnly <- replace(CEI$Nino3.4Had,ReferencePeriodIndices,NA)
anomalyNino <- CEI$Nino3.4Had - ave(ReferencePeriodOnly,months, FUN=function(x) mean(x, na.rm=TRUE))
filter11 <- rep(1/11,11)
filter5 <- rep(1/5,5)
SOI_11Month <- stats::filter(CEI$SOI,filter11, sides=2)
Nino_5Month <- stats::filter(anomalyNino,filter5, sides=2)
CEI_Indices <- zoo((SOI_11Month / 10 - Nino_5Month),order.by = index(anomalyNino))

#colnames(CEI_Indices) <- "CEI"
Nino34_5 <- zoo(Nino_5Month,order.by = index(anomalyNino))
SOI11 <- zoo(SOI_11Month,order.by = index(anomalyNino))
plot(CEI_Indices,ylim=c(-5,5),ylab="Coupled ENSO index",xlab="Year",yaxt="n")
axis(2,at=seq(-5,5,1))
abline(0,0)
abline(-3,0,lty=4)
abline(3,0,lty=4)


#*******************
# IPO
#******************
#Read in the IPO data
FileName <- "IPO011857-122018.txt"
IPO_wide <- read.table(file.path(DataDirectory,FileName),header = TRUE, fill=T)

#Convert from short form to long
IPO_long <- melt(IPO_wide, id.vars="Year")

#Build a column of dates
Dates <- as.yearmon(paste(IPO_long$Year,IPO_long$variable,sep="-"),format="%Y-%b")

#Create the Zoo series
IPO_Indices <- zoo(IPO_long[,c("value"),drop=FALSE],Dates)
colnames(IPO_Indices) <- "IPO"

IPONoNA <- IPO_Indices[!is.na(IPO_Indices)]
IPO_13yr <- filtfilt(cf,IPONoNA)
IPO13_Indices <- zoo(IPO_13yr,index(IPONoNA))
#plot(IPO13_Indices)

#*******************
# PDO
#******************
#Read in the PDO data
FileName <- "PDO011900-012019.txt"
PDO_wide <- read.table(file.path(DataDirectory,FileName),header = TRUE, fill=T)

#Convert from short form to long
PDO_long <- melt(PDO_wide, id.vars="YEAR")

#Build a column of dates
Dates <- as.yearmon(paste(PDO_long$YEAR,PDO_long$variable,sep="-"),format="%Y-%b")

#Create the Zoo series
PDO_Indices <- zoo(PDO_long[,c("value"),drop=FALSE],Dates)
colnames(PDO_Indices) <- "PDO"

PDONoNA <- PDO_Indices[!is.na(PDO_Indices)]
PDO_13yr <- filtfilt(cf,PDONoNA)
PDO13_Indices <- zoo(PDO_13yr,index(PDONoNA))
#plot(PDO13_Indices)

#*******************
# TPI(IPO)
#******************
#Read in the TPI data
FileName <- "TPI011861-122016.txt"
TPI_wide <- read.table(file.path(DataDirectory,FileName),header = TRUE, fill=T)

#Convert from short form to long
TPI_long <- melt(TPI_wide, id.vars="Year")

#Build a column of dates
Dates <- as.yearmon(paste(TPI_long$Year,TPI_long$variable,sep="-"),format="%Y-%b")

#Create the Zoo series
TPI_Indices <- zoo(TPI_long[,c("value"),drop=FALSE],Dates)
colnames(TPI_Indices) <- "TPI"

TPINoNA <- TPI_Indices[!is.na(TPI_Indices)]
TPI_13yr <- filtfilt(cf,TPINoNA)
TPI13_Indices <- zoo(TPI_13yr,index(TPINoNA))
#plot(TPI13_Indices)

#*******************
# SAM
#******************
#Read in the SAM data
FileName <- "SAM011967-042020.txt"
SAM_wide <- read.table(file.path(DataDirectory,FileName),header = TRUE, fill=T,na.strings = "-999.9")

#Convert from short form to long
SAM_long <- melt(SAM_wide, id.vars="Year")

#Build a column of dates
Dates <- as.yearmon(paste(SAM_long$Year,SAM_long$variable,sep="-"),format="%Y-%b")

#Create the Zoo series
SAM_Indices <- zoo(SAM_long[,c("value"),drop=FALSE],Dates)
colnames(SAM_Indices) <- "SAM"


#********************************
# Put them altogether into a single dataframe
#*********************************
Indices <- merge(ElNinoERSSTv4Indices,NinoHad_Indices,TNI_Indices,ONI_Indices,SOI_Indices,MEI_Indices,MEIExt_Indices,CEI_Indices,IPO_Indices,TPI_Indices,PDO_Indices,IPO13_Indices,TPI13_Indices,PDO13_Indices,SAM_Indices)
colnames(Indices)<- c("NINO1.2","NINO3","NINO4","NINO3.4","Nino3.4Had","TNI","ONI","SOI","MEI","MEIearly","MEIxt","MEIxtearly","CEI","IPO","TPI","PDO","IPO13","TPI13","PDO13","SAM")
save(Indices,file=file.path(DataDirectory,"ClimateIndices.RData"))
plot(Indices)