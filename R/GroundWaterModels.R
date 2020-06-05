#Calibration
#This script is intended to be used to figure out how to calibrate the Vadose and bouss.eigen functions to an observed data set.
#**********************
#  Yet to be done
# Multi objective calibrations
# Including multiple observation data sets
#****************

library(lhs)  #attach the latin hypercube sampling library
library(hydroGOF) #attach the hydrological goodness of fit library
library(pbapply)
library(OpenRepGrid)
library(zoo)
library(GDATools)
library(hydromad) #Note that this is available from github https://github.com/josephguillaume/hydromad 
library(stringi)

#Site specific data
#Read in observed data
#Observeddata <- read.csv(system.file("extdata","Observeddata.csv",package="GDATools"))
Observeddata <- read.csv(system.file("extdata","Observeddata.csv",package="GDATools"))
Observedzoo <- read.zoo(Observeddata,format="%d/%m/%Y")$PaynesFord
#Observeddata <- read.csv("G:\\ARL Projects\\Other\\C15066_WaterWheel2\\Modeling\\Groundwater\\R eigenmodels\\Toenepi\\ToenepiObserved.csv")
#Observedzoo <- read.zoo(Observeddata,format="%d/%m/%Y",drop=FALSE)$Q
#Storativity=0.001
#ZoneLengths=c(0,23100,19800,23100)
#Storativity <- 0.005                           #this is a ratio so nondimensional
Storativity=0.016
ZoneLengths=c(10000,16000,18000)
#ZoneLengths <- c(0,0,0,6660)       #metres
WellDistance <- 38108.06                         #metres
AquiferLength <- sum(ZoneLengths)             #metres
#AquiferZoneDryFractions = list(c(0,0,0,0),c(0,0,0,0),c(0,0,0.033,0.8263))
#AquiferZoneIrrigFractions=list(c(0,0,0,0),c(0,0,0,0),c(0,0.0602,0.0069,0.1737))
#RiverRechargeFractions=c(0,1,0.958,1)
#AquiferZoneDryFractions = list(c(0,0,0,1))
#AquiferZoneIrrigFractions=list(c(0,0,0,0))
#RiverRechargeFractions=c(0,0,0,0)
AquiferZoneDryFractions = list(c(0.66,0.72,1))
AquiferZoneIrrigFractions=list(c(0.34,0.28,0))
RiverRechargeFractions=c(0,0,0)


#PaynesFordRecharge<-vadose.recharge(ZoneStorageTime=c(0,0,0,0),AquiferZoneDryFractions = list(c(0,0,0,0),c(0,0,0,0),c(0,0,0.033,0.8263)),AquiferZoneIrrigFractions=list(c(0,0,0,0),c(0,0,0,0),c(0,0.0602,0.0069,0.1737)),RiverRechargeFractions=c(0,1,0.958,1),RechargeFileName=RechargeFileName,PumpingFileName=PumpingFileName)
#PaynesForddischarge <- bouss.eigen(WellDistance=66000,ZoneLengths=c(0,23100,19800,23100),Storativity=0.001,Transmisivity=653400,DischargeScaleFactor=1450,RechargeData=PaynesFordRecharge,GWBypassFlow=2.45)

#I need to calibrate for rsquared first to determine the transmisivity value (using some default values for DischargeScale factor and GWbypassFlow)

#The following function takes in five arguments x[1;5] and returns an objective function for the use of that value as the transmissivity in the underlying boussinesq model, and the vadose travel times in the underlying vadose model

DefaultDSF <- 1000
DefaultGWBP <- 0

boussR2 <- function(x,DSF=DefaultDSF,GWBP=-DefaultGWBP,Observed){
  #Recharge<-vadose.recharge(ZoneStorageTime=c(0,0,0,x[2]),
  Recharge<-vadose.recharge(ZoneStorageTime=c(x[2],x[3],x[4],x[5]),
                            AquiferZoneDryFractions = AquiferZoneDryFractions,
                            AquiferZoneIrrigFractions=AquiferZoneIrrigFractions,
                            RiverRechargeFractions=RiverRechargeFractions,
                            #RechargeFileName="G:\\ARL Projects\\Other\\C15066_WaterWheel2\\Modeling\\Groundwater\\R eigenmodels\\Toenepi\\ToenepiRechargeV2.csv",
                            #PumpingFileName="G:\\ARL Projects\\Other\\C15066_WaterWheel2\\Modeling\\Groundwater\\R eigenmodels\\Toenepi\\ToenepiGWPumping.csv")
                            RechargeFileName=system.file("extdata","GoldenBayLandSurfaceRechargeData.csv",package="GDATools"),
                            PumpingFileName=system.file("extdata","GoldenBayGWPumpingData.csv",package="GDATools"))
  Discharge <- bouss.eigen(WellDistance=6660,ZoneLengths=ZoneLengths,Storativity=Storativity,Transmisivity=x[1],DischargeScaleFactor=DSF,RechargeData=Recharge,GWBypassFlow=GWBP)
  ObjectiveFunction <- gof(Discharge[,4],Observed)["R2",]
  return(ObjectiveFunction)
}

boussME <- function(x,timeSeries,Observed){
  OffsetTimeSeries <- timeSeries + x
  ObjectiveFunction <- abs(gof(OffsetTimeSeries[,4],Observed)["ME",])
  return(ObjectiveFunction)
}

boussNSE <- function(x,timeSeries,Observed){
  scaledTimeSeries <- timeSeries * x
  ObjectiveFunction <- gof(scaledTimeSeries[,4],Observed)["NSE",]
  return(ObjectiveFunction)
}

#Find the transmisivity value that maximises R squared
#OptimisationResults <- optimise(boussR2,c(0,10000000),Observed=Observedzoo,maximum=TRUE)
#OptimisationResults <- optim(par=c(20000,1,1,1,1),fn=boussR2,Observed=Observedzoo,method= "L-BFGS-B",lower=c(0,0,0,0,0),upper=c(10000000,5,5,5,5),control=list(fnscale=-1))
#OptimisationResults <- optim(par=c(200000,1,1,1,1),fn=boussR2,Observed=Observedzoo,method= "Nelder-Mead",control=list(fnscale=-1,parscale=c(1000,0.1,0.1,0.1,0.1)))
OptimisationResults <- SCEoptim(FUN=boussR2,par=c(20000,1,1,1,1),Observed=Observedzoo,lower=c(0,0,0,0,0),upper=c(10000000,5,5,5,5),control=list(fnscale=-1))
#OptimisationResults <- SCEoptim(FUN=boussR2,par=c(20000,1),Observed=Observedzoo,lower=c(0,0),upper=c(10000000,5),control=list(fnscale=-1))


#Generate a discharge series with the optimised transmisivity
#Discharge <- bouss.eigen(WellDistance=WellDistance,ZoneLengths=ZoneLengths,Storativity=Storativity,Transmisivity=OptimisationResults$maximum,DischargeScaleFactor=1000,RechargeData=Recharge,GWBypassFlow=0)
#Discharge <- bouss.eigen(WellDistance=WellDistance,ZoneLengths=ZoneLengths,Storativity=Storativity,Transmisivity=OptimisationResults$par,DischargeScaleFactor=1000,RechargeData=Recharge,GWBypassFlow=0)
Recharge<-vadose.recharge(ZoneStorageTime=c(OptimisationResults$par[2],OptimisationResults$par[3],OptimisationResults$par[4],OptimisationResults$par[5]),
#Recharge<-vadose.recharge(ZoneStorageTime=c(0,0,0,OptimisationResults$par[2]),
                                                    
                          AquiferZoneDryFractions = AquiferZoneDryFractions,
                          AquiferZoneIrrigFractions=AquiferZoneIrrigFractions,
                          RiverRechargeFractions=RiverRechargeFractions,
                          #RechargeFileName="G:\\ARL Projects\\Other\\C15066_WaterWheel2\\Modeling\\Groundwater\\R eigenmodels\\Toenepi\\ToenepiRechargeV2.csv",
                          #PumpingFileName="G:\\ARL Projects\\Other\\C15066_WaterWheel2\\Modeling\\Groundwater\\R eigenmodels\\Toenepi\\ToenepiGWPumping.csv")
                          RechargeFileName=system.file("extdata","GoldenBayLandSurfaceRechargeData.csv",package="GDATools"),
                          PumpingFileName=system.file("extdata","GoldenBayGWPumpingData.csv",package="GDATools"))

Discharge <- bouss.eigen(WellDistance=WellDistance,ZoneLengths=ZoneLengths,Storativity=Storativity,Transmisivity=OptimisationResults$par[1],DischargeScaleFactor=DefaultDSF,RechargeData=Recharge,GWBypassFlow=-DefaultGWBP)


#Find the GroundWater ByPass quantity (effectively the offset) that minimises the absolute value of the mean error
OptimisationResults2 <- optimise(boussME,c(0,10),timeSeries=Discharge,Observed=Observedzoo,maximum=FALSE)

#Generate a discharge series with the optimised transmisivity and ground water bypass
#Discharge <- bouss.eigen(WellDistance=WellDistance,ZoneLengths=ZoneLengths,Storativity=Storativity,Transmisivity=OptimisationResults$maximum,DischargeScaleFactor=1000,RechargeData=Recharge,GWBypassFlow=-OptimisationResults2$minimum)
Discharge <- bouss.eigen(WellDistance=WellDistance,ZoneLengths=ZoneLengths,Storativity=Storativity,Transmisivity=OptimisationResults$par[1],DischargeScaleFactor=1000,RechargeData=Recharge,GWBypassFlow=-OptimisationResults2$minimum)


#Find the DischargeScaleFactor that maximises the Nash Sutcliffe criteria (NSE)
OptimisationResults3 <- optimise(boussNSE,c(0,10),timeSeries=Discharge,Observed=Observedzoo,maximum=TRUE)

#Generate a discharge series with the optimised transmisivity and ground water bypass and DischargeScaleFactor
#Discharge <- bouss.eigen(WellDistance=WellDistance,ZoneLengths=ZoneLengths,Storativity=Storativity,Transmisivity=OptimisationResults$maximum,DischargeScaleFactor=1000*OptimisationResults3$maximum,RechargeData=Recharge,GWBypassFlow=-OptimisationResults2$minimum)
Discharge <- bouss.eigen(WellDistance=WellDistance,ZoneLengths=ZoneLengths,Storativity=Storativity,Transmisivity=OptimisationResults$par[1],DischargeScaleFactor=1000*OptimisationResults3$maximum,RechargeData=Recharge,GWBypassFlow=-OptimisationResults2$minimum)


#and plot it
bouss.plot(Discharge[,4],Observedzoo)

#and print the optimised values
print(noquote("Optimised parameters"))
print(noquote("********************"))
print(noquote(paste0("Given a storativity of ",Storativity, " and aquifer length of ",AquiferLength," m")))
print(noquote(paste0("The optimal Transmissivity is:")))
print(noquote(paste0(OptimisationResults$par[1], "m2/day")))
print(noquote(paste0("The optimal Ground Water Bypass quantity is:")))
print(noquote(paste0(OptimisationResults2$minimum, " m2/day")))
print(noquote("The optimal Discharge Scale Factor is:"))
print(noquote(1000*OptimisationResults3$maximum))
print(noquote("The optimal vadose transit times for zones 1 to 4 are:"))
#print(noquote(paste0(format(OptimisationResults$par[2:5], digits=2)," days")))      
print(noquote(paste0(format(OptimisationResults$par[2], digits=2)," days")))   

