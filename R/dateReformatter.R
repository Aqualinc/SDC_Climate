#Script to sort out the date formats after Excel has messed with them
#Tim Kerr
#August 2016



multidate <- function(data, formats){
  a<-list()
  for(i in 1:length(formats)){
    a[[i]]<- as.Date(data,format=formats[i])
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}



dataDirectory <- "G:\\ARL Projects\\Other\\C16049_SDC climate impact\\Data\\StationData"
parameter          <- "Rainfall" # MinT"
#for (SiteOfInterest in c(39066,25821,4882,4881,4843,4836,4764,4698,4513)) {    #for the MinT sites
  for (SiteOfInterest in c(4513,4591,4663,4669,4670,4674,4698,4701,4720,4722,4764,4834,4836,4843,4851,4881,4917,4935,25821,36645,39964)) {    #for the Rainfall sites
#SiteOfInterest     <- 4698
SiteOfInterestFileName  <- file.path(dataDirectory,parameter,paste(SiteOfInterest,"extended.csv"))
SiteData                <- read.csv(SiteOfInterestFileName,colClasses = c("character","numeric"))
SiteData$Date         <- multidate(SiteData$Date, c("%Y-%m-%d","%d/%m/%Y"))  #need this cause the dates have mixed formats
write.csv(SiteData,file=file.path(dataDirectory,parameter,paste0(SiteOfInterest," extended.csv")),quote=FALSE,
          row.names=FALSE)
}