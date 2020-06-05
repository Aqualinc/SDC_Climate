#Script to present climate indices
#Tim Kerr
#April 2016
#Aqualinc Research Ltd
#Prepared as part of project C16049 Impact of climate cycles and trends on Selwyn District water assets

#Load libraries
if (!require(zoo)) install.packages('zoo'); library(zoo)
if (!require(reshape2)) install.packages('reshape2'); library(reshape2)
if (!require(lattice)) install.packages('lattice'); library(lattice)
if (!require(grid)) install.packages('grid'); library(grid)
if (!require(extrafont)) install.packages('extrafont'); library(extrafont)
if (!require(xts)) install.packages('xts'); library(xts)
if (!require(Cairo)) install.packages('Cairo'); library(Cairo)

#Useful functions
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y,use="complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

#Set directories
#DataDirectory <- "\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Data"
DataDirectory <- "D:\\Projects\\Aqualinc\\projects\\SDC 2020\\Data"        #Rainfall.NZ server
#ReportDirectory <- ("\\\\aqualinc-sbs\\data\\ARL Projects\\Other\\C16049_SDC climate impact\\Reports\\Climate")
ReportDirectory <- "D:\\Projects\\Aqualinc\\projects\\SDC 2020\\Reports"

#Get the CLimate indices data
load(file=file.path(DataDirectory,"ClimateIndices","ClimateIndices.RData"))


#********************************************************
#Plot the various ENSO indices against each other
#******************************************************
png(file=file.path(ReportDirectory,"Index-IndexScatter.png"),width=15.5,height=15.5,units="cm",res=600,family="Arial",pointsize=10)
IndexPlot <- pairs(Indices[,c(1,2,5,3,6,7,8,13,9)], upper.panel = panel.cor)
print(IndexPlot)
dev.off()

#********************************************************
#Plot the CEI index
#******************************************************
{
svg(file=file.path(ReportDirectory,"CEI-TimeSeries.svg"),width=6,height=2,family="Arial",pointsize=10)
#png(file=file.path(ReportDirectory,"CEI-TimeSeries.png"),width=15,height=5,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
par(mar=c(2,4,1,0),cex=1)
IPOTimeSeriesPlot <- plot(Indices[,13],plot.type="single",
                          lty=c(1,2,3),xlab="",ylab="Index value",col=rgb(49,132,155,255,maxColorValue=255),lwd=2,
                          xlim=c(1910,2020),xaxt="n")
axis(1,at=seq(from=1910,to=2020,by=10))
par(oldpar)
dev.off()
}
#********************************************************
#Plot the various IPO indices against each other
#******************************************************
#png(file=file.path(ReportDirectory,"IPO-IPOScatter.png"),width=7.5,height=7.5,units="cm",res=600,family="Arial",pointsize=10)
#IndexPlot <- pairs(Indices[,c(14,15,16)], upper.panel = panel.cor)
#print(IndexPlot)
#dev.off()

#********************************************************
#Plot the various IPO time series
#******************************************************
{
svg(file=file.path(ReportDirectory,"IPO-TimeSeries.svg"),width=6,height=2,family="Arial",pointsize=10)
#png(file=file.path(ReportDirectory,"IPO-TimeSeries.png"),width=15,height=5,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
par(mar=c(2,4,1,0),cex=1)
IPOTimeSeriesPlot <- plot(Indices[,c(17,18,19)],plot.type="single",
                          lty=c(1,2,3),xlab="",ylab="Index value",col=rgb(49,132,155,255,maxColorValue=255),lwd=2,
                          ylim=c(-2.5,2.5),xlim=c(1910,2020),xaxt="n")
axis(1,at=seq(from=1910,to=2020,by=10))
abline(0,0)
legend("topright",bty="n",legend=c("IPO","TPI","PDO"),lty=c(1,2,3),col=rgb(49,132,155,255,maxColorValue=255),lwd=2)
par(oldpar)
dev.off()
}
#********************************************************
#Plot the SAM
#******************************************************
png(file=file.path(ReportDirectory,"SAM-TimeSeries.png"),width=15,height=5,units="cm",res=600,family="Arial",pointsize=10)
oldpar <- par()
par(mar=c(2,4,1,0),cex=1)
SAMTimeSeriesPlot <- plot(Indices[,20],plot.type="single",
                          xlab="",ylab="Index value",col=rgb(49,132,155,255,maxColorValue=255),lwd=2,xlim=c(1956,2020),xaxt="n")
axis(1,at=seq(from=1960,to=2020,by=10))
par(oldpar)
dev.off()
