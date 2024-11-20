## ----echo=FALSE, include=FALSE------------------------------------------------
knitr::opts_chunk$set(collapse = F, dpi=60, fig.retina = 1)
options('rgdal_show_exportToProj4_warnings'='none')

## ----echo=F,message=F---------------------------------------------------------
library(move)

## ----eval=FALSE---------------------------------------------------------------
#  library(move)
#  myMoveObject <- move(x="C:/User/Documents/GPS_data.csv")
#  myMoveEnv <- move(x="C:/User/Documents/GPS_data_Annotated.zip")
#  

## ----eval=F-------------------------------------------------------------------
#  data <- read.csv(system.file("extdata","leroy.csv.gz",package="move"))
#  leroy <- move(x=data$location.long, y=data$location.lat,
#                time=as.POSIXct(data$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#                proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
#                data=data, animal=data$individual.local.identifier, sensor=data$sensor)

## ----eval=T-------------------------------------------------------------------
leroy

## ----message=F, warning=F-----------------------------------------------------
ricky<-move(system.file("extdata","ricky.csv.gz", package="move"))
data(leroy)
## if argument forceTz is not stated, the timestamp is converted to the computer timezone
leroyP<-spTransform(leroy, proj4string(ricky))
myStack <- moveStack(list(leroyP, ricky),forceTz="UTC")

## ----eval=T-------------------------------------------------------------------
myStack

## ----eval=F-------------------------------------------------------------------
#  unstacked <- split(myStack)
#  myStack <- moveStack(unstacked, forceTz="UTC")

## ----eval=F-------------------------------------------------------------------
#  str(leroy)

## -----------------------------------------------------------------------------
timestamps(leroy)[1:3]

## For a MoveStack the output is a continuous vector of timestamps of all individuals:
timestamps(myStack)[1:3]

## -----------------------------------------------------------------------------
coordinates(leroy)[1:3,]

## For a MoveStack the output is one matrix containing the coordinates of all individuals:
coordinates(myStack)[1:3,]

## -----------------------------------------------------------------------------
projection(leroy)

## -----------------------------------------------------------------------------
extent(leroy)
bbox(leroy)

## -----------------------------------------------------------------------------
idData(leroy)
idData(myStack)

## -----------------------------------------------------------------------------
leroy@data[1:3,]

## -----------------------------------------------------------------------------
namesIndiv(leroy)
namesIndiv(myStack)

## -----------------------------------------------------------------------------
n.locs(leroy)
n.locs(myStack)

## -----------------------------------------------------------------------------
n.indiv(leroy)
n.indiv(myStack)

## -----------------------------------------------------------------------------
levels(sensor(leroy))

## -----------------------------------------------------------------------------
## all information
as.data.frame(unUsedRecords(leroy))[1:3,]

## only the timestamps of the unused records:
leroy@timestampsUnUsedRecords[1:3]

## only the data of the unused records:
leroy@dataUnUsedRecords[1:3,]

## only the sensor of the unused records:
levels(leroy@sensorUnUsedRecords)


## ----eval=F-------------------------------------------------------------------
#  coatis_bci <- getMovebankData(study="Coatis on BCI Panama (data from Powell et al. 2017)")
#  
#  ## the study name:
#  coatis_bci@study
#  # [1] "Coatis on BCI Panama (data from Powell et al. 2017)"
#  
#  ## how to cite the study:
#  citations(coatis_bci)
#  # [1] "Powell RA, Ellwood S, Kays R, Maran T (2017) Stink or swim: techniques to meet the challenges for the study and conservation of small critters that hide, swim, or climb, and may otherwise make themselves unpleasant. In Macdonald DW, Newman C, Harrington LA, eds, Biology and Conservation of Musteloids. Oxford University Press, Oxford. p 216–230. doi:10.1093/oso/9780198759805.003.0008"
#  
#  ## license terms of the study
#  licenseTerms(coatis_bci)
#  # [1] "These data have been published by the Movebank Data Repository with DOI "http://dx.doi.org/10.5441/001/1.41076dq1"

## ----eval=T-------------------------------------------------------------------
trackId(myStack)[1:3]

## ----eval=F-------------------------------------------------------------------
#  rickyT <- myStack[["Ricky.T"]]

## ----eval=F-------------------------------------------------------------------
#  indv1 <- myStack[[1]]

## ----eval=F-------------------------------------------------------------------
#  leroyAndRicky <- myStack[[c("Leroy","Ricky.T")]]

## ----eval=F-------------------------------------------------------------------
#  twoInd <- myStack[[c(1,2)]]

## ----eval=F-------------------------------------------------------------------
#  noRickyT <- myStack[[-which(namesIndiv(myStack)=="Ricky.T")]]

## ----eval=F-------------------------------------------------------------------
#  noIndv1 <- myStack[[-1]]

## ----eval=F-------------------------------------------------------------------
#  noLeroyAndRicky <- myStack[[-which(namesIndiv(mv)%in%c("Leroy","Ricky.T"))]]

## ----eval=F-------------------------------------------------------------------
#  noOneAndTwo <- myStack[[-c(1,2)]]

## ----eval=F-------------------------------------------------------------------
#  ## subset to locations 1-50
#  leroySub <- leroy[1:50]

## ----eval=F-------------------------------------------------------------------
#  ## select the locations 1-50 from a movestack. WARNING: this will just select the 50 first locations in order of occurrence, in this case they correspond to the first individual of the movestack
#  myStackSub <- myStack[1:50]
#  
#  ## to select locations 1-50 from each individual
#  myStackSubs <- moveStack(lapply(split(myStack), function(x){x[1:50]}))

## ----eval=F-------------------------------------------------------------------
#  ## selecting a specific day
#  leroyOneDay <- leroy[as.Date(timestamps(leroy))==as.Date("2009-02-25")]
#  ## selecting a range of days
#  leroy3Days <- leroy[as.Date(timestamps(leroy))%in%c(as.Date("2009-02-25"):as.Date("2009-02-28"))]
#  ## selecting a specific month
#  myStackMarch <- myStack[month(timestamps(myStack))==3]

## ----message=FALSE------------------------------------------------------------
library(lubridate)
leroy@timestamps[1]
leroyLocal <- leroy
timestamps(leroyLocal) <- with_tz(timestamps(leroyLocal), tz="America/New_York")
leroyLocal@timestamps[1]

## -----------------------------------------------------------------------------
projection(leroy)
leroyProj <- spTransform(leroy, CRSobj="+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
projection(leroyProj)

## ----fig.width=5, fig.height=5------------------------------------------------
plot(leroy, xlab="Longitude", ylab="Latitude", type="l", pch=16, lwd=0.5)
points(leroy, pch=20, cex=0.5)
plot(myStack, xlab="Longitude", ylab="Latitude",type="b", pch=16, cex=0.5, col=c("blue","magenta")[myStack@trackId])

## ----fig.width=5, fig.height=4, warning=F,message=F---------------------------
library(ggplot2)
myStackDF <- as.data.frame(myStack)
ggplot(data = myStackDF, aes(x = location.long, y = location.lat, color = trackId)) + 
  geom_path() + geom_point(size = 0.5) + theme_bw() + coord_cartesian()

## ----fig.width=5, fig.height=5, eval=F----------------------------------------
#  library(ggmap)
#  require(mapproj)
#  leroyDF <- as.data.frame(leroy)
#  register_stadiamaps(your_stadia_key)
#  m <- get_map(bbox(extent(leroy)*1.1),maptype="stamen_terrain", source="stadia", zoom=12)
#  ggmap(m)+geom_path(data=leroyDF, aes(x=location.long, y=location.lat))

## ----fig.width=5, fig.height=5, echo=F, message=F-----------------------------
library(ggmap)
require(mapproj)
leroyDF <- as.data.frame(leroy)
load("a.Rdata")
ggmap(m)+geom_path(data=leroyDF, aes(x=location.long, y=location.lat))

## -----------------------------------------------------------------------------
## from a move object (a vector is returned)
timeLag(leroy, units="mins")[1:5]

## from a moveStack object (a list of vectors is returned)
str(timeLag(myStack, units="mins"))

## -----------------------------------------------------------------------------
## from a move object (a vector is returned)
distance(leroy)[1:5]

## from a moveStack object (a list of vectors is returned)
str(distance(myStack))

## -----------------------------------------------------------------------------
## from a move object (a vector is returned)
speed(leroy)[1:5]

## from a moveStack object (a list of vectors is returned)
str(speed(myStack))

## -----------------------------------------------------------------------------
## from a move object (a vector is returned)
angle(leroy)[1:5]

## from a moveStack object (a list of vectors is returned)
str(angle(myStack))

## -----------------------------------------------------------------------------
## from a move object (a vector is returned)
turnAngleGc(leroy)[1:5]

## from a moveStack object (a list of vectors is returned)
str(turnAngleGc(myStack))

## ----message=F----------------------------------------------------------------
library(solartime)
elev<-computeSunPosition(timestamps(leroy), coordinates(leroy)[,2],coordinates(leroy)[,1])[,'elevation']
DayNight <- c("Night",'Day')[1+(elev>0)]
## assigning to each segment if it is during daytime or night
leroy.burst <- burst(x=leroy, f=DayNight[-n.locs(leroy)])

## ----fig.width=5, fig.height=5------------------------------------------------
plot(leroy.burst, type="l", col=c("red", "black"), asp=1)
legend("bottomleft",legend=c("day","night"), col=c("red", "black"), lty=1)

## ----fig.width=5, fig.height=5, message=F, warning=F--------------------------
### in the default the size of the cicles is relative to the total time spent within each burst segment 
plotBursts(leroy.burst,breaks=5, col=c("red", "black"), pch=19, add=F,main="Size of points: total time spent in burst segment", asp=1)
legend("bottomleft",legend=c("day","night"), col=c("red", "black"), pch=19)

## here, e.g., the size of the circles is relative to the total distance moved within each burst segment
plotBursts(object=leroy.burst, breaks=5, sizeFUN=function(x){sum(distance(x))},col=c("red", "black"), pch=19, add=F,main="Size of points: total distance moved in burst segment", asp=1)
legend("bottomleft",legend=c("day","night"), col=c("red", "black"), pch=19)

## ----fig.width=10, fig.height=3-----------------------------------------------
leroyB.split <- split(leroy.burst)

par(mfrow=c(1,4))
plot(leroyB.split[[1]], type="b", pch=20, main=paste0(names(leroyB.split[1])," 1"))
plot(leroyB.split[[2]], type="b", pch=20, main=paste0(names(leroyB.split[2])," 1"))
plot(leroyB.split[[3]], type="b", pch=20, main=paste0(names(leroyB.split[3])," 2"))
plot(leroyB.split[[4]], type="b", pch=20, main=paste0(names(leroyB.split[4])," 2"))

## ----fig.width=5, fig.height=5------------------------------------------------
LeroyCorr <- corridor(leroy)
plot(LeroyCorr, type="l", xlab="Longitude", ylab="Latitude", col=c("black", "grey"), lwd=c(1,2))
legend("bottomleft", c("Corridor", "Non-corridor"), col=c("black", "grey"), lty=c(1,1), bty="n")

## ----message=F, warning=F-----------------------------------------------------
leroyRed <- leroy[1:200] # reducing dataset for example
leroy.prj <- spTransform(leroyRed, center=TRUE) # center=T: the center of the coordinate system is the center of the track. Units are in meters

dBB.leroy <- brownian.bridge.dyn(leroy.prj, ext=.85, raster=100, location.error=20)

## ----fig.width=5, fig.height=5, message=F-------------------------------------
plot(dBB.leroy, main="dBBMM")

## ----fig.width=10, fig.height=5-----------------------------------------------
UDleroy <- getVolumeUD(dBB.leroy)

par(mfrow=c(1,2))
plot(UDleroy, main="UD")

## also a contour can be added
plot(UDleroy, main="UD and contour lines")
contour(UDleroy, levels=c(0.5, 0.95), add=TRUE, lwd=c(0.5, 0.5), lty=c(2,1))

## ----fig.width=12, fig.height=4-----------------------------------------------
par(mfrow=c(1,3))

## mantaining the lower probabilities
ud95 <- UDleroy
ud95[ud95>.95] <- NA
plot(ud95, main="UD95")

## or extracting the area with a given probability, where cells that belong to the given probability will get the value 1 while the others get 0
ud95 <- UDleroy<=.95
plot(ud95, main="UD95")

ud50 <- UDleroy<=.5
plot(ud50, main="UD50")

## ----fig.width=12, fig.height=8, message=F, warning=F-------------------------
leroyBRed <- leroy.burst[1:155] # reducing dataset for example
leroyB.prj <- spTransform(leroyBRed, center=TRUE) # center=T: the center of the coordinate system is the center of the track. Units are in meters

dBB.leroyB <- brownian.bridge.dyn(leroyB.prj, ext=1.25, raster=100, location.error=20)

plot(dBB.leroyB)

## ----fig.width=12, fig.height=8, message=F------------------------------------
leroyBud <- UDStack(dBB.leroyB)
UDleroyB <- getVolumeUD(leroyBud)

plot(UDleroyB)

## ----fig.width=10, fig.height=5, fig.show='hold', message=F, warning=F--------
par(mfrow=c(1,2))
## select all layers corresponding to "Day", sum them up, and convert them to class .UD
daylayers <- grep("Day", names(dBB.leroyB))
rasterDay <- sum(dBB.leroyB[[daylayers]])
rasterDayUD <- as(rasterDay,".UD")
UDleroy.day <- getVolumeUD(rasterDayUD)
plot(UDleroy.day, main="UD of 'Day'")

## same for layer corresponding to "Night"
nightlayers <- grep("Night", names(dBB.leroyB))
rasterNight <- sum(dBB.leroyB[[nightlayers]])
rasterNightUD <- as(rasterNight,".UD")
UDleroy.night <- getVolumeUD(rasterNightUD)
plot(UDleroy.night, main="UD of 'Night'")

## ----fig.width=9, fig.height=3, message=F, warning=F--------------------------
dBB.leroyB.night <- brownian.bridge.dyn(leroyB.prj, ext=.75, raster=100, location.error=20, burstType="Night")

plot(dBB.leroyB.night,nr=1)

## -----------------------------------------------------------------------------
## check the timeLag of data. If there are timelags shorter than the intended scedule, use the argument "timestep" 
rickyRed <- ricky[1:100]
summary(timeLag(rickyRed,"mins")) 

## ----eval=F-------------------------------------------------------------------
#  ricky.prj <- spTransform(rickyRed, center=TRUE)
#  
#  ts <- median(timeLag(rickyRed,"mins"))
#  BB.ricky <- brownian.bridge.dyn(ricky.prj, ext=.45, dimSize=100, location.error=20,time.step=ts/15)

## ----error=T,fig.width=10, fig.height=5,message=F, warning=F------------------
## creating a gappy data set
leroyWithGap <- leroy[-c(50:500,550:850)]
leroyWithGap_p <- spTransform(leroyWithGap, center=TRUE)

## calculate the dBBMM with the default extent gives an error that it is too small
dbb <- brownian.bridge.dyn(leroyWithGap_p, raster=100, location.error=20)

## making the extent bigger seems to solve the problem
dbb <- brownian.bridge.dyn(leroyWithGap_p, raster=100, location.error=20, ext=4)

## but than the UD is not very informative
ud <- getVolumeUD(dbb)

par(mfrow=c(1,2))
plot(ud, main="UD")
contour(ud, levels=c(0.5, 0.95), add=TRUE, lwd=c(0.5, 0.5), lty=c(2,1))

plot(ud, main="UD with locations")
points(leroyWithGap_p, col="red",  cex=.5, pch=20)
contour(ud, levels=c(0.5, 0.95), add=TRUE, lwd=c(0.5, 0.5), lty=c(2,1))

## ----fig.width=10, fig.height=5, message=F, warning=F-------------------------
## calculate the dynamic brownian motion variance of the gappy track
dbbv <- brownian.motion.variance.dyn(leroyWithGap_p, location.error=20, window.size=31, margin=11)

## the intended GPS fix rate of leroy was 15min, so we will ignore for example all segments that have a larger time lag than 5hours. The 'dBMvariance' object resulting from the function above, contains the slot '@interest' in which those segments marked as FALSE won't be included in the calculation of the dBBMM. Therefore we set all segments with time lag larger than 300mins to false
dbbv@interest[timeLag(leroyWithGap_p,"mins")>300] <- FALSE

## then we use the 'dBMvariance' object to calculate the dBBMM
dbb.corrected <- brownian.bridge.dyn(dbbv, raster=100, ext=.45,location.error=20)

## now the UD makes more sense
ud.corrected <- getVolumeUD(dbb.corrected)

par(mfrow=c(1,2))
plot(ud.corrected, main="UD")
contour(ud.corrected, levels=c(0.5, 0.95), add=TRUE, lwd=c(0.5, 0.5), lty=c(2,1))

plot(ud.corrected, main="UD with locations")
points(leroyWithGap_p, col="red", cex=.5, pch=20)
contour(ud.corrected, levels=c(0.5, 0.95), add=TRUE, lwd=c(0.5, 0.5), lty=c(2,1))

## ----message=F, warning=F-----------------------------------------------------
## e.g. compare the utilization distributions of the day and night UDs of Leroy
dBB.leroyB <- brownian.bridge.dyn(leroyB.prj, ext=1.5, raster=500, location.error=20)
leroyBud <- UDStack(dBB.leroyB)
## to optimize the calculation, the cells outside of the 99.99% UD contour are removed by setting them to zero.
values(leroyBud)[values(getVolumeUD(leroyBud))>.999999]<-0
## the rasters have to be standardized so the pixels sum up to 1
leroyBud_2<-(leroyBud/cellStats(leroyBud,sum))
emd(leroyBud_2)
## the result is an matrix of distances of the class 'dist'

## ----fig.width=5,fig.height=5, fig.show='hold'--------------------------------
## providing the number of locations. In this case the interpolated trajectory will have 500 locations with a regular timelag
interp500p <- interpolateTime(leroyRed, time=500, spaceMethod='greatcircle')
plot(leroyRed, col="red",pch=20, main="By number of locations")
points(interp500p)
lines(leroyRed, col="red")
legend("bottomleft", c("True locations", "Interpolated locations"), col=c("red", "black"), pch=c(20,1))

summary(timeLag(interp500p, "mins"))

## ----fig.width=5,fig.height=5-------------------------------------------------
## providing a time interval. In this case the interpolated trajectory will have a location every 5mins
interp5min <- interpolateTime(leroyRed, time=as.difftime(5, units="mins"), spaceMethod='greatcircle')
plot(leroyRed, col="red",pch=20, main="By time interval")
points(interp5min)
lines(leroyRed, col="red")
legend("bottomleft", c("True locations", "Interpolated locations"), col=c("red", "black"), pch=c(20,1))

## ----fig.width=5,fig.height=5-------------------------------------------------
## providing a vector of timestamps
ts <- as.POSIXct(c("2009-02-12 06:00:00", "2009-02-12 12:00:00", "2009-02-12 23:00:00",
                   "2009-02-14 06:00:00", "2009-02-14 12:00:00", "2009-02-14 23:00:00"),
                 format="%Y-%m-%d %H:%M:%S", tz="UTC")

interpCusom <- interpolateTime(leroyRed, time=ts, spaceMethod='greatcircle')

plot(leroyRed, col="red",pch=20, main="By custom timestamps")
points(interpCusom)
lines(leroyRed, col="red")
legend("bottomleft", c("True locations", "Interpolated locations"), col=c("red", "black"), pch=c(20,1))

## -----------------------------------------------------------------------------
## selecting those segments that have a time interval of 45 mins plus/minus 5 mins. The original data have a fix every ~15mins.
thintime <- thinTrackTime(leroyRed, interval = as.difftime(45, units='mins'),
                          tolerance = as.difftime(5, units='mins'))

## -----------------------------------------------------------------------------
data.frame(TL=timeLag(thintime,"mins"),burst=thintime@burstId)[15:25,]

## ----eval=F-------------------------------------------------------------------
#  ## selecting those segments that have a travel distance of ~300m in the original track
#  thindist <- thinDistanceAlongTrack(leroyRed, interval = 300, tolerance = 10)

## ----eval=F-------------------------------------------------------------------
#  leroyDF <- as.data.frame(leroy)

## ----eval=F-------------------------------------------------------------------
#  leroySPDF <- as(leroy,"SpatialPointsDataFrame")

## ----eval=F-------------------------------------------------------------------
#  leroy.ltraj <- as(leroy,"ltraj")

## ----eval=F-------------------------------------------------------------------
#  ## save the Move* object as a RData file
#  save(leroy, file="C:/User/Documents/leroy.RData")
#  
#  ## load an  Move* object saved as a RData file
#  load("C:/User/Documents/leroy.RData")

## ----eval=F-------------------------------------------------------------------
#  ## save as a text file
#  leroyDF <- as.data.frame(leroy)
#  write.table(leroyDF, file="C:/User/Documents/leroyDF.csv", sep=",", row.names = FALSE)

## ----eval=F-------------------------------------------------------------------
#  ## save as a shape file
#  library(rgdal)
#  writeOGR(leroy, "C:/User/Documents/leroySHP/", layer="leroy", driver="ESRI Shapefile")

## ----eval=F-------------------------------------------------------------------
#  ## kml or kmz of movestack ##
#  library(plotKML)
#  ## open a file to write the content
#  kml_open('myStack.kml')
#  ## write the movement data individual-wise
#  for(i in levels(trackId(myStack))){
#    kml_layer(as(myStack[[i]],'SpatialLines'))
#  }
#  ## close the file
#  kml_close('myStack.kml')
#  
#  ## export KML using writeOGR ##
#  for(i in 1:nrow(myStack@idData)){
#    writeOGR(as(myStack[[i]], "SpatialPointsDataFrame"),
#             paste(row.names(myStack@idData)[i], ".kml", sep=""),
#             row.names(myStack@idData)[i], driver="KML")
#  
#    writeOGR(as(myStack[[i]], "SpatialLinesDataFrame"),
#           paste(row.names(myStack@idData)[i], "-track.kml", sep=""),
#           row.names(myStack@idData)[i], driver="KML")
#  }

