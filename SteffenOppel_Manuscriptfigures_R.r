############################################################################################################
#######  MARINE CHARTS AND MAPS OF SST AND CHL-A  ###################################
############################################################################################################
## written by steffen oppel 26 June 2014
## data focussed on Ascension and St Helena, South Atlantic


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PACKAGES AND CUSTOM SCRIPTS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(fields)
library(marmap)
require(maps)
require(mapdata)
require(maptools)
require(geosphere)
require(sp)
require(rgdal)
require(rgeos)
library(raster)
library(plotrix)

source("C:\\STEFFEN\\RSPB\\Statistics\\northarrow.r")
source("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Statistics\\northarrow.r")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA FROM R WORKSPACE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\MABO_analysis3.RData")
load("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\MABO_analysis3.RData")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GET BATHYMETRY DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bathymap<-getNOAA.bathy(lon1=-18, lon2=-2, lat1=-18, lat2=-4, resolution=1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT TRACKING DATA ON A BATHYMETRY MAP
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

outlier<-tracks[tracks$ID==204,]
tracks<-tracks[!tracks$ID==204,]


pdf("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\Ascension\\Fig1.pdf", width=8, height=7)
jpeg("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\Ascension\\Fig1.jpg", width=8, height=7, units="in", quality = 75, res=200)

pdf("A:\\MANUSCRIPTS\\submitted\\Ascension\\Fig1.pdf", width=8, height=7)
jpeg("A:\\MANUSCRIPTS\\in_press\\Ascension\\Fig1.jpg", width=8, height=7, units="in", quality = 100, res=600)


par(mar=c(5,6,0,0), oma=c(0,0,0,0))
plot(Latitude~Longitude, data=tracks[tracks$island=="Ascension",], type='l',col='gray10', xlim=c(-18,-2), ylim=c(-18,-4), main="", frame=F, axes=F, cex.lab=1.8, cex.axis=1.8, mgp=c(3.4,0.7,0), asp=1)
par(new=T)
#plot(bathymap, deep=-5000, shallow=0, step=1500, lwd = 0.8,col = "darkgrey", drawlabels=TRUE, xlim=c(-18,-2), ylim=c(-18,-4), frame=F, axes=F, xlab="", ylab="", asp=1)
#par(new=T)
plot(Latitude~Longitude, data=tracks[tracks$island=="Ascension",], type='l',col='darkred', xlim=c(-18,-2), ylim=c(-18,-4), main="", frame=F, axes=F, cex.lab=1.8, cex.axis=1.8, xlab="", ylab="", asp=1)
par(new=T)
plot(Latitude~Longitude, data=tracks[tracks$island=="StHelena",], type='l',col='blue', xlim=c(-18,-2), ylim=c(-18,-4), frame=F, axes=F, cex.lab=1.8, cex.axis=1.8, xlab="", ylab="", asp=1)
par(new=T)
plot(Latitude~Longitude, data=outlier, type='l',col='gray50', xlim=c(-18,-2), ylim=c(-18,-4), frame=F, axes=F, cex.lab=1.8, cex.axis=1.8, xlab="", ylab="", asp=1)

map("worldHires", add=T, fill=T, col="grey91", asp=1)

draw.circle(-5.734380,-16.003095,radius=1,border='gray27',lty=2,lwd=1)
draw.circle(-14.301331,-7.946484,radius=1,border='gray27',lty=2,lwd=1)
draw.circle(-5.734380,-16.003095,radius=3,border='gray27',lty=1,lwd=1)
draw.circle(-14.301331,-7.946484,radius=3,border='gray27',lty=1,lwd=1)
axis(1,at=seq(-18,-4,2), labels=T, cex=2, cex.lab=1.8, cex.axis=1.8)
axis(2,at=seq(-18,-4,2), labels=T, cex=2, cex.lab=1.8, cex.axis=1.8, las=1, mgp=c(3.4,0.7,0))
text(-9.5,-8,"Ascension", cex=1.8,col='darkred')
text(-11,-16,"St Helena", cex=1.8,col='blue')
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT TRIP TIME AGAINST DISTANCE (FIG 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pdf("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\Ascension\\Fig2.pdf", width=10, height=7)
jpeg("C:\\STEFFEN\\MANUSCRIPTS\\in_prep\\Ascension\\Fig2.jpg", width=10, height=7, units="in", quality = 75, res=200)

pdf("A:\\MANUSCRIPTS\\submitted\\Ascension\\Fig2.pdf", width=10, height=7)
#jpeg("A:\\MANUSCRIPTS\\submitted\\Ascension\\Fig2.jpg", width=10, height=7, units="in", quality = 75, res=200)

par(mar=c(5,5,0,0), oma=c(0,0,0,0))
plot(max_dist~time, data=trip_distances[trip_distances$island=="StHelena",], pch=21,bg="white",xlim=c(0,90), ylim=c(0,350), las=1, cex=1.7, cex.lab=1.5, cex.axis=1.5, axes=F, xlab="", ylab="")
par(new=T)
plot(max_dist~time, data=trip_distances[trip_distances$island=="Ascension",], pch=16, xlim=c(0,90), ylim=c(0,350), las=1, cex=1.7, cex.lab=1.5, cex.axis=1.5, axes=F, xlab="Trip duration (hrs)", ylab="Maximum distance from colony (km)")
axis(1,at=seq(0,90,10), labels=T,cex.axis=1.3)
axis(2,at=seq(0,350,50), cex.axis=1.3, las=1, mgp=c(4,0.5,0), tck = -0.009)
legend(0,350,pch=c(16,1), legend=c("Ascension", "St Helena"), cex=1.5, bty = "n")
segments(x0=20,x1=20, y0=0,y1=300,lty=2, col="darkgray")
dev.off()





#### IN COLOUR FOR PPT PRESENTATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
jpeg("A:\\MANUSCRIPTS\\in_press\\Ascension\\Fig2.jpg", width=10, height=7, units="in", quality = 100, res=600)
par(mar=c(5,5,0,0), oma=c(0,0,0,0))
plot(max_dist~time, data=trip_distances[trip_distances$island=="StHelena",], pch=16,col="blue",xlim=c(0,90), ylim=c(0,350), las=1, cex=1.7, cex.lab=1.5, cex.axis=1.5, axes=F, xlab="", ylab="")
par(new=T)
plot(max_dist~time, data=trip_distances[trip_distances$island=="Ascension",], pch=16, col='darkred',xlim=c(0,90), ylim=c(0,350), las=1, cex=1.7, cex.lab=1.5, cex.axis=1.5, axes=F, xlab="Trip duration (hrs)", ylab="Maximum distance from colony (km)")
axis(1,at=seq(0,90,10), labels=T,cex.axis=1.3)
axis(2,at=seq(0,350,50), cex.axis=1.3, las=1, mgp=c(4,0.5,0), tck = -0.009)
legend(0,350,pch=c(16,16), legend=c("Ascension", "St Helena"), col=c('darkred','blue'),cex=1.5, bty = "n")
#segments(x0=20,x1=20, y0=0,y1=300,lty=2, col="darkgray")
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE GRID AROUND EACH ISLAND AND EXTRACT CHL-A VALUES TO COMPARE DISTRIBUTION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\UKOT")
setwd("C:\\STEFFEN\\RSPB\\UKOT")


latrange<-seq(-22,-2,0.2)
longrange<-seq(-18,-2,0.2)
allgrid<-expand.grid(longrange, latrange)
plot(allgrid)
draw.circle(-5.734380,-16.003095,radius=3,border='gray27',lty=1,lwd=1)
draw.circle(-14.301331,-7.946484,radius=3,border='gray27',lty=1,lwd=1)
names(allgrid)<-c("long","lat")


### CALCULATE DISTANCE OF EACH GRID CELL TO THE COLONY ###
allgrid$distSH<-rdist.earth(x1=allgrid[,1:2],x2=matrix(c(-5.734380,-16.003095), nrow=1), miles=F)
allgrid$distASI<-rdist.earth(x1=allgrid[,1:2],x2=matrix(c(-14.301331,-7.946484), nrow=1), miles=F)
head(allgrid)

### CREATE DATA FRAME FOR ALL SEASONS ####
tracking_seasons<-data.frame(season=c(1:4), start=as.Date(c("2011-11-01", "2013-02-01","2013-12-15","2014-02-15")), end=as.Date(c("2011-12-01", "2013-03-05","2014-01-15","2014-03-15")))
ENV_DATA<-data.frame()
for (t in tracking_seasons$season){
days<-seq(tracking_seasons$start[tracking_seasons$season==t], tracking_seasons$end[tracking_seasons$season==t], 1)
for (s in 1:length(days)){
allgrid$season<-days[s]
allgrid$season_num<-t
ENV_DATA<-rbind(ENV_DATA, allgrid)
}
}
head(ENV_DATA)
ENV_DATA$taxon<-"background"
write.table(ENV_DATA,"ENV_DATA_IMPORT.csv", row.names=F, sep=",")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DOWNLOAD ENV DATA FROM MOVEBANK AND THEN SUMMARISE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### AFTER IMPORTING AND PROCESSING IN MOVEBANK, READ IN ENVIRONMENTAL DATA ###
ENV<-read.table("ENV_DATA.csv", header=T, sep=",")
head(ENV)
ENV<-ENV[,c(3:5,17:20)]
names(ENV)<-c("season","long","lat","SST","CHLA","SSTnight22","SSTnight31")
ENV_DATA$taxon<-NULL
ENV_DATA<-merge(ENV_DATA,ENV, by=c("season","long","lat"), all.x=T)

ENV_DATA$SST<-as.numeric(as.character(ENV_DATA$SST))
ENV_DATA$CHLA<-as.numeric(as.character(ENV_DATA$CHLA))



### ASSIGNING THE ENV DATA TO SEPARATE SEASONS ###
#seasons<-c(as.Date("2011-01-01"),as.Date("2012-01-01"),as.Date("2013-09-01"),as.Date("2014-03-01"),as.Date("2014-10-01"))
#ENV_DATA$season_num<-findInterval(ENV_DATA$season, seasons, all.inside=T)

### CREATING CONCENTRIC CIRCLES ###
dist_bands<-data.frame(distance=c(0,25, 50, 100, 150, 300,100000), band=c(1:7))
ENV_DATA$ASI_band<-findInterval(ENV_DATA$distASI, dist_bands$distance, all.inside=T)
ENV_DATA$SH_band<-findInterval(ENV_DATA$distSH, dist_bands$distance, all.inside=T)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT MEAN DATA FOR DISTANCES FROM COLONY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### AGGREGATE DATA 
SST_plot<-aggregate(SST~ASI_band+season_num, data=ENV_DATA, FUN=mean)
SST_plot[,4]<-aggregate(SST~ASI_band+season_num, data=ENV_DATA, FUN=sd)[,3]
SST_plot[,5]<-aggregate(SST~SH_band+season_num, data=ENV_DATA, FUN=mean)[,3]
SST_plot[,6]<-aggregate(SST~SH_band+season_num, data=ENV_DATA, FUN=sd)[,3]

CHLA_plot<-aggregate(CHLA~ASI_band+season_num, data=ENV_DATA, FUN=mean)
CHLA_plot[,4]<-aggregate(CHLA~ASI_band+season_num, data=ENV_DATA, FUN=sd)[,3]
CHLA_plot[,5]<-aggregate(CHLA~SH_band+season_num, data=ENV_DATA, FUN=mean)[,3]
CHLA_plot[,6]<-aggregate(CHLA~SH_band+season_num, data=ENV_DATA, FUN=sd)[,3]


SST_plot<-SST_plot[SST_plot$ASI_band<6,]
CHLA_plot<-CHLA_plot[CHLA_plot$ASI_band<6,]



### PLOT SUMMARIES


ry <- c(20,28)
rx <- c(0,6)
par (mfrow=c(2,2), mar=c(3,3,1,0), oma=c(2.5,3,0,0))
labels<-c("Nov 2011", "Mar 2013", "Jan 2014", "Mar 2014")
for (s in 1:4){
pb<-subset(SST_plot, season_num==s)
#mh<-subset(CHLA_plot, season_num==s)

## plot 1 for wax blocks
errbar(pb$ASI_band, pb$SST, (pb$SST+pb[,4]/2), (pb$SST-pb[,4]/2), xlim=rx, ylim=ry, cex=1.2, type="b", cex.lab=1.3, pch=16, bty='n', col="red", xlab="", ylab="", axes=F, main = format(seasons[s], "%b %Y"))
text(0,28,labels[s], adj=0, cex=1.5)
par(new=T)
errbar(pb$ASI_band, pb[,5], (pb[,5]+pb[,6]/2), (pb[,5]-pb[,6]/2), xlim=rx, ylim=ry, cex=1.2, type="b", cex.lab=1.3, pch=16, bty='n', col="blue", xlab="", ylab="", axes=F)
axis(1, at= seq(0,6,1), labels=c("","0-25","25-50","50-100","100-150","150-300",""), tck=-0.02, mgp=c(3,1,0), cex.axis=0.9)
axis(2, at=seq(20,28,1), labels=T, tck=-0.02, mgp=c(3,1,0), cex.axis=0.9, las=1)
}
legend(rx[1],22,"St Helena", col='blue', pch=16, cex=1.5, bty='n')
legend(rx[1],23,"Ascension", col='red', pch=16, cex=1.5, bty='n')
mtext("Distance from colony (km)",1, outer=T, line=0.6, cex=2)
mtext("Sea surface temperature (deg C)",2, outer=T, line=1, cex=2)



ry <- c(0,0.2)
rx <- c(0,6)
par (mfrow=c(2,2), mar=c(3,3,1,0), oma=c(2.5,3,0,0))
labels<-c("Nov 2011", "Mar 2013", "Jan 2014", "Mar 2014")
for (s in 1:4){
pb<-subset(CHLA_plot, season_num==s)

## plot 1 for wax blocks
errbar(pb$ASI_band, pb$CHLA, (pb$CHLA+pb[,4]/2), (pb$CHLA-pb[,4]/2), xlim=rx, ylim=ry, cex=1.2, type="b", cex.lab=1.3, pch=16, bty='n', col="red", xlab="", ylab="", axes=F, main = format(seasons[s], "%b %Y"))
text(0,0.2,labels[s], adj=0, cex=1.5)
par(new=T)
errbar(pb$ASI_band, pb[,5], (pb[,5]+pb[,6]/2), (pb[,5]-pb[,6]/2), xlim=rx, ylim=ry, cex=1.2, type="b", cex.lab=1.3, pch=16, bty='n', col="blue", xlab="", ylab="", axes=F)
axis(1, at= seq(0,6,1), labels=c("","0-25","25-50","50-100","100-150","150-300",""), tck=-0.02, mgp=c(3,1,0), cex.axis=0.9)
axis(2, at=seq(0,0.2,0.02), labels=T, tck=-0.02, mgp=c(3,1,0), cex.axis=0.9, las=1)
}
legend(rx[1],0.17,"St Helena", col='blue', pch=16, cex=1.5, bty='n')
legend(rx[1],0.19,"Ascension", col='red', pch=16, cex=1.5, bty='n')
mtext("Distance from colony (km)",1, outer=T, line=0.6, cex=2)
mtext("Chlorophyll-a concentration (mg / l)",2, outer=T, line=1, cex=2)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT BACKGROUND CHLA AND SST FROM MOVEBANK (8-DAY COMPOSITES)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col_locs<-data.frame(x=c(-5.734380,-14.301331), y=c(-16.003095,-7.946484))
PLOT<-aggregate(SST~lat+long+season_num, data=ENV_DATA, FUN=mean)
PLOT$color<-level.colors(PLOT$SST, at = do.breaks(c(20,28), 100),col.regions = rainbow)

par (mfrow=c(2,2), mar=c(3,3,2,0), oma=c(0,0,0,0))
for (s in 1:4){
plot(PLOT$lat[PLOT$season_num==s]~PLOT$long[PLOT$season_num==s], col= PLOT$color[PLOT$season_num==s], pch=15, cex=1.2, xlab="", ylab="", main=labels[s], frame=F)
par(new=T)
plot(y~x, data=col_locs, axes=F, pch=16, cex=2, xlim=c(-20,-2), ylim=c(-22,-2))
}



PLOT<-aggregate(CHLA~lat+long+season_num, data=ENV_DATA, FUN=mean)
PLOT$color<-level.colors(PLOT$CHLA, at = do.breaks(c(0,0.2), 100),col.regions = rainbow)

par (mfrow=c(2,2), mar=c(3,3,2,0), oma=c(0,0,0,0))
for (s in 1:4){
plot(PLOT$lat[PLOT$season_num==s]~PLOT$long[PLOT$season_num==s], col= PLOT$color[PLOT$season_num==s], pch=15, cex=1.2, xlab="", ylab="", main=labels[s], frame=F)
par(new=T)
plot(y~x, data=col_locs, axes=F, pch=16, cex=2, xlim=c(-20,-2), ylim=c(-22,-2))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DOWNLOAD AND PLOT MONTHLY COMPOSITE DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMHchlamday.html
## http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMHsst4mday.html


col_locs<-data.frame(x=c(-5.734380,-14.301331), y=c(-16.003095,-7.946484))
par (mfrow=c(2,2), mar=c(3,3,2,0), oma=c(0,0,0,0))
for (s in 1:3){
filename<-sprintf("chla_season%s.csv",s)
chla<-read.table(filename,header=T, skip=1, sep=",") 
head(chla)
names(chla)<-c("season", "alt","lat","long","CHLA")
chla$season_num<-findInterval(as.Date(chla$season), seasons, all.inside=T)
chla$lat<-as.numeric(as.character(chla$lat))
chla$long<-as.numeric(as.character(chla$long))-360


chla$color<-level.colors(chla$CHLA, at = do.breaks(c(0,0.5), 100),col.regions = rainbow)

plot(chla$lat~chla$long, col= chla$color, pch=15, cex=0.5, xlab="", ylab="", main=labels[s], frame=F)
par(new=T)
plot(y~x, data=col_locs, axes=F, pch=16, cex=2, xlim=c(-20,-2), ylim=c(-18,-2))
}




col_locs<-data.frame(x=c(-5.734380,-14.301331), y=c(-16.003095,-7.946484))
par (mfrow=c(2,2), mar=c(3,3,2,0), oma=c(0,0,0,0))
for (s in 1:3){
filename<-sprintf("sst_season%s.csv",s)
sst<-read.table(filename,header=T, skip=1, sep=",") 
head(sst)
names(sst)<-c("season", "alt","lat","long","SST")
sst$season_num<-findInterval(as.Date(sst$season), seasons, all.inside=T)
sst$lat<-as.numeric(as.character(sst$lat))
sst$long<-as.numeric(as.character(sst$long))-360


sst$color<-level.colors(sst$SST, at = do.breaks(c(15,30), 100),col.regions = rainbow)

plot(sst$lat~sst$long, col= sst$color, pch=15, cex=0.5, xlab="", ylab="", main=labels[s], frame=F)
par(new=T)
plot(y~x, data=col_locs, axes=F, pch=16, cex=2, xlim=c(-20,-2), ylim=c(-18,-2))
}




