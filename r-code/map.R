
library(rgdal)
library(RgoogleMaps)
sites <- read.table("data/vegetation_header_raw.csv", header=T, row.names = 1, sep=",", dec=",")

### Map (mit Internetanschluss) ####
xy <- as.matrix (sites[,c("easting", "northing")]) ## xy-Koordinaten
geotagged <- complete.cases(xy)
lonlat <- xy[geotagged,] ## NAs beseitigen
rownames(lonlat) <- sites[geotagged,"id"]
colnames(lonlat) <- c("lon", "lat")

x11(width=7)
par(mfrow=c(1,1))
mymap_e <- GetMap(center = c(49.1066199, 8.5737459), zoom=12, marker=lonlat)
PlotOnStaticMap(mymap_e)

graphics.off()

