library(rgdal)
library(RgoogleMaps)

### Read site data ####
sites <- read.table("data/header_2015-2016-2017_vegclasses.txt")

### Map (mit Internetanschluss) ####
xy <- as.matrix (sites[,c("longitude", "latitude")]) ## xy-Koordinaten
geotagged <- complete.cases(xy)
lonlat <- xy[geotagged,] ## NAs beseitigen
rownames(lonlat) <- sites[geotagged,"id"]
colnames(lonlat) <- c("lon", "lat")

x11(width=7)
par(mfrow=c(1,1))
mymap_e <- GetMap(center = c(49.1066199, 8.5737459), zoom=12, marker=lonlat)
PlotOnStaticMap(mymap_e)

graphics.off()

### Write figure into file
pdf("results/map_example.pdf", width=7)
par(mfrow=c(1,1))
mymap_e <- GetMap(center = c(49.1066199, 8.5737459), zoom=12, marker=lonlat)
PlotOnStaticMap(mymap_e)
dev.off()


### TASK: Plot a map (with googe, open street map...) for the entire dataset of region "Eichelberg"
### (Duemmel is too far away to plot it into the same map). Points for sites should indicate the 
### vegetation class they belong to (by col and pch). Upload the code and a png/pdf file of the 
### plotting result.
mycols <- c("red", "green", "blue")
mysyms <- c(15, 16, 17)
mcenter <- apply(na.omit(sites[sites$location=="E", c("latitude", "longitude")]), 2, function(x) median(x))
mymap_e <- GetMap(center = mcenter, maptype="terrain", zoom=14)
PlotOnStaticMap(mymap_e, lat=lonlat[,"lat"], lon=lonlat[,"lon"], pch=mysyms[sites$vegclass], col=mycols[sites$vegclass], cex=1.5) 
