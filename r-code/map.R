library(rgdal)
library(RgoogleMaps)


#install.packages("osmar")
library("osmar")

### Read site data ####
sites <- read.table("data/header_2015-2016-2017_vegclasses.txt")


plot(latitude ~ longitude, data = sites, col = 1 + sites$year - min(sites$year), pch=16)

### Map (mit Internetanschluss) ####
lonlat <- as.matrix (sites[,c("longitude", "latitude")]) ## xy-Koordinaten
cc <- complete.cases(lonlat)
lonlat <- lonlat[cc,] ## NAs beseitigen
rownames(lonlat) <- sites[geotagged,"id"]
colnames(lonlat) <- c("lon", "lat")
box <-as.numeric(apply(lonlat, 2, range))[c(1, 3, 2, 4)]

api <- osmsource_api(url = "http://api.openstreetmap.org/api/0.6/")

box <- corner_bbox(11.579341, 48.15102, 11.582852, 48.1530)
mymap_e <- osmar::get_osm(box, source = osmsource_api())

# mymap_e <- GetMap(center = c(latitude=49.1066199, longitude=8.5737459), zoom=12, marker=lonlat)

x11(width=7)
par(mfrow=c(1,1))
PlotOnStaticMap(mymap_e)
graphics.off()

### Write figure into file
pdf("results/map_example.pdf", width=7)
par(mfrow=c(1,1))
mymap_e <- GetMap(center = c(49.1066199, 8.5737459), zoom=12, marker=lonlat)
PlotOnStaticMap(mymap_e)
dev.off()


### TASK: Plot a map (with google, open street map...) for the entire dataset of region "Eichelberg"
### (Duemmel is too far away to plot it into the same map). Points for sites should indicate the 
### vegetation class they belong to (by col and pch). Upload the code and a png/pdf file of the 
### plotting result.
mycols <- c("red", "green", "blue")
mysyms <- c(15, 16, 17)
mcenter <- apply(na.omit(sites[sites$location=="E", c("latitude", "longitude")]), 2, function(x) median(x))
mymap_e <- GetMap(center = mcenter, maptype="terrain", zoom=14)
PlotOnStaticMap(mymap_e, lat=lonlat[,"lat"], lon=lonlat[,"lon"], pch=mysyms[sites$vegclass], col=mycols[sites$vegclass], cex=1.5) 

