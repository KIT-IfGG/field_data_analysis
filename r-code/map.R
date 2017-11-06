library(rgdal)
library(RgoogleMaps)
sites <- read.table("data/header_2017.csv", header=T, row.names = 1, sep=";", dec=",")

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
