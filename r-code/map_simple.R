library(raster)
### Check your working directory and
### set it, if not on the right path!
getwd()

### Read site data ####
sites <- read.table("data/header_2015-2018.csv", sep=" ", header=TRUE)

sites <- sites[!is.na(sites$longitude),]
sites <- sites[!is.na(sites$year),]
sites <- sites[order(sites$year),]
#write.table(sites, "data/header_2015-2018_clean.csv")
 ### First plot
sites$color <- 1 + sites$year - min(sites$year, na.rm = TRUE)
mycolors <- c("blue", "red", "green", "black")
plot(latitude ~ longitude, data = sites, col = mycolors[sites$color], pch=16)

### Task: How many vegetation plots per year?
### Hint: table()

### Plot with Shape file and DEM ####
shp <- getData("GADM", country='DE', level=2, path = "../")
dem <- getData("alt", country='DE', path = "../")
coords <- SpatialPoints(sites[,c("longitude", "latitude")], CRS(projection(dem)))

buff <- 5000
ka <- crop(shp, extent(buffer(coords, width=buff)))
dem_ka <- crop(dem, extent(buffer(coords, width=buff)))
labpt <- do.call(rbind, lapply(ka@polygons, function (x) x@labpt))

x11()
plot(dem_ka, col=terrain.colors(10, alpha=0.5), xlab="Longitude", ylab="Latitude")
points(latitude ~ longitude, data = sites, col = mycolors[sites$color], pch=16)
plot(ka, add=TRUE)
text(labpt, ka@data$NAME_2, cex=0.7)
legend("topright", legend = unique(sites$year), col = mycolors, pch = 16)

### TASK: Add a legend (year, colors) and write the map figure into a pdf file into a new folder "figures".
### Hints: legend(), pdf(), dev.off()

### SOLUTION
pdf("figures/study_region.pdf", height = 7)
plot(dem_ka, col=terrain.colors(10, alpha=0.7), xlab="Longitude", ylab="Latitude")
points(latitude ~ longitude, data = sites, col = mycolors[sites$color], pch=16, cex=1)
plot(ka, add=TRUE)
text(labpt, ka@data$NAME_2, cex=0.7)
legend("topright", legend = unique(sites$year), col = mycolors, pch = 16)
dev.off()


