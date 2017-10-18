library(vegan)

### Read vegetation data ###
veg <- read.table("data/vegetation_2017.csv", sep=",", dec=".", row.names = 1, header=TRUE)
summary(veg)

veg <- t(veg) ### transpose

### Histogram of species frequencies
hist(colSums(decostand(veg, "pa"))) 
# Typical pattern!

### Read header/site data ###
sites <- read.table("data/header_2017.csv", header=T, row.names = 1, sep=";", dec=",")
head(sites)
summary(sites)

### Plot positions
plot(northing ~ easting, sites)

