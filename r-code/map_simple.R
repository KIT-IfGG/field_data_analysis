library(raster)
### Check your working directory and
### set it, if not on the right path!
getwd()

### Read site data ####
sites <- read.table("data/header_2015-2018.csv", sep=" ", header=TRUE)

### First plot
sites$color <- 1 + sites$year - min(sites$year, na.rm = TRUE)
mycolors <- c("blue", "red", "green", "black")
plot(latitude ~ longitude, data = sites, col = mycolors[sites$color], pch=16)

### Task: How many vegetation plots per year?
### Hint: table()

### Plot with Shape file
shp <- getData("GADM", country='DE', level=3, path = "../")

sregion <- shp[shp@data$NAME_1 %in% c("Rheinland-Pfalz", "Baden-Württemberg"),]

ka <- shp[sregion@data$NAME_2 %in% c("Karlsruhe"),]   #, "Germersheim", "Südwestpfalz", "Südliche Weinstraße"),]

plot(latitude ~ longitude, data = sites, col = mycolors[sites$color], pch=16, asp=1)
plot(ka, add=TRUE)

### TASK: Add a legend (year, colors) and write the map figure into a pdf file into a new folder "figures".
### Hints: legend(), pdf(), dev.off()




