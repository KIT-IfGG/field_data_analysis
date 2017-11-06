library(vegan)
library(isopam)

mycols <- c("red", "green", "blue")

### Read vegetation data ###
veg <- read.table("data/veg_2015-2016-2017.txt", sep=" ", dec=".", row.names = 1, header=TRUE)
summary(veg)
rownames(veg)

### Exclude rare species ####
table(colSums(decostand(veg, "pa")) < 3)
veg <- veg[,colSums(decostand(veg, "pa")) > 3]

veg_trans <- decostand(veg, "max") ### transform

#####################################################
### Classification ####
#####################################################
iso <- isopam(veg_trans, k=3, ind=c("Fagus_sylvatica", "Quercus_spec", "Quercus_robur", "Abies_alba", "Acer_campestre", "Acer_platanoides"), sieve = FALSE)
isotab(iso)   ### Diagnostic species - decide if this makes sense!
str(iso$flat)
vegclass <- iso$flat

### Write the results into text file
sink("results/isotab.txt")
isotab(iso)
sink()

#####################################################
### Analyse site information by class ####
#####################################################
sites <- read.table("data/header_2015-2016-2017.txt")
summary(sites)
nrow(sites)

### Transform circular variable into cos/sin
sites$aspect_radians <- sites$aspect * pi /180
sites$northerness <- cos(sites$aspect_radians)

### Add vegetation classification results to the site table
m <- match(rownames(sites), names(vegclass))
sites$vegclass <- vegclass[m]

### Save vegetation classes for further analysis
write.table(sites, "data/header_2015-2016-2017_vegclasses.txt")   

### Influence of year, location (factors)
x11(width=9)
par(mfrow=c(1,2), cex=2, mar=c(0,0,0,0), oma=c(1,2,3,1))
plot(table(sites$year, sites$vegclass), col=mycols, main="")
mtext("Year", 3, 1, cex=2)
plot(table(sites$location, sites$vegclass), col=mycols, main="")
mtext("Location", 3, 1, cex=2)

### Influence of physical site factors (numeric)
x11(width=9, height=5)
par(mfrow=c(1,2), cex=1.3, mar=c(4,5,0,0), oma=c(0,0,1,1))
bp <- boxplot(slope ~ vegclass, sites, col=mycols, xlab="Veg. classes", ylab="Slope (°)")
text(1:3, bp$stats[3,] +  abs(bp$stats[3,]*0.1), bp$n, cex=1)
mtext("Slope", 3, -1, cex=1.3)
bp <- boxplot(northerness ~ vegclass, sites, col=mycols, xlab="Veg. classes", ylab="cos(aspect)")
text(1:3, bp$stats[3,] +  abs(bp$stats[3,]*0.1), bp$n, cex=1)
mtext("Northerness", 3, -1, cex=1.3)
### Close all graphic devices
graphics.off()

### Krushkal-Wallis test ####
### Are differences shown by boxplots significant for alpha = 0.05?
kruskal.test(slope ~ vegclass, sites)
kruskal.test(northerness ~ vegclass, sites)

