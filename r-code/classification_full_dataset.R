library(vegan)
library(isopam)

mycols <- c("red", "green", "blue", "black")

### Read vegetation data ###
veg <- read.table("data/veg_2015-2018_clean.txt", sep=" ", dec=".", row.names = 1, header=TRUE)
summary(veg)
rownames(veg)

sites <- read.table("data/header_2015-2018_clean.csv", sep=" ", header=TRUE)
summary(sites)
nrow(sites)

nrow(veg) == nrow(sites)
table(sites$X %in% rownames(veg))
table(rownames(veg) %in% sites$X)

### Exclude rare species ####
table(colSums(decostand(veg, "pa")) < 3)
veg <- veg[,colSums(decostand(veg, "pa")) > 3]

veg <- veg[rowSums(veg) > 0 ,]
veg_trans <- decostand(veg, "max") ### transform

#####################################################
### Classification ####
#####################################################
iso <- isopam(veg_trans, c.fix = 3, ind=c("Fagus_sylvatica", "Quercus_spec", "Quercus_robur", "Abies_alba", "Galium_odoratum", "Fraxinus_excelsior"))
isotab(iso)   ### Diagnostic species - decide if this makes sense!
str(iso$flat)
vegclass <- iso$flat

### Optional: Set own diagnostic species and/or number of classes
### iso <- isopam(veg_trans, c.fix = 3, ind=c("Fagus_sylvatica", "Quercus_spec", "Quercus_robur", "Abies_alba", "Acer_campestre", "Acer_platanoides"), sieve = FALSE)


### Write the results into text file
sink("results/isotab.txt")
isotab(iso)
sink()

#####################################################
### Analyse site information by class ####
#####################################################
sites <- read.table("data/header_2015-2018_clean.csv", sep=" ", header=TRUE)
summary(sites)
nrow(sites)

### Transform circular variable into cos/sin
sites$aspect_radians <- sites$aspect * pi /180
sites$northerness <- cos(sites$aspect_radians)

### Add vegetation classification results to the site table
m <- match(sites$X, names(vegclass))
sites$vegclass <- vegclass[m]

### Save vegetation classes for further analysis
write.table(sites, "data/header_2015-2018_vegclasses.txt")   

### Influence of year,location (factors)
x11(width = 9)
par(mfrow = c(1,1), cex = 2, mar = c(0,0,0,0), oma = c(1,2,3,1))
plot(table(sites$year, sites$vegclass), col=mycols, main="")
mtext("Year", 3, 1, cex=2)

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

### Map ####
sites$color <- sites$vegclass
mycolors <- c("blue", "red", "green", "black")
plot(latitude ~ longitude, data = sites, col = mycolors[sites$color], pch=16)

### TASK: Calculate species richness for each vegetation plot and add to 
### the sites table.Caution, there is an NA in the data you have to deal with!
sr <- rowSums(decostand(veg, "pa"))
sites$species_richness <- sr[match(sites$X, names(sr))]

boxplot(species_richness ~ vegclass, sites, col=mycols, xlab="Veg. classes", ylab="species_richness")
