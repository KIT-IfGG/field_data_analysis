library(vegan)
library(scatterplot3d)
library(isopam)

### Some global variables
mycols <- c("red", "green", "blue", "black")
mysyms <- c(15, 16, 17, 18)

### Read vegetation data ###
veg <- read.table("data/veg_2015-2018_clean.txt", sep=" ", dec=".", row.names = 1, header=TRUE)

### Read site data ####
sites <- read.table("data/header_2015-2018_vegclasses.txt")

### Check consistancy of veg and sites!!! ####
nrow(veg) == nrow(sites)
table(sites$X %in% rownames(veg))
table(rownames(veg) %in% sites$X)

#veg <- veg[rownames(veg) %in% sites$X, ]
#sites <- sites[sites$X %in% rownames(veg), ]
m <- match(sites$X, rownames(veg))
sites <- sites[m,]

### Ordination ####
### Exclude rare species 
veg <- veg[,colSums(decostand(veg, "pa")) > 3]

### Exclude zero-plots
veg <- veg[rowSums(veg) > 0, ]
nrow(veg) == nrow(sites)
sites <- sites[sites$X %in% rownames(veg), ]

### Transform
veg_trans <- decostand(veg, "max")

### Recalculate classification for 2 classes #### 
iso <- isopam(veg_trans)
isotab(iso)
sites$vegclass <- iso$flat$level.1

### Example for 2d plot of vegetation community ###
x11(width=8, height=8)
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot(veg_trans$Fagus_sylvatica, veg_trans$Abies_alba, pch=16, col="red")
plot(veg_trans$Fagus_sylvatica, veg_trans$Anemone_nemorosa, pch=16, col="red")
plot(veg_trans$Fagus_sylvatica, veg_trans$Galium_odoratum, pch=16, col="red")
plot(veg_trans$Fagus_sylvatica, veg_trans$Fraxinus_excelsior, pch=16, col="red")

x11()
plot(veg_trans[, colSums(decostand(veg, "pa")) > 60], pch=16, col="red")
### -> Das ist unübersichtlich!

### Calculate ordination (NDMS)
ordi <- metaMDS(veg_trans, distance = "bray", k = 3, trace = TRUE)   ### NMDS

### Alternative ordination method (there are more!)
#ordi <- decorana(veg_trans) 

### Plot results
x11(width=13, height=6.5)
par(mfrow=c(1,2), mar=c(4,4,1,1))
ordiplot(ordi, choices=c(1,2), type = "n")
orditorp(ordi, choices=c(1,2), display="species", cex=1.2, priority=colSums(veg_trans))
points(ordi, choices=c(1,2), display="sites", col="red", cex=1, pch=16)

ordiplot(ordi, choices=c(1,3), type = "n")
orditorp(ordi, choices=c(1,3), display="species", cex=1.2, priority=colSums(veg_trans))
points(ordi, choices=c(1,3), display="sites", col="red", cex=1, pch=16)
dev.off()

x11()
par(pch = 16, cex = 1.3)
scatterplot3d(scores(ordi, display="sites"), color = mycols[sites$vegclass])
graphics.off()

### Goodness NMDS ####
### Calculate "stress"
x <- vegdist(veg_trans, "bray")   ### "True distances"
y <- dist(scores(ordi))  ### "Distances in NDMS"
plot(x, y, pch=1, col="blue", xlab="Observed distances", ylab="Distances in NMDS")
lines(smooth.spline(x, y, df=5), col="red", lwd=2)

stressplot(ordi)   ### "Ordination distance" is not Euclidian distance but similar (Disparität, Monotonie).
ordi$stress        ###  1 - 5: Sehr gut; 5 - 10: Gut; 10 - 15: Brauchbar, aber Einzelheiten sollten nicht interpretiert werden (c.f. Leyer, Wesche 2007 Tab.12.2).

### Find appropiate number of axes ####
n <- 1:10   ### Number of axes
stress <- lapply(as.list(n), function(x) metaMDS(veg_trans,"bray", x)$stress)
plot(n, stress, type="b", pch=16, col="blue", xlab="Number of axes", ylab="Stress")
### -> k = 3 axes

### Plot vegetation classes in ordination ####
x11()
ordiplot(ordi, choices=c(1,2), type = "n")
orditorp(ordi, choices=c(1,2), display="species", cex=1.2, priority=colSums(veg_trans))
points(ordi, choices=c(1,2), display="sites", cex=1.5, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
legend("bottomright", legend=c("A", "B"), col=mycols, pch=mysyms, cex=1.5, bg="white")

### Plot single species cover data into ordination ####
x11()
ordiplot(ordi, choices=c(1,2), type = "n")
orditorp(ordi, choices=c(1,2), display="species", cex=1.2, priority=colSums(veg_trans))
points(ordi, choices=c(1,2), display="sites", cex=(veg_trans$Fagus_sylvatica+0.2)*2, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
legend("bottomright", legend=c("A", "B"), col=mycols, pch=mysyms, cex=1.5, bg="white")

### Species richness ####
sites$species_richness <- rowSums(decostand(veg, "pa"))

ordiplot(ordi, choices=c(1,2), type = "n")
points(ordi, choices=c(1,2), display="sites", cex=(0.5+sites$species_richness/max(sites$species_richness))^2, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])

### Tree cover
ordiplot(ordi, choices=c(1,2), type = "n")
points(ordi, choices=c(1,2), display="sites", cex=(sites$cover_tree/70)^2, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])

### Environmental fits ####
### Numeric variables, linear
ef <- envfit(ordi ~ northerness +  slope + species_richness + cover_tree, sites, na.rm=TRUE, choices=c(1,2))
ef

ordiplot(ordi, choices=c(1,2), type = "n")
points(ordi, choices=c(1,2), display="sites", cex=1.5, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
plot(ef, add=TRUE, label=c("Northerness", "Slope", "Species richness", "Tree cover"), col="black")
legend("bottomright", legend=c("A", "B"), col=mycols, pch=mysyms, cex=1.5, bg="white")

### Categorical variables ####
ordiplot(ordi, choices=c(1,2), type = "n")
points(ordi, choices=c(1,2), display="sites", cex=1.5, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
#ordiellipse(ordi, choices=c(1,2), sites$year, label = TRUE, cex=1.3)
ordispider(ordi, sites$year, label = TRUE, cex=1.3)

x11()
ordiplot(ordi, choices=c(1,2), type = "n")
points(ordi, choices=c(1,2), display="sites", cex=1.5, pch=16, col=1+as.numeric(sites$northerness>0))
ordispider(ordi, sites$northerness>0, label = TRUE, cex=1.3, col=c(1,2))
#ordihull(ordi, sites$year, label = TRUE, cex=1.3)

### Numeric variables, isolines ####
ordiplot(ordi, choices=c(1,2), type = "n")
points(ordi, choices=c(1,2), display="sites", cex=1.3, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
fit <- ordisurf(ordi, sites$northerness, add=T, labcex=1, col="black", nlevels=3)

ordiplot(ordi, choices=c(1,2), type = "n")
points(ordi, choices=c(1,2), display="sites", cex=1.3, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
ordisurf(ordi, sites$slope, add=T, labcex=1.5, col="black", nlevels=5)

### TASK: Add Ellenberg values. Fill the document with one column for species name (SAME as in vegetation table) and columns for the Ellenberg values. See web-link to shared doc in Ilias!

### TASK: Create boxplots for Ellenberg values for isopam vegetation classes (one figure per pdf page or a multi-panel plot). Save them in a pdf file.

### TASK: Display Ellenberg values in the ordination plot and interprete the results.

