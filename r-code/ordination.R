library(vegan)
library(scatterplot3d)

### Some global variables
mycols <- c("red", "green", "blue")
mysyms <- c(15, 16, 17)

### Read vegetation data ###
veg <- read.table("data/veg_2015-2016-2017.txt", sep=" ", dec=".", row.names = 1, header=TRUE)

### Read site data ####
sites <- read.table("data/header_2015-2016-2017_vegclasses.txt")

### Check consistance of veg and sites!!! ####
nrow(veg) == nrow(sites)
m <- match(rownames(sites), rownames(veg))
sites <- sites[m,]

### Ordination ####
### Exclude rare species 
veg <- veg[,colSums(decostand(veg, "pa")) > 3]

### Transform
veg_trans <- decostand(veg, "max") 

### Example for 2d plot of vegetation community ###
x11(width=8, height=8)
par(mfrow=c(2,2), mar=c(4,4,1,1))
plot(veg_trans$Fagus_sylvatica, veg_trans$Abies_alba, pch=16, col="red")
plot(veg_trans$Fagus_sylvatica, veg_trans$Anemone_nemorosa, pch=16, col="red")
plot(veg_trans$Fagus_sylvatica, veg_trans$Galium_odoratum, pch=16, col="red")
plot(veg_trans$Fagus_sylvatica, veg_trans$Fraxinus_excelsior, pch=16, col="red")

x11()
plot(veg_trans[,colSums(decostand(veg, "pa")) > 40], pch=16, col="red")
### -> Das ist unübersichtlich!

### Calculate ordination (NDMS)
ordi <- metaMDS(veg_trans,"bray", k=3, trace=T)   ### NMDS

### Plot results
x11(width=10, height=5)
par(mfrow=c(1,2), mar=c(4,4,1,1))
ordiplot(ordi, choices=c(1,2), type = "n")
orditorp(ordi, choices=c(1,2), display="species", cex=1.2, priority=colSums(veg_trans))
points(ordi, choices=c(1,2), display="sites", col="red", cex=1, pch=16)

ordiplot(ordi, choices=c(1,3), type = "n")
orditorp(ordi, choices=c(1,3), display="species", cex=1.2, priority=colSums(veg_trans))
points(ordi, choices=c(1,3), display="sites", col="red", cex=1, pch=16)
dev.off()

x11()
par(col="red", cex=1, pch=16)
scatterplot3d(scores(ordi, display="sites"))
dev.off()

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
### -> k=3 axes

### Plot vegetation classes in ordination ####
x11()
ordiplot(ordi, choices=c(1,2), type = "n")
orditorp(ordi, choices=c(1,2), display="species", cex=1.2, priority=colSums(veg_trans))
points(ordi, choices=c(1,2), display="sites", cex=1.5, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
legend("bottomright", legend=c("Eichelberg-1", "Eichelberg-2", "Duemmel"), col=mycols, pch=mysyms, cex=1.5, bg="white")

### Environmental fit ####
### Numeric variables
ef <- envfit(ordi ~ northerness +  slope, sites, na.rm=TRUE, choices=c(1,2))
ef

ordiplot(ordi, choices=c(1,2), type = "n")
points(ordi, choices=c(1,2), display="sites", cex=1.5, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
plot(ef, add=TRUE, label=c("Northerness", "Slope"), col="black")
legend("bottomright", legend=c("Eichelberg-1", "Eichelberg-2", "Duemmel"), col=mycols, pch=mysyms, cex=1.5, bg="white")

ef13 <- envfit(ordi~ northerness +  slope, sites, na.rm=TRUE, choices=c(1,3))
ef13
ordiplot(ordi, choices=c(1,3), type = "n")
points(ordi, choices=c(1,3), display="sites", cex=1.5, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
plot(ef13, add=TRUE, label=c("Northerness", "Slope"), col="black")
legend("bottomright", legend=c("Eichelberg-1", "Eichelberg-2", "Duemmel"), col=mycols, pch=mysyms, cex=1.5, bg="white")

### Categorical variables
ordiplot(ordi, choices=c(1,2), type = "n")
points(ordi, choices=c(1,2), display="sites", cex=1.5, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
ordiellipse(ordi, choices=c(1,2), sites$year, label = TRUE, cex=1.3)
#ordispider(ordi, sites$year, label = TRUE, cex=1.3)
#ordihull(ordi, sites$year, label = TRUE, cex=1.3)
