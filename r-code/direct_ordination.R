library(vegan)
library(scatterplot3d)
library(isopam)

### Some global variables
mycols <- c("red", "green", "blue", "black")
mysyms <- c(15, 16, 17, 18)

### Read vegetation data ###
veg <- read.table("data/veg_2015-2018_clean.txt", sep=" ", dec=".", row.names = 1, header=TRUE)

### Read site data ####
sites <- read.table("data/sites_ellenberg.txt")
names(sites)

### Check consistancy of veg and sites!!! ####
nrow(veg) == nrow(sites)
table(sites$X %in% rownames(veg))
table(rownames(veg) %in% sites$X)

m <- match(rownames(veg), sites$X)
sites <- sites[m,]
cbind.data.frame(sites$X, rownames(veg)) ### Check again!

### Exclude rare species 
# veg <- veg[,colSums(decostand(veg, "max")) > 3]

### Exclude zero-plots
veg <- veg[rowSums(veg) > 0, ]
nrow(veg) == nrow(sites)
sites <- sites[sites$X %in% rownames(veg), ]

### Transform
veg_trans <- decostand(veg, "max")

sites$northerness[is.na(sites$northerness)] <- mean(sites$northerness, na.rm=TRUE)
sites$slope[is.na(sites$slope)] <- mean(sites$slope, na.rm=TRUE)

### Speices occurences across the gradient ####
spnames <- c("Acer_pseudoplatanus", "Fagus_sylvatica", "Sorbus_torminalis")
myrainbow <- rainbow(length(spnames), alpha = 0.8, s = 1, v = 0.6)   ### Nicer colors!
defpar <- par()   ### Get current plotting parameters
ptcex <- 1.7

x11(width = 10, height = 6)
par(mar = c(4,4,1,11), xpd = FALSE, cex = 1.3)
matplot(sites$northerness, veg[,spnames], pch=16, cex=ptcex, col = myrainbow, frame.plot = FALSE, ylab = "Species cover (%)", xlab = "Northerness", axes = FALSE)
axis(1, pos = 0)
axis(2, pos = -1)
grid(col = "grey") ; abline(h = 100); abline(v = 1)
par(xpd = TRUE)   ### Allow to plot into the margin
legend(par("usr")[2], par("usr")[4] * 0.95, legend = spnames, pch = 16, col = myrainbow, pt.cex = ptcex)   ### Use user coordinates to plot legend into margin
par(defpar) ### Set plotting parameters back to the "default"

# box()  ### Try this to see how position of axes differs from the default.

### Calculate ordination (Canonical correspondence analysis) ####
ordi0 <- cca(veg_trans)   ### CCA
#ordi <- cca(veg_trans ~ T + L + F, sites)   ### CCA
ordi <- cca(veg_trans ~ northerness + slope, sites)

### Plot vegetation classes in ordination ####
x11(width = 10, height=7)
par(mfrow=c(1, 2))
ordiplot(ordi0, choices = c(1,2), type = "n")
orditorp(ordi0, choices = c(1,2), display = "species", cex = 1.2, priority = colSums(veg_trans))
points(ordi0, choices = c(1,2), display = "sites", cex = 1.5, pch = mysyms[sites$vegclass], col = mycols[sites$vegclass])

ordiplot(ordi, choices = c(1,2), type = "n")
orditorp(ordi, choices = c(1,2), display = "species", cex = 1.2, priority = colSums(veg_trans))
points(ordi, choices = c(1,2), display = "sites", cex = 1.5, pch = mysyms[sites$vegclass], col = mycols[sites$vegclass])
legend("bottomright", legend = c("A", "B", "C", "D"), col = mycols, pch = mysyms, cex = 1.5, bg = "white")

### Environmental fits ####
### Numeric variables, linear
ef <- envfit(ordi ~ L + T + K + F + R + N + northerness + slope, sites, na.rm=TRUE, choices = c(1,2), permutations = 100)
ef

alpha <- 0.05

x11()
par(mfrow=c(1,1))
ordiplot(ordi, choices = c(1,2), type = "n")
points(ordi, choices = c(1,2), display="sites", cex=1.5, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
orditorp(ordi, choices = c(1,2), display="species", cex=0.7, priority=colSums(veg_trans), col="red")
plot(ef, add = TRUE, col = "black", p.max = alpha)
legend("bottomright", legend = c("A", "B", "C", "D"), col = mycols, pch = mysyms, cex = 1.5, bg = "white")

