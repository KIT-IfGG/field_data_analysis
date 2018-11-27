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

x11()
par(mfrow=c(1,1))
ordiplot(ordi, choices = c(1,2), type = "n")
points(ordi, choices = c(1,2), display="sites", cex=1.5, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
orditorp(ordi, choices = c(1,2), display="species", cex=0.7, priority=colSums(veg_trans), col="red")
plot(ef, add = TRUE, col = "black", p.max = alpha)
legend("bottomright", legend = c("A", "B", "C", "D"), col = mycols, pch = mysyms, cex = 1.5, bg = "white")

