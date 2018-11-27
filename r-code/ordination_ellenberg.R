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

#veg <- veg[rownames(veg) %in% sites$X, ]
#sites <- sites[sites$X %in% rownames(veg), ]
m <- match(rownames(veg), sites$X)
sites <- sites[m,]
cbind.data.frame(sites$X, rownames(veg)) ### Check again!

### Exclude rare species 
#veg <- veg[,colSums(decostand(veg, "max")) > 3]

### Exclude zero-plots
veg <- veg[rowSums(veg) > 0, ]
nrow(veg) == nrow(sites)
sites <- sites[sites$X %in% rownames(veg), ]

### Transform
veg_trans <- decostand(veg, "max")

### Recalculate classification for 2 classes #### 
iso <- isopam(veg_trans, c.fix = 4, ind = c("Abies_alba", "Galium_odoratum", "Quercus_spec", "Quercus_robur", "Pinus_sylvestris", "Luzula_luzuloides", "Carex_sylvatica", "Carex_flacca"), sieve = FALSE)
isotab(iso)
sites$vegclass <- iso$flat #$level.1

### krushal test and boxplots ####
ktest <- apply(sites[,c("L", "T", "K", "F", "R", "N")], 2, function(x) kruskal.test(x ~ sites$vegclass))
pvalues <- unlist(lapply(ktest, function(x) x$p.value))

pvalues <- round(pvalues[order(as.numeric(pvalues))], 3)
alpha = 0.01

x11(width = 9)
pdf("figures/boxplots.pdf", width = 9)
par(mfrow=c(2,3), mar = c(5,5,1,1), cex=1.2)
for(i in 1:length(pvalues)) {
  if(pvalues[i] < alpha) {
    local_colors <- mycols
  } else {
    local_colors <- "grey"
  }
  sites$ell <- sites[,names(pvalues)[i]]
  boxplot(ell ~ vegclass, sites, col=local_colors, ylab = names(pvalues)[i], xlab = "Vegetation classes", names = unique(sites$vegclass))
  mtext(paste0("p-value = ", pvalues[i], " "), 1, -1.2, adj = 1)
}
dev.off()

### Calculate ordination (NMDS) ####
ordi <- metaMDS(veg_trans, distance = "bray", k = 3, trace = TRUE)   ### NMDS
ordi$stress

### Plot vegetation classes in ordination ####
x11()
ordiplot(ordi, choices=c(1,2), type = "n")
orditorp(ordi, choices=c(1,2), display="species", cex=1.2, priority=colSums(veg_trans))
points(ordi, choices=c(1,2), display="sites", cex=1.5, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
legend("bottomright", legend=c("A", "B", "C", "D"), col=mycols, pch=mysyms, cex=1.5, bg="white")

### Environmental fits ####
### Numeric variables, linear
ef <- envfit(ordi ~ L + T + K + F + R + N + northerness + slope, sites, na.rm=TRUE, choices=c(1,2), permutations = 100)
ef

x11()
par(mfrow=c(1,1))
ordiplot(ordi, choices=c(1,2), type = "n")
points(ordi, choices=c(1,2), display="sites", cex=1.5, pch=mysyms[sites$vegclass], col=mycols[sites$vegclass])
orditorp(ordi, choices=c(1,2), display="species", cex=0.7, priority=colSums(veg_trans), col="red")
plot(ef, add=TRUE, col="black", p.max = alpha)
legend("bottomright", legend=c("A", "B", "C", "D"), col=mycols, pch=mysyms, cex=1.5, bg="white")
