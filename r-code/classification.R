library(vegan)
library(isopam)

### Read vegetation data ###
veg <- read.table("data/vegetation_2017.csv", sep=",", dec=".", row.names = 1, header=TRUE)
summary(veg)

veg <- t(veg) ### transpose

veg_pa <- decostand(veg, "pa") ### transform

### Similarity between relevées ###
### Calculate Sörensen index
A <- sum(veg_pa[1,])
B <- sum(veg_pa[2,])
C <- sum(colSums(veg_pa[1:2,]) == 2)
SI <- (2*C)/(A+B)   ### Sörensen index
SI
1 - SI ### Dissimilarity = "Distance"

### Distance matrix (basis for classification) ####
distmat <- vegdist(veg_pa, "bray")    ### 1-SI (for pa data)
distmat[1]

### Classification ####
vegclust <- hclust(distmat, method="complete") 

### Plot dendrogramm ####
x11()
plot(vegclust, cex=1.3, main="Classification") 

k <-2
plot(vegclust, main="Classification", cex=1.4,  labels=envdata$location) 
rect.hclust(vegclust, k=k, border=c("lightblue","orange", "darkred","darkgreen","red"))   

vegclass1 <- cutree(vegclust, k=k)
table(vegclass1)

### Use isopam for classification ####
groups <- isopam(veg_pa)
str(groups)
groups
isotab(groups)   ### Diagnostic species

vegclass <- groups$flat$level.1

### Comarison of two classification results
rbind(vegclass, vegclass1)
cor(vegclass, vegclass1, method="spearman")
table(vegclass, vegclass1)
