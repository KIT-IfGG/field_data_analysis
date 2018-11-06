library(vegan)
library(isopam)

### Read vegetation data (T) ###
veg <- read.table("data/vegetation_2018.csv", sep=",", dec=".", row.names = 1, header=TRUE)
summary(veg)

veg <- t(veg) ### Transpose. For this analysis species must be in cols and sites in rows (for isopam, later NMDs)
veg <- veg[rowSums(veg) > 0,]

### Data transformation (T) ####
veg_trans <- decostand(veg, "pa") ### transform
#veg_trans <- veg

### Similarity between relevées ###
### Calculate Sörensen index
A <- sum(veg_trans[1,])
B <- sum(veg_trans[2,])
C <- sum(colSums(veg_trans[1:2,]) == 2)
SI <- (2*C)/(A+B)   
SI
1 - SI ### Dissimilarity = "Distance"

### Distance matrix (basis for classification) (T-a) ####
distmat <- vegdist(veg_trans, "bray")    ### 1-SI (for pa data)
distmat[1]

### Classification (T-a) ####
vegclust <- hclust(distmat, method="complete") 

### Plot dendrogramm (T-a) ####
x11()
plot(vegclust, cex=1.3, main="Classification") 

k <- 2
plot(vegclust, main="Classification", cex=1.3) 
rect.hclust(vegclust, k=k, border=c("lightblue","orange", "darkred","darkgreen","red"))   

vegclass1 <- cutree(vegclust, k=k)
table(vegclass1)

### Use isopam for classification ####
groups <- isopam(veg_trans)
str(groups)
groups
isotab(groups)   ### Diagnostic species

vegclass <- groups$flat$level.1

### Comparison of two classification results
rbind(vegclass, vegclass1)
cor(vegclass, vegclass1, method="spearman")
table(vegclass, vegclass1)

### Add groups to header ####
sites <- read.table("data/header_2015-2018_clean.csv", sep=" ", header=TRUE)
sites <- sites[sites$year == 2018,]
m <- match(sites$X, names(vegclass))
sites$vegclass <- vegclass[m]

### Exclude sites with header info but no vegetation data!
sites <- sites[!is.na(sites$vegclass),]

### TASK: Plot dendrogram with isomap vegetation types ("vegclass") as lables.
plot(vegclust, main="Classification", cex=1.4,  labels=sites$vegclass)

### TASK: Vegetation analysis for the full dataset (data/veg_2015-2018_clean.txt) ####
### Read the data, transformation, apply one of the two classification methods (hclust, isopam),
### Decide on number of classes, write an interpretation - why did you choose exactly k classes?
### Delivery via Ilias, COMMENTED R-Code.

