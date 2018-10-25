library(vegan)
library(isopam)

### Read vegetation data (T) ###
#veg <- read.table("data/vegetation_2017.csv", sep=",", dec=".", row.names = 1, header=TRUE)
veg <- read.table("data/vegetation_2018.csv", sep=",", dec=".", row.names = 1, header=TRUE)
summary(veg)

veg <- t(veg) ### Transpose. For this analysis species must be in cols and sites in rows (for isopam, later NMDs)
veg <- veg[rowSums(veg)>0,]

### Data transformation (T) ####
#veg_trans <- decostand(veg, "max") ### transform
veg_trans <- veg
### Similarity between relevées ###
### Calculate Sörensen index
A <- sum(veg_trans[1,])
B <- sum(veg_trans[2,])
C <- sum(colSums(veg_trans[1:2,]) == 2)
SI <- (2*C)/(A+B)   ### Sörensen index
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

k <-2
plot(vegclust, main="Classification", cex=1.4,  labels=envdata$location) 
rect.hclust(vegclust, k=k, border=c("lightblue","orange", "darkred","darkgreen","red"))   

vegclass1 <- cutree(vegclust, k=k)
table(vegclass1)

### Use isopam for classification (T-b) ####
groups <- isopam(veg_trans)
str(groups)
groups
isotab(groups)   ### Diagnostic species

vegclass <- groups$flat

### Comparison of two classification results
rbind(vegclass, vegclass1)
cor(vegclass, vegclass1, method="spearman")
table(vegclass, vegclass1)

### TASK: Vegetation classification for the full dataset ####
### Read the data, transformation, apply one of the tho classification methods (hclust, isopam),
### decide on number of classes, write an interpretation - why did you choose exactly k classes?
### Delivery via Ilias, COMMENTED R-Code.
