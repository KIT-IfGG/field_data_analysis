library(vegan)
library(isopam)

mycols <- c("steelblue", "orange", "darkgreen", rainbow(5))
mysyms <- c(15, 16, 17)


### Read the data ###
veg <- read.table("data/vegetation_2017.csv", sep=",", dec=".", row.names = 1, header=TRUE)
summary(veg)

veg <- t(veg)

### Exclude very rare species ####
veg <- veg[,colSums(decostand(veg, "pa")) > 1]

### Transform data ####
#veg_trans <- decostand(veg, "pa")
veg_trans <- veg

### Ordination ####
ordi <- metaMDS(veg_trans)

plot(ordi, type="n")
orditorp(ordi, display="sites")
orditorp(ordi, display="species", col="red")

### Classification ####
classif <- isopam(veg_trans)
isotab(classif)

#classif <- isopam(veg, c.fix=2)
#isotab(classif)

sink("results/isotab.txt")
isotab(classif)
sink()

vclasses <- classif$flat

### Plot classification results ####
plot(ordi, type="n")
points(scores(ordi, display="sites"),  pch=mysyms[vclasses], col=mycols[vclasses], cex=2)
orditorp(ordi, display="species", col="black", priority=colSums(veg_trans), cex=1)

