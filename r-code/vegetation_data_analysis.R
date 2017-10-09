library(vegan)
library(isopam)

mycols <- c("steelblue", "orange", "darkgreen", rainbow(5))


### Read the data ###
veg <- read.table("data/vegetation.csv", sep=",", dec=".", row.names = 1, header=TRUE)
summary(veg)

veg <- t(veg)

### Ordination ####
ordi <- metaMDS(veg)

plot(ordi, type="n")
orditorp(ordi, display="sites")
orditorp(ordi, display="species", col="red")

### Classification ####
classif <- isopam(veg)
isotab(classif)

classif <- isopam(veg, c.fix=3)
isotab(classif)

sink("results/isotab.txt")
isotab(classif)
sink()

vclasses <- classif$flat

### Plot classification results ####
plot(ordi, type="n")
orditorp(ordi, display="sites", col=mycols[vclasses])


