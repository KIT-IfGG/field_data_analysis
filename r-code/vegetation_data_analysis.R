library(vegan)
library(isopam)
veg <- read.table("data/vegetation.csv", sep=",", dec=".", row.names = 1, header=TRUE)
summary(veg)

veg <- t(veg)
ordi <- metaMDS(veg)

plot(ordi, type="n")
orditorp(ordi, display="sites")
orditorp(ordi, display="species", col="red")

classif <- isopam(veg)
isotab(classif)

classif$flat$level.1


plot(ordi, type="n")
orditorp(ordi, display="sites", col=classif$flat$level.1)
#orditorp(ordi, display="species", col="red")
