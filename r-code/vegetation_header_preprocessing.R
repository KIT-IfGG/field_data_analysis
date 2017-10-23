sites <- read.table("data/header_2017.csv", header=T, row.names = 1, sep=";", dec=",")
names(sites)
summary(sites)

plot(Cover_field_layer_vascular_perc ~ cover_trees_perc, sites)
abline(lm(Cover_field_layer_vascular_perc ~ cover_trees_perc, sites), col="red", lwd=2)

plot(Cover_field_layer_vascular_perc ~ cover_trees_perc, sites)
abline(lm(Cover_field_layer_vascular_perc ~ cover_trees_perc, sites), col="red", lwd=2)



