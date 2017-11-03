sites <- read.table("data/header_2015-2016-2017.txt")
summary(sites)
nrow(sites)

nas_only <- apply(sites, 2, function(x) sum(is.na(x)) )

sites <- sites[,nas_only!=nrow(sites)]
sites <- sites[,!colnames(sites) %in% c("Observers", "remarks")]
summary(sites)

sites[sites$year==2017,"location"] <- "E"

write.table(sites, "data/header_2015-2016-2017.txt")

