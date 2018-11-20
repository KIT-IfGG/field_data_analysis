options(stringsAsFactors = FALSE)
### Read vegetation data ####
veg <- read.table("data/veg_2015-2018_clean.txt", sep=" ", dec=".", row.names = 1, header=TRUE)

### Read site data ####
sites <- read.table("data/header_2015-2018_vegclasses.txt")

## Read Ellenberg values ####
elli <- read.table("data/ellenberg.txt", header=T)
summary(elli)
elli[,2:ncol(elli)] <- apply(elli[,2:ncol(elli)], 2, as.numeric)
summary(elli)

### Elli per site ####
sites_ellis <- list()
for (i in 1:nrow(sites)) {
  si <- veg[sites$X[i] == rownames(veg),]
  si <- si[,si > 0]
#  m <- match(colnames(veg), names(si))
  elli[elli$species %in% colnames(si),]
  sites_ellis[[i]] <- apply(elli[elli$species %in% colnames(si), 2:ncol(elli)], 2, function(x) median(x[m], na.rm=T))
}

sites <- cbind.data.frame(sites, do.call(rbind, sites_ellis))
write.table(sites, "data/sites_ellenberg.txt")

