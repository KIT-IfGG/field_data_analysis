### use Date proveded by Schmidtlein to create vegetation dataset technically ready to be used for the analysis 
options(stringsAsFactors=F)

### Read the data ####
veg <- read.table("data/Bruchsal_2017.csv", header=TRUE, sep=",", dec=".")
head(veg)

### Create full species names, NO spaces in names! ####
veg$species <- lapply(strsplit(veg$species, " "), function(x) paste0(x, collapse = "_"))
veg$species <- paste0(veg$species, "_", veg$layer)


### Get column names for cover data ####
s <- colnames(veg)[grep("p", colnames(veg))]
s <- s[grep("spe", s, invert = TRUE)]

### replace NAs ####
veg[,s][is.na(veg[,s])] <- 0

### Get all values in the dataset ####
old_vals <- na.omit(unique(as.character(as.matrix((veg[,s])))))
old_vals
### Values with "*" need to be replaced by meaningfull numeric values!!!

### Create numeric cover values ####
stars <- ifelse(nchar(old_vals)==2, "0.", "") ## WHAT DO * mean?! 
nums <- substring(old_vals, first=nchar(old_vals), last = nchar(old_vals))
new_vals <- as.numeric(paste0(stars, nums))

veg_mat <- as.matrix(veg[,s])

for(i in 1:length(old_vals)) {
  veg_mat[veg_mat == old_vals[i]] <- new_vals[i]
}

veg <- matrix(as.numeric(veg_mat), ncol = ncol(veg_mat), dimnames = list(paste0(veg$species), s))


### Remove duplicated species ####
dpl <- table(rownames(veg))
names(dpl)[dpl > 1]
veg[rownames(veg) %in% names(dpl)[dpl > 1], ]

rowSums(veg[rownames(veg) %in% names(dpl)[dpl > 1], ])

veg <- veg[rowSums(veg) > 0,]

### Write the data ####
write.csv(veg, "data/vegetation.csv")
