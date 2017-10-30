# DATENTRANSFORMATION:
# a) Zur Normalisierung von Daten (sqrt, log...).
# b) Zum Herauf- und Herabgewichten von kleiner oder großer Werte (z.B. seltener und häufiger Arten).
# c) Um verschiedene Objekte vergleichen zu können (z.B. Aufnahmen mit verschiedener Gesamtdeckung).
###############################################################
### Auswirkungen verschiedener Datentransforamtionen auf
### Daten am Beispiel von Deckungsprozenten.
###############################################################
deckung  <- cbind.data.frame(seq(0,100,1),seq(0,100,1))  # Daten erstellen
names(deckung) <- c("X","Y")
head(deckung)
plot(deckung$Y ~ deckung$X, type="l", lwd=2, lty=2, col="black", ylim=c(0,105),
     ylab="transformierter Wert", xlab="Deckungsprozent", main="Datentransformation")
lines(sqrt(deckung$Y)*10 ~ deckung$X, col="red", lwd=2)
lines(log(deckung$Y)*10 ~ deckung$X, col="orange", lwd=2)
lines((deckung$Y^2)/100 ~ deckung$X, col="lightblue", lwd=2)
legend("topleft", legend=c("Original", "Wurzeltransformation",
                           "Log-Transforamtion", "Quadratische Transformation"), col=c("black", "red", "orange","lightblue"), lty=c(2,1,1,1) , lwd=2)


###############################################################
### Auswirkung von Transformationen auf die Verteilung von Daten ####
###############################################################

# Vergleich der Transformationen anhand normalverteilter Daten
library(fields) # für arrow.plot()
set.seed(44)
normdata <- rnorm(1000, 5, 1.5)

par(mfrow=c(2,2))
hist(normdata, main = "Histogramm normalverteilter Daten", col = "lightblue", border = "white", nclass = 40, freq=F)
box(which = "plot")

# Quadratische Transforamtion
hist(normdata^2, main = "Quadratische Transformation", col = "lightblue", border = "white", nclass = 40, freq=F)
box(which = "plot")
sd(normdata^2)
mean(normdata^2)
arrow.plot(a1 = 75, a2 = 0.02, u = -1, v = 0, arrow.ex=1000, length=0.2, col="darkorange", lwd=5)

# Wurzeltransformation
hist(sqrt(normdata), main = "Wurzeltransformation", col = "lightblue", border = "white", nclass = 40, freq=F)
box(which = "plot")
arrow.plot(a1 = .7, a2 = 1, u = 1, v = 0, arrow.ex=0.5, length=0.2, col="darkorange", lwd=5)

# Log-Transformation
hist(log(normdata + 1), main = "Log-Transformation", col = "lightblue", border = "white", nclass = 40, freq=F)
box(which = "plot")
arrow.plot(a1 = 0.2, a2 = 1, u = 1, v = 0, arrow.ex=0.65, length=0.2, col="darkorange", lwd=5)

###############################################################
### Beispiel einer Normalisierung ####
###############################################################

# Simulation von Daten
set.seed(66)
data <- rgamma(1000, 2, 3)
hist(data, main = "Histogramm der Beispieldaten", col = "lightblue", border = "white", nclass = 40)
box(which = "plot")

#Visuelle Darstellung in einem QQ Plot:
qqnorm(data)

# Log-Transforamtion normalisiert daten
par(mfrow=c(1,2))
hist(data, main = "Beispieldaten", col = "lightblue", border = "white", nclass = 40)
box(which = "plot")
hist(log(data + 1), main = "Transformierte Daten", col = "darkorange", border = "white", nclass = 40)
box(which = "plot")

###############################################################
### Transformationen mit decostand(vegan) ####
###############################################################
library(vegan)
data(varespec)   ### Example data
head(varespec)
colSums(varespec)

vegdataTotal1 <- decostand(t(varespec), method="total")   # Teilen durch die Zeilensumme *Zeilen
vegdataTotal <- t(vegdataTotal1)
colSums(vegdataTotal)
vegdataMax <- decostand(varespec, method="max")       # Teilen durch das Spaltenmaximum *Spalten
colSums(vegdataMax)
vegdataRange <- decostand(varespec, method="range")   # Relativierung                        *Spalten
colSums(vegdataRange )
vegdataStand <- decostand(varespec, method="standardize")  # Standardisierung                     *Spalten
colSums(vegdataStand)
vegdataPA <- decostand(varespec, method="pa")         # Umwandlung der Daten in Präsens-Absens-Daten: 0,1
colSums(vegdataPA)
vegdataScale <- scale(varespec, center=TRUE)                          # Zentrierung der Daten *Spalten
colSums(vegdataScale)


