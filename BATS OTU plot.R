#Clean up all tables and make depth profile plots

##### Level 1 #####
OTU1 <- read.csv("./OTU tables/level-1.csv")

# order by ascending depth
attach(OTU1)
x1 <- OTU1[order(Depth),]
detach(OTU1)

#### Archaea #####
plot(x1$Depth ~ x1$D_0__Archaea, type = "n", main = "Archaea OTU vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 2000, 4000, 6000, 8000), h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x1$Depth ~ x1$D_0__Archaea)
# add trendline, get function from lm(), but need to convert the intersection 
# for abline, because we are plotting for OTU vs depth
summary(lm(x1$D_0__Archaea ~ x1$Depth))
abline(4382.7538/0.2299, -1/0.2299, lty = 5, col = 4)
# Use Estimate values, (Intercept)/-x1$Depth, 1/-x1$Depth
legend("bottomright", legend = c("Archaea", expression(paste(plain(r)^{paste("2")}, 
                                                             paste("="), paste("0.2256")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")
# use the "Multiple R-squared" value

##### Bacteria ######
plot(x1$Depth ~ x1$D_0__Bacteria, type = "n", main = "Bacteria OTU vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(5000, 10000, 15000, 20000, 25000, 30000), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x1$Depth ~ x1$D_0__Bacteria)
summary(lm(OTU1$D_0__Bacteria ~ OTU1$Depth))
abline(-13510.711/1.199, 1/1.199, lty = 5, col = 4)
legend("right", legend = c("Bacteria", expression(paste(plain(r)^{paste("2")}, 
                    paste("="), paste("0.05732")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

###### Shannon Index #####
library(vegan)
OTU1$Shannon <- diversity(OTU1, index = "shannon")
# plot shannon vs depth
plot(OTU1$Depth ~ OTU1$Shannon, type = "n", main = "Shannon Diversity vs Depth",
     xlab = "Shannon Diversity", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
grid(nx = NA, ny = NULL, lty = 2, lwd = 1, col = "lightgray")
points(OTU1$Depth ~ OTU1$Shannon)
summary(lm(OTU1$Shannon ~ OTU1$Depth))
abline(-7.220e-01/8.742e-05, 1/8.742e-05, lty = 5, col = 4)
legend("bottomleft", legend = c("Shannon index", expression(paste(plain(r)^{paste("2")}, 
                                                        paste("="), paste("0.2952")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

###### Level 2 ######
OTU2 <- read.csv("./OTU tables/level-2.csv")

#### Shannon #####
OTU2$Shannon <- diversity(OTU2, index = "shannon")
# order by depth
attach(OTU2)
OTU2 <- OTU2[order(Depth),]
detach(OTU2)

# plot shannon vs depth
plot(OTU2$Depth ~ OTU2$Shannon, type = "n", main = "Shannon Diversity vs Depth",
     xlab = "Shannon Diversity", ylab = "Depth(m)", 
     ylim = c(4600, 0), las = 1)
grid(nx = NA, ny = NULL, lty = 2, lwd = 1, col = "lightgray")
points(OTU2$Depth ~ OTU2$Shannon)
lw1 <- loess(Depth ~ Shannon, data = OTU2, span = 5)
j <- order(OTU2$Shannon)
lines(OTU2$Shannon[j], lw1$fitted[j], col = "red", lwd = 2)
legend("bottomleft", legend = c("Shannon Index","Trendline"),  
       lty = c(0, 1), lwd = c(NA, 2), col = c(1, "red"), pch = c(1, NA), 
       bty = "o", box.lty = 0)

##### Crenarchaeota #####
plot(x2$Depth ~ x2$D_0__Archaea.D_1__Crenarchaeota, type = "n", 
     main = "Crenarchaeota vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 10, 20, 30, 40), h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Archaea.D_1__Crenarchaeota)
legend("bottomright", legend = "Crenarchaeota",
       col = 1, pch = 1, bty = "o", bg = "white")

##### Euryarchaeota #####
plot(x2$Depth ~ x2$D_0__Archaea.D_1__Euryarchaeota, type = "n", 
     main = "Euryarchaeota vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 200, 400, 600, 800, 1000), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Archaea.D_1__Euryarchaeota)
summary(lm(x2$D_0__Archaea.D_1__Euryarchaeota ~ x2$Depth))
abline(-481.31045/0.07977, 1/0.07977, lty = 5, col = 4)
legend("bottomleft", legend = c("Euryarchaeota", expression(paste(plain(r)^{paste("2")}, 
                                                        paste("="), paste("0.1147")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Nanoarchaeota #####
plot(x2$Depth ~ x2$D_0__Archaea.D_1__Nanoarchaeaeota, type = "n", 
     main = "Nanoarchaeota vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 100, 200, 300, 400, 500, 600), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Archaea.D_1__Nanoarchaeaeota)
summary(lm(x2$D_0__Archaea.D_1__Nanoarchaeaeota ~ x2$Depth))
abline(-47.3582/0.1026, 1/0.1026, lty = 5, col = 4)
legend("bottomleft", legend = c("Nanoarchaeota", expression(paste(plain(r)^{paste("2")}, 
                                                                  paste("="), paste("0.5285")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Thaumarchaeota #####
plot(x2$Depth ~ x2$D_0__Archaea.D_1__Thaumarchaeota, type = "n", 
     main = "Thaumarchaeota vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), xlim = c(0, 8000), las = 1)
abline(v = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Archaea.D_1__Thaumarchaeota)
summary(lm(x2$D_0__Archaea.D_1__Thaumarchaeota
           ~ x2$Depth))
abline(3845.5598/0.4152, -1/0.4152, lty = 5, col = 4)
legend("bottomright", legend = c("Thaumarchaeota", expression(paste(plain(r)^{paste("2")}, 
                                                                  paste("="), paste("0.09137")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Other Archaea #####
plot(x2$Depth ~ x2$D_0__Archaea.__, type = "n", 
     main = "Other Archaea vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 10, 20, 30, 40, 50, 60, 70), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Archaea.__)
legend("right", legend = "Other Archaea", 
       col = 1, pch = 1, bty = "o", bg = "white")

##### Acidobacteria #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Acidobacteria, type = "n", 
     main = "Acidobacteria vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 500, 1000, 1500, 2000), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Acidobacteria)
summary(lm(x2$D_0__Bacteria.D_1__Acidobacteria ~ x2$Depth))
abline(-812.38534/0.10005, 1/0.10005, lty = 5, col = 4)
legend("bottomright", legend = c("Acidobacteria", expression(paste(plain(r)^{paste("2")}, 
                                                                    paste("="), paste("0.06207")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Actinobacteria #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Actinobacteria, type = "n", 
     main = "Actinobacteria vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 500, 1000, 1500, 2000), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Actinobacteria)
summary(lm(x2$D_0__Bacteria.D_1__Actinobacteria ~ x2$Depth))
abline(998.38713/0.20873, -1/0.20873, lty = 5, col = 4)
legend("bottomright", legend = c("Actinobacteria", expression(paste(plain(r)^{paste("2")}, 
                                                                   paste("="), paste("0.3709")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### AncK6 #####

##### BRC1 #####

##### Bacteroidetes #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Bacteroidetes, type = "n", 
     main = "Bacteroidetes vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 500, 1000, 1500), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Bacteroidetes)
summary(lm(x2$D_0__Bacteria.D_1__Bacteroidetes ~ x2$Depth))
abline(-9.89986/0.18669, 1/0.18669, lty = 5, col = 4)
legend("right", legend = c("Bacteroidetes", expression(paste(plain(r)^{paste("2")}, 
                                                                    paste("="), paste("0.361")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Calditrichaeota #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Calditrichaeota, type = "n", 
     main = "Calditrichaeota vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 10, 20, 30, 40, 50, 60, 70), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Calditrichaeota)
summary(lm(x2$D_0__Bacteria.D_1__Calditrichaeota ~ x2$Depth))
abline(10.43466/0.01119, 1/0.01119, lty = 5, col = 4)
legend("right", legend = c("Calditrichaeota", expression(paste(plain(r)^{paste("2")}, 
                                                             paste("="), paste("0.4933")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Chloroflexi #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Chloroflexi, type = "n", 
     main = "Chloroflexi vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 1000, 2000, 3000, 4000, 5000, 6000), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Chloroflexi)
summary(lm(x2$D_0__Bacteria.D_1__Chloroflexi ~ x2$Depth))
abline(-1895.0309/0.3772, 1/0.3772, lty = 5, col = 4)
legend("right", legend = c("Chloroflexi", expression(paste(plain(r)^{paste("2")}, 
                                                               paste("="), paste("0.114")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Cyanobacteria #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Cyanobacteria, type = "n", 
     main = "Cyanobacteria vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 100, 200, 300, 400, 500), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Cyanobacteria)
summary(lm(x2$D_0__Bacteria.D_1__Cyanobacteria ~ x2$Depth))
abline(208.02074/0.04095, -1/0.04095, lty = 5, col = 4)
legend("right", legend = c("Cyanobacteria", expression(paste(plain(r)^{paste("2")}, 
                                                           paste("="), paste("0.2005")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Dadabacteria ######
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Dadabacteria, type = "n", 
     main = "Dadabacteria vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 100, 200, 300, 400, 500), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Dadabacteria)
summary(lm(x2$D_0__Bacteria.D_1__Dadabacteria~ x2$Depth))
abline(150.59030/0.01559, -1/0.01559, lty = 5, col = 4)
legend("bottomright", legend = c("Dadabacteria", expression(paste(plain(r)^{paste("2")}, 
                                                              paste("="), paste("0.02612")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Firmicutes #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Firmicutes, type = "n", 
     main = "Firmicutes vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 200, 400, 600, 800), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Firmicutes)
summary(lm(x2$D_0__Bacteria.D_1__Firmicutes ~ x2$Depth))
abline(494.01474/0.10994, -1/0.10994, lty = 5, col = 4)
legend("bottomright", legend = c("Firmicute", expression(paste(plain(r)^{paste("2")}, 
                                                                  paste("="), paste("0.2827")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Gemmatimonadetes #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Gemmatimonadetes, type = "n", 
     main = "Gemmatimonadetes vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 100, 200, 300, 400), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Gemmatimonadetes)
summary(lm(x2$D_0__Bacteria.D_1__Gemmatimonadetes ~ x2$Depth))
abline(-113.504644/0.04304, 1/0.04304, lty = 5, col = 4)
legend("topright", legend = c("Gemmatimonadetes", expression(paste(plain(r)^{paste("2")}, 
                                                               paste("="), paste("0.1686")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Kiritimatiellaeota #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Kiritimatiellaeota, type = "n", 
     main = "Kiritimatiellaeota vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 50, 100, 150, 200, 250), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Kiritimatiellaeota)
summary(lm(x2$D_0__Bacteria.D_1__Kiritimatiellaeota ~ x2$Depth))
abline(-25.81423/0.02820, 1/0.02820, lty = 5, col = 4)
legend("topright", legend = c("Kiritimatiellaeota", expression(paste(plain(r)^{paste("2")}, 
                                                                   paste("="), paste("0.248")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Lentisphaerae #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Lentisphaerae, type = "n", 
     main = "Lentisphaerae vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 50, 100, 150, 200), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Lentisphaerae)
summary(lm(x2$D_0__Bacteria.D_1__Lentisphaerae ~ x2$Depth))
abline(-34.835698/0.02232, 1/0.02232, lty = 5, col = 4)
legend("topright", legend = c("Lentisphaerae", expression(paste(plain(r)^{paste("2")}, 
                                                                     paste("="), paste("0.3446")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Marinimicrobia SAR406 #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Marinimicrobia..SAR406.clade., type = "n", 
     main = "Marinimicrobia SAR40 vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 500, 1000, 1500, 2000), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Marinimicrobia..SAR406.clade.)
summary(lm(x2$D_0__Bacteria.D_1__Marinimicrobia..SAR406.clade.~ x2$Depth))
abline(1018.98950/0.02051, -1/0.02051, lty = 5, col = 4)
legend("bottomright", legend = c("SAR406", expression(paste(plain(r)^{paste("2")}, 
                                                                paste("="), paste("0.0035")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Nitrospinae #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Nitrospinae, type = "n", 
     main = "Nitrospinae vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 100, 200, 300, 400, 500), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Nitrospinae)
summary(lm(x2$D_0__Bacteria.D_1__Nitrospinae ~ x2$Depth))
abline(-181.54968/0.05185, 1/0.05185, lty = 5, col = 4)
legend("bottomleft", legend = c("Nitrospinae", expression(paste(plain(r)^{paste("2")}, 
                                                            paste("="), paste("0.282")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### PAUC34f #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__PAUC34f, type = "n", 
     main = "PAUC34f vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 100, 200, 300, 400, 500), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__PAUC34f)
summary(lm(x2$D_0__Bacteria.D_1__PAUC34f ~ x2$Depth))
abline(-135.81635/0.04938, 1/0.04938, lty = 5, col = 4)
legend("topright", legend = c("PAUC34f", expression(paste(plain(r)^{paste("2")}, 
                                                                paste("="), paste("0.1879")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Planctomycetes ######
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Planctomycetes, type = "n", 
     main = "Planctomycetes vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 500, 1000, 1500, 2000, 2500, 3000), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Planctomycetes)
summary(lm(x2$D_0__Bacteria.D_1__Planctomycetes ~ x2$Depth))
abline(-555.1925/0.3383, 1/0.3383, lty = 5, col = 4)
legend("topright", legend = c("Planctomycetes", expression(paste(plain(r)^{paste("2")}, 
                                                          paste("="), paste("0.3046")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Proteobacteria #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Proteobacteria, type = "n", 
     main = "Proteobacteria vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 2000, 4000, 6000, 8000, 10000, 12000), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Proteobacteria)
summary(lm(x2$D_0__Bacteria.D_1__Proteobacteria ~ x2$Depth))
abline(-6444.9365/0.1502, 1/0.1502, lty = 5, col = 4)
legend("bottomright", legend = c("Proteobacteria", expression(paste(plain(r)^{paste("2")}, 
                                                                 paste("="), paste("0.004927")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

###### Spirochaetes #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Spirochaetes, type = "n", 
     main = "Spirochaetes vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 20, 40, 60, 80, 100, 120), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Spirochaetes)
summary(lm(x2$D_0__Bacteria.D_1__Spirochaetes ~ x2$Depth))
abline(-46.548897/0.005602, 1/0.005602, lty = 5, col = 4)
legend("topright", legend = c("Spirochaetes", expression(paste(plain(r)^{paste("2")}, 
                                                                    paste("="), paste("0.02642")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

###### Verrucomicrobia #####
plot(x2$Depth ~ x2$D_0__Bacteria.D_1__Verrucomicrobia, type = "n", 
     main = "Verrucomicrobia vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 200, 400, 600, 800, 1000, 1200), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x2$Depth ~ x2$D_0__Bacteria.D_1__Verrucomicrobia)
summary(lm(x2$D_0__Bacteria.D_1__Verrucomicrobia ~ x2$Depth))
abline(-254.59902/0.07779, 1/0.07779, lty = 5, col = 4)
legend("topright", legend = c("Verrucomicrobia", expression(paste(plain(r)^{paste("2")}, 
                                                               paste("="), paste("0.1191")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

#### R HW #####
par(mfrow=c(1,2))
library(ggplot2)
ggplot(x2, aes(D_0__Bacteria.D_1__Nitrospinae, Depth)) +
    geom_point(colour = "red", size = 3) +
    xlab ("OTU") +
    ylab ("Depth") +
    theme_classic() +
    ggtitle("Nitrospinae vs Depth") +
    geom_smooth(method = "lm")

###### Level 3 ######
OTU3 <- read.csv("./OTU tables/level-3.csv")

attach(OTU3)
x3 <- OTU3[order(Depth),]
detach(OTU3)

##### Alphaproteobacteria #####
plot(x3$Depth ~ x3$D_0__Bacteria.D_1__Proteobacteria.D_2__Alphaproteobacteria, type = "n", 
     main = "Alphaproteobacteria vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000), h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x3$Depth ~ x3$D_0__Bacteria.D_1__Proteobacteria.D_2__Alphaproteobacteria)
summary(lm(x3$D_0__Bacteria.D_1__Proteobacteria.D_2__Alphaproteobacteria ~ x3$Depth))
abline(3.993e+03/9.108e-03, -1/9.108e-03, lty = 5, col = 4)
legend("bottomleft", legend = c("Alphaproteobacteria", expression(paste(plain(r)^{paste("2")}, 
                                                                paste("="), paste("4.412e-05")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Deltaproteobacteria #####
plot(x3$Depth ~ x3$D_0__Bacteria.D_1__Proteobacteria.D_2__Deltaproteobacteria, type = "n", 
     main = "Deltaproteobacteria vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 500, 1000, 1500), h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x3$Depth ~ x3$D_0__Bacteria.D_1__Proteobacteria.D_2__Deltaproteobacteria)
summary(lm(x3$D_0__Bacteria.D_1__Proteobacteria.D_2__Deltaproteobacteria ~ x3$Depth))
abline(-654.46093/0.02735, 1/0.02735, lty = 5, col = 4)
legend("bottomleft", legend = c("Deltaproteobacteria", expression(paste(plain(r)^{paste("2")}, 
                                                                        paste("="), paste("0.009452")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Gammaproteobacteria #####
plot(x3$Depth ~ x3$D_0__Bacteria.D_1__Proteobacteria.D_2__Gammaproteobacteria, type = "n", 
     main = "Gammaproteobacteria vs Depth",
     xlab = "OTU", ylab = "Depth", 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 1000, 1500, 2000, 2500, 3000, 3500), h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(x3$Depth ~ x3$D_0__Bacteria.D_1__Proteobacteria.D_2__Gammaproteobacteria)
summary(lm(x3$D_0__Bacteria.D_1__Proteobacteria.D_2__Gammaproteobacteria ~ x3$Depth))
abline(-1795.5335/0.1272, 1/0.1272, lty = 5, col = 4)
legend("bottomleft", legend = c("Gammaproteobacteria", expression(paste(plain(r)^{paste("2")}, 
                                                                        paste("="), paste("0.04964")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Heatmap #####
rownames(OTU3) <- OTU3$Depth
attach(OTU3)
OTU3 <- OTU3[order(Depth),]
detach(OTU3)

rownames(OTU2) <- OTU2$Depth

heatOTU <- cbind(OTU2[,2:40], 
              OTU3$D_0__Bacteria.D_1__Proteobacteria.D_2__Alphaproteobacteria, 
              OTU3$D_0__Bacteria.D_1__Proteobacteria.D_2__Deltaproteobacteria, 
              OTU3$D_0__Bacteria.D_1__Proteobacteria.D_2__Gammaproteobacteria, 
              OTU3$D_0__Bacteria.D_1__Proteobacteria.__) 
colnames(heatOTU) <- c("Crenaarchaeota", "Euryarchaeota", "Nanoarchaeota", 
                       "Thaumarchaeota", "Other Archaea", "Acidobacteria", 
                       "Actinobacteria", "AncK6", "BRC1", "Bacteroidetes", 
                       "Calditrichaeota", "Chlamydiae", "Chloroflexi", 
                       "Cyanobacteria", "Dadabacteria", "Deinococcus", 
                       "Dependentiae", "Epsilonbacteraeota", "Firmicutes", 
                       "Gemmatimonadetes", "Hydrogenedentes", 
                       "Kiritimatiellaeota", "Lentisphaerae", 
                       "Margulisbacteria", "Marinimicrobia", "Nitrospinae", 
                       "Nitrospirae", "PAUC34f", "Patescibacteria", 
                       "Planctomycetes", "Poribacteria", "Proteobacteria", 
                       "Schekmanbacteria", "Spirochaetes", "Verrucomicrobia", 
                       "WPS.2", "Other Bacteria", "Eukaryota", "Unassigned", 
                       "Alphaproteobacteria", "Deltaproteobacteria", 
                       "Gammaproteobacteria", "Other Proteobacteria")
heatOTU.m <- as.matrix(heatOTU[,-32])
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(9, "Reds"))(25)
library(gplots)
heatmap.2(heatOTU.m, scale = "none", trace = "none", 
          col = coul, 
          key.title = NA, key.xlab = "OTU #", density.info = "none", 
          main = "OTU Phylum Level", 
          xlab = "Species", ylab = "Depth (m)", 
          margins = c(10,5), adjCol = c(NA,0.5), 
          dendrogram = "none", Rowv = FALSE, Colv = FALSE)

#### Heatmap Top ?? Class (Level 4) ######
OTU4 <- read.csv("./OTU tables/level-4.csv")

for(i in 1:309) {
    OTU4[20,i] = sum(OTU4[1:19,i])
}

rownames(OTU4) <- OTU4$Depth

attach(OTU4)
OTU4 <- OTU4[order(Depth),]
detach(OTU4)

OTU4t <- t(OTU4)
OTU4t <- as.data.frame(OTU4t)

colnames(OTU4t) <- c(200, 400, 600, 1000, 1400, 2000, 2200, 2400, 2600, 
                     2800, 3000, 3200, 3400, 3600, 3800, 4000, 4200, 4400, 
                     4530, "Sum")

attach(OTU4t)
OTU4t <- OTU4t[order(Sum, decreasing = TRUE),]
detach(OTU4t)

OTU4t <- OTU4t[-2,]
OTU4t <- OTU4t[-4,]
OTU4t <- OTU4t[-4,]

heatOTU4 <- OTU4t[1:50,]

heatOTU4 <- t(heatOTU4)
heatOTU4 <- as.data.frame(heatOTU4)
heatOTU4 <- heatOTU4[-20,]

colnames(heatOTU4) <- c("")

heatOTU4.m <- as.matrix(heatOTU4)

library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(9, "Reds"))(25)
library(gplots)
heatmap.2(heatOTU4.m, scale = "none", trace = "none", 
          col = coul, 
          key.title = NA, key.xlab = "OTU #", density.info = "none", 
          main = "Top OTU Class", 
          xlab = "Species", ylab = "Depth (m)", 
          margins = c(15,5), adjCol = c(NA,0.5), 
          dendrogram = "none", Rowv = FALSE, Colv = FALSE)
