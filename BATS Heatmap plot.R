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
