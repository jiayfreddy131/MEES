#Clean up tables and plot rel abun depth profile

##### test #####
# read the table
test1 <- read.delim("./OTU tables/rel_phyla_table_1 copy.tsv", header = T, sep = "\t")
# add the depth to the table
test <- rbind(test1, c("Depth", 1000, 1400, 200, 2000, 2200, 400, 600, 3200, 
              3400, 3600, 3800, 4000, 2400, 4200, 
              4400, 4530, 2600, 2800, 3000)) 
    #this is not gonna work if there are too many variable values
# transpose the table
test5t <- t(test)
test5 <- as.data.frame(test5t)
# change header names
names(test5) <- c("Archaea", "Bacteria", "Eukaryota", "Unassigned", "Depth")
# delete first row
test555 <- test5[-1,]

# tried R plot funcion, showed density instead of scatterplot 
# because the % numbers were read as factors
plot(test555555$Depth ~ test555555$Archaea, main = "Archaea vs Depth", 
     xlab = "OTU", ylab = "Depth", ylim = c(4600, 0), las = 1)
# need to change variables to numeric

# change the factor rel abun values to numeric (column by column)
test555555$Archaea <- as.numeric(as.character(test555555$Archaea))
test555555$Bacteria <- as.numeric(as.character(test555555$Bacteria))
test555555$Depth <- as.numeric(as.character(test555555$Depth))
# not sure why, but applying as.numeric directly, the values are all wrong
# so the values need to be changed as.character first, then as.numeric

# plot for Bacteria vs Depth
library(ggplot2)
ggplot(test555555, aes(x = Bacteria, y = Depth)) +
    geom_point() + #scatterplot
    xlab("OTU") +
    ylab("Depth") +
    xlim(0, 1) +
    ylim(4600, 0) + #invert depth values
    theme_bw()

# faster way to change variable types, so that the whole data frame can be changed
# without maunally changing each column
test.chr <- data.frame(lapply(test555555, as.character), 
                       stringsAsFactors = FALSE)
test.numeric <- data.frame(lapply(test.chr, as.numeric), 
                           stringsAsFactors = FALSE)

# plot test for test.numeric vs Depth
ggplot(test.numeric, aes(x = Unassigned, y = Depth)) +
    geom_point() +
    xlab("OTU") +
    ylab("Depth") +
    xlim(0, 1) +
    ylim(4600, 0) +
    theme_bw()
# great, this works, but the whole steps need a lot of tweaking


# the manual way of changing header names can only work in level 1 with 5 variables, level 2 has 44
# so we need to figure out how to copy the characters in the first row to be the header names
# method 1, didn't work
library(dplyr)
header.true <- function(test5) {
    names(test5) <- as.character(unlist(test5[1,]))
    test5[-1,]
}
# method 2, this works, but because "Depth" were read as NA, we don't have "Depth" as header name
test6 <- data.frame(lapply(test5, as.character), stringsAsFactors = FALSE)
colnames(test6) <- test6[1,]

# so if we want "Depth" to be header name, we need to change data types before
# rbind the Depth vector
test2 <- data.frame(lapply(test, as.character), stringsAsFactors = FALSE)
test2 <- data.frame(lapply(test2, as.numeric), stringsAsFactors = FALSE) 
    # so NA was introduced when trying to change to numeric
# let's try adding data$Depth column instead of rbind
# so first transpose, then use data$Depth = c() to add depth column
test3 <- t(rel1)
test3 <- as.data.frame(test3) 
test4 <- t(rel1)
test4 <- as.data.frame(test4) 
# got two of the same data frame, but we're only going to as.character test3
test3 <- data.frame(lapply(test3, as.character), stringsAsFactors = FALSE)
test3$Depth = c("Depth", 1000, 1400, 200, 2000, 2200, 400, 600, 3200, 
                3400, 3600, 3800, 4000, 2400, 4200, 
                4400, 4530, 2600, 2800, 3000)
test4$Depth = c("Depth", 1000, 1400, 200, 2000, 2200, 400, 600, 3200, 
                3400, 3600, 3800, 4000, 2400, 4200, 
                4400, 4530, 2600, 2800, 3000)
# Both works, so adding column is better because it will register "Depth" (instead of NA) whether if we change the data type
colnames(test4) <- test4[1,] # header names will be nonsense if we don't change the data frame as.character
colnames(test3) <- test3[1,] # header shows the same as first line
test3 = test3[-1,]
test3 <- data.frame(lapply(test3, as.numeric), stringsAsFactors = FALSE)
# So this is the simplest way to clean up data table
# 1) transpose 2) as.character 3)data$Depth add column 4) copy first row to header
# 5) delete first row 6) as.numeric

####### Template ########
# How to clean up rel abun table ("x.tsv")extracted from qiime2 and plot with other variables
# ggplot and R plot both works after changing variable types, so it all depends
# on what you want the graph to look like

#STEP 1: *****Delete the first line of x.tsv******
#STEP 1.5: read the table
x <- read.delim("x.tsv", #tsv file from qiime2
                header = T, 
                sep = "\t") #\t is tab, because you're using tab separated value (tsv) file
#STEP 2: transpose the table, so that the we can plot more easily (by column)
x <- t(x)
x <- as.data.frame(x)

#STEP 3: change the values to character for the whole data frame, so we can copy first row to header
x <- data.frame(lapply(x, as.character), stringsAsFactors = FALSE)

#STEP 4: add the depth to the table, or any other metadata list
        # note that for different variables the method may be different
x$Depth = c("Depth", 1000, 1400, 200, 2000, 2200, 400, 600, 3200, 
                       3400, 3600, 3800, 4000, 2400, 4200, 
                       4400, 4530, 2600, 2800, 3000)

#STEP 5: change header names
colnames(x) <- x[1,]
# delete first row
x <- x[-1,]

#STEP 6: change the values to numeric for the whole data frame, so that we can make scatterplot
x <- data.frame(lapply(x, as.numeric), stringsAsFactors = FALSE)
    # we can't change the values to numeric directly from original file
    # because the values will not be correct

#STEP 7: Plot rel abun vs Variable (Depth)
ggplot(x, aes(x = D_0__Bacteria, y = Depth)) +
    geom_point() +
    xlab("OTU") +
    ylab("Depth") +
    xlim(0, 1) +
    ylim(4600, 0) +
    theme_bw()
# a little issue here is that the x-axis name is hard to type manually
# but we can use the x$ to review header names and delete "x$" after selecting the taxon

# we can add properties to the plot using different ggplot functions

###### Level 2 ######
# delete first line of "rel_phyla_table_2.tsv"
rel2 <- read.delim("OTU tables/rel_phyla_table_2.tsv", header = T, sep = "\t")

rel2 <- t(rel2)
rel2 <- as.data.frame(rel2)

rel2 <- data.frame(lapply(rel2, as.character), stringsAsFactors = FALSE) 

rel2$Depth = c("Depth", 1000, 1400, 200, 2000, 2200, 400, 600, 3200, 
            3400, 3600, 3800, 4000, 2400, 4200, 
            4400, 4530, 2600, 2800, 3000)

colnames(rel2) <- rel2[1,]
rel2 <- rel2[-1,]

rel2 <- data.frame(lapply(rel2, as.numeric), stringsAsFactors = FALSE)

#### Cyanobacteria ######
library(ggplot2)
ggplot(rel2, aes(x = D_0__Bacteria.D_1__Cyanobacteria, y = Depth)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ggtitle("Cyanobacteria vs Depth") +
    xlab("Relative Abundance") +
    ylab("Depth(m)") +
    xlim(0, 0.25) +
    ylim(4600, 0) +
    theme_bw()
# the smooth line is y~x, but we want x~y, which ggplot doesn't allow, so back to R plot

##### Euryarchaeota ######
plot(rel2$Depth ~ rel2$D_0__Archaea.D_1__Euryarchaeota, type = "n", 
     main = "Euryarchaeota vs Depth",
     xlab = "Relative Abundance", ylab = "Depth", 
     xlim = c(0, 0.25), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Archaea.D_1__Euryarchaeota)
summary(lm(rel2$D_0__Archaea.D_1__Euryarchaeota ~ rel2$Depth))
abline(-2.200e-02/4.842e-06, 1/4.842e-06, lty = 5, col = 4)
legend("bottomright", legend = c("Euryarchaeota", expression(paste(plain(r)^{paste("2")}, 
                                                                           paste("="), paste("0.1679")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Thaumarchaeota ######
plot(rel2$Depth ~ rel2$D_0__Archaea.D_1__Thaumarchaeota, type = "n", 
     main = "Thaumarchaeota vs Depth",
     xlab = "Relative Abundance", ylab = "Depth", 
     xlim = c(0, 0.25), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Archaea.D_1__Thaumarchaeota)
summary(lm(rel2$D_0__Archaea.D_1__Thaumarchaeota ~ rel2$Depth))
abline(1.658e-01/1.575e-05, -1/1.575e-05, lty = 5, col = 4)
legend("bottomright", legend = c("Thaumarchaeota", expression(paste(plain(r)^{paste("2")}, 
                                                                   paste("="), paste("0.1505")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Acidobacteria ######
plot(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Acidobacteria, type = "n", 
     main = "Acidobacteria vs Depth",
     xlab = "Relative Abundance", ylab = "Depth (m)", 
     xlim = c(0, 0.25), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Acidobacteria)
summary(lm(rel2$D_0__Bacteria.D_1__Acidobacteria ~ rel2$Depth))
abline(-3.829e-02/4.656e-06, 1/4.656e-06, lty = 5, col = 4)
legend("bottomright", legend = c("Acidobacteria", expression(paste(plain(r)^{paste("2")}, 
                                                                    paste("="), paste("0.0951")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

###### Actinobacteria ######
plot(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Actinobacteria, type = "n", 
     main = "Actinobacteria vs Depth",
     xlab = "Relative Abundance", ylab = "Depth (m)", 
     xlim = c(0, 0.25), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Actinobacteria)
summary(lm(rel2$D_0__Bacteria.D_1__Actinobacteria ~ rel2$Depth))
abline(1.346e-01/3.598e-05, -1/3.598e-05, lty = 5, col = 4)
legend("bottomright", legend = c("Actinobacteria", expression(paste(plain(r)^{paste("2")}, 
                                                                   paste("="), paste("0.2272")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Bacteroidetes ######
plot(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Bacteroidetes, type = "n", 
     main = "Bacteroidetes vs Depth",
     xlab = "Relative Abundance", ylab = "Depth (m)", 
     xlim = c(0, 0.25), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Bacteroidetes)
summary(lm(rel2$D_0__Bacteria.D_1__Bacteroidetes ~ rel2$Depth))
abline(-6.546e-03/6.817e-06, 1/6.817e-06, lty = 5, col = 4)
legend("bottomright", legend = c("Bacteroidetes", expression(paste(plain(r)^{paste("2")}, 
                                                                    paste("="), paste("0.3616")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

###### Chloroflexi ######
plot(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Chloroflexi, type = "n", 
     main = "Chloroflexi vs Depth",
     xlab = "Relative Abundance", ylab = "Depth (m)", 
     xlim = c(0, 0.25), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Chloroflexi)
summary(lm(rel2$D_0__Bacteria.D_1__Chloroflexi ~ rel2$Depth))
abline(-8.313e-02/1.857e-05, 1/1.857e-05, lty = 5, col = 4)
legend("bottomright", legend = c("Chloroflexi", expression(paste(plain(r)^{paste("2")}, 
                                                                   paste("="), paste("0.4012")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

####### Firmicutes ######
plot(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Firmicutes, type = "n", 
     main = "Firmicutes vs Depth",
     xlab = "Relative Abundance", ylab = "Depth (m)", 
     xlim = c(0, 0.25), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Firmicutes)
summary(lm(rel2$D_0__Bacteria.D_1__Firmicutes ~ rel2$Depth))
abline(5.722e-02/1.540e-05, -1/1.540e-05, lty = 5, col = 4)
legend("bottomright", legend = c("Firmicutes", expression(paste(plain(r)^{paste("2")}, 
                                                                 paste("="), paste("0.2762")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

###### Marinimicrobia SAR406 clade ######
plot(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Marinimicrobia..SAR406.clade., type = "n", 
     main = "Marinimicrobia SAR406 vs Depth",
     xlab = "Relative Abundance", ylab = "Depth", 
     xlim = c(0, 0.25), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Marinimicrobia..SAR406.clade.)
summary(lm(rel2$D_0__Bacteria.D_1__Marinimicrobia..SAR406.clade. ~ rel2$Depth))
abline(-4.733e-02/3.462e-07, 1/3.462e-07, lty = 5, col = 4)
legend("bottomright", legend = c("Marinimicrobia SAR406", expression(paste(plain(r)^{paste("2")}, 
                                                                  paste("="), paste("0.0004677")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

##### Nitrospinae #####
plot(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Nitrospinae, type = "n", 
     main = "Nitrospinae vs Depth",
     xlab = "Relative Abundance", ylab = "Depth", 
     xlim = c(0, 0.25), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Nitrospinae)
summary(lm(rel2$D_0__Bacteria.D_1__Nitrospinae ~ rel2$Depth))
abline(-8.998e-03/2.930e-06, 1/2.930e-06, lty = 5, col = 4)
legend("bottomright", legend = c("Nitrospinae", expression(paste(plain(r)^{paste("2")}, 
                                                                           paste("="), paste("0.2013")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

###### Planctomycetes #####
plot(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Planctomycetes, type = "n", 
     main = "Planctomycetes vs Depth",
     xlab = "Relative Abundance", ylab = "Depth", 
     xlim = c(0, 0.25), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Planctomycetes)
summary(lm(rel2$D_0__Bacteria.D_1__Planctomycetes ~ rel2$Depth))
abline(-2.629e-02/1.569e-05, 1/1.569e-05, lty = 5, col = 4)
legend("bottomright", legend = c("Planctomycetes", expression(paste(plain(r)^{paste("2")}, 
                                                                 paste("="), paste("0.5932")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

###### Proteobacteria #####
plot(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Proteobacteria, type = "n", 
     main = "Proteobacteria vs Depth",
     xlab = "Relative Abundance", ylab = "Depth", 
     xlim = c(0, 0.5), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Proteobacteria)
summary(lm(rel2$D_0__Bacteria.D_1__Proteobacteria ~ rel2$Depth))
abline(3.564e-01/8.346e-06, -1/8.346e-06, lty = 5, col = 4)
legend("bottomright", legend = c("Proteobacteria", expression(paste(plain(r)^{paste("2")}, 
                                                                    paste("="), paste("0.1443")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

###### Verrucomicrobia #####
plot(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Verrucomicrobia, type = "n", 
     main = "Verrucomicrobia vs Depth",
     xlab = "Relative Abundance", ylab = "Depth", 
     xlim = c(0, 0.25), 
     ylim = c(4600, 0), las = 1)
abline(v = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), 
       h = c(0, 1000, 2000, 3000, 4000), 
       cex = 1, lty = 2, lwd = 1, col = "lightgray")
points(rel2$Depth ~ rel2$D_0__Bacteria.D_1__Verrucomicrobia)
summary(lm(rel2$D_0__Bacteria.D_1__Verrucomicrobia ~ rel2$Depth))
abline(-1.099e-02/3.939e-06, 1/3.939e-06, lty = 5, col = 4)
legend("bottomright", legend = c("Verrucomicrobia", expression(paste(plain(r)^{paste("2")}, 
                                                                    paste("="), paste("0.2982")))), 
       lty = c(0, 5), col = c(1, 4), pch = c(1, NA), bty = "o", bg = "white")

###### Level 3 ######
# delete first line of "rel_phyla_table_3.tsv"
rel3 <- read.delim("OTU tables/rel_phyla_table_3.tsv", header = T, sep = "\t")

rel3 <- t(rel3)
rel3 <- as.data.frame(rel3)

rel3 <- data.frame(lapply(rel3, as.character), stringsAsFactors = FALSE) 

rel3$Depth = c("Depth", 1000, 1400, 200, 2000, 2200, 400, 600, 3200, 
               3400, 3600, 3800, 4000, 2400, 4200, 
               4400, 4530, 2600, 2800, 3000)

colnames(rel3) <- rel3[1,]
rel3 <- rel3[-1,]

rel3 <- data.frame(lapply(rel3, as.numeric), stringsAsFactors = FALSE)

attach(rel3)
rel3_order <- rel3[order(Depth),]
detach(rel3)

rownames(rel3_order) <- rel3_order$Depth

##### Heatmap test ########
# order table with depth
attach(rel2)
rel2_order <- rel2[order(Depth),]
detach(rel2)
#change rownames to depth for better visualization
rownames(rel2_order) <- rel2_order$Depth
# R heatmap is limited in function
rel2_heat <- as.matrix(rel2_order[1:19, 1:43])
heatmap(rel2_heat, 
        main = "Relative Abundance Phylum Level", 
        xlab = "Species", ylab = "Depth (m)", 
        Rowv = NA, Colv = NA)
# try heatmap.2 in gplots
library(gplots)
heatmap.2(rel2_heat, scale = "none", trace = "none", 
          key.title = "abundance", density.info = "none", 
          main = "Relative Abundance Phylum Level", 
          xlab = "Species", ylab = "Depth (m)", 
          dendrogram = "none")

##### Heatmap Level 2 & Proteobacteria level 3 #####
# create new table for plotting heatmap
heat <- cbind(rel2_order, 
              rel3_order$D_0__Bacteria.D_1__Proteobacteria.D_2__Alphaproteobacteria, 
              rel3_order$D_0__Bacteria.D_1__Proteobacteria.D_2__Deltaproteobacteria, 
              rel3_order$D_0__Bacteria.D_1__Proteobacteria.D_2__Gammaproteobacteria, 
              rel3_order$D_0__Bacteria.D_1__Proteobacteria.__) 
heat <- heat[,-44]
heat.m <- as.matrix(heat[,-35])
heatmap.2(heat.m, scale = "none", trace = "none", 
          density.info = "none", 
          main = "Relative Abundance Phylum Level", 
          xlab = "Species", ylab = "Depth (m)", 
          dendrogram = "none", Rowv = FALSE, Colv = FALSE)
