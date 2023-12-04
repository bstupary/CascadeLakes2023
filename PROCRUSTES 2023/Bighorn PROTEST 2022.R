#### Packages ####
library(vegan)
library(ggplot2)
library(ggalt)
library(ggforce)
library(ggrepel)
library(ggpmisc)
library(cowplot)
library(sciplot)
library(ggsci)



##### Set WD ########

setwd("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/PROCRUSTES 2022")

##### Load workspace from WD ########

load("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/PROCRUSTES 2022/PROtest Oct5, 2022.RData")

############# OR Load in workspaces from other projects ###############

load("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/Abundance NMDS/.RData")
load("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/CWMs NMDS 2022/CWM Oct5,2022.RData")


#############

PRObig.tax <- Big22[,-c(3:5)]
PRObig.cwm <- BigCWM22.rm[,-c(3:5)]
big.years <- Big22$Year

protest.big <- protest(PRObig.tax, PRObig.cwm)
print(protest.big, digits = 6)
summary(protest.big)


graphics.off()
par(pty="s")

big.p1.figure <- plot(protest.big, kind = 1, main = "Bighorn Lake Procrustes Superimposition", lwd = 1.5, cex = 1.5, cex.axis = 1.2, cex.lab = 1.2)


big.p2.figure <- plot(protest.big, kind = 2, type = "points", main = "Bighorn Lake Procrustes Residuals", lwd = 10, xaxt = 'n', cex.axis = 1.2, xlab = "Year", cex.lab = 1.2 )
axis(side = 1,labels = big.years, at = c(1:23), cex.axis = 1.2, las = 2)


Pro.big.res <- data.frame(Residuals = residuals(protest.big))

