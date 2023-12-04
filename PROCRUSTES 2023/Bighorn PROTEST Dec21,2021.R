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



############# Load in workspaces from other projects ###############

load("C:/Users/Blake Stuparyk/Desktop/New Control Tinkering Jan 17, 2020/NMDS Feb 2020/NMDS Dec 21,2021.RData")
load("C:/Users/Blake Stuparyk/Desktop/New Control Tinkering Jan 17, 2020/CWMs May 2020/Dec21 2021/CWM Dec21,2021.RData")

setwd("C:/Users/Blake Stuparyk/Desktop/New Control Tinkering Jan 17, 2020/Proscrutes Nov5,2021")

#############

PRObig.tax <- Big22[,-c(3:5)]
PRObig.cwm <- BigCWM22[,-c(3:5)]
big.years <- Big22$Year

protest.big <- protest(PRObig.tax, PRObig.cwm)
print(protest.big, digits = 6)
summary(protest.big)


graphics.off()
par(pty="s")

big.p1.figure <- plot(protest.big, kind = 1, main = "Bighorn Lake Procrustes Superimposition", lwd = 1.5, cex = 1.5, cex.axis = 1.2, cex.lab = 1.2)


big.p2.figure <- plot(protest.big, kind = 2, type = "points", main = "Bighorn Lake Procrustes Residuals", lwd = 10, xaxt = 'n', cex.axis = 1.2, xlab = "Year", cex.lab = 1.2 )
axis(side = 1,labels = big.years, at = c(1:23), cex.axis = 1.2, las = 2)


Pro.big.res <- as.data.frame(residuals(protest.big))

