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

PROhar.tax <- Har22[,-c(3:5)]
PROhar.cwm <- HarCWM22[-c(3:5)]
har.years <- Har22$Year

protest.har <- protest(PROhar.tax, PROhar.cwm)
print(protest.har, digits = 6)
summary(protest.har)


graphics.off()
par(pty="s")

har.p1.figure <- plot(protest.har, kind = 1, main = "Harrison Lake Procrustes Superimposition", lwd = 1.5, cex = 1.5, cex.axis = 1.2, cex.lab = 1.2)


har.p2.figure <- plot(protest.har, kind = 2, type = "points", main = "Harrison Lake Procrustes Residuals", lwd = 10, xaxt = 'n', cex.axis = 1.2, xlab = "Year", cex.lab = 1.2 )
axis(side = 1,labels = har.years, at = c(1:17), cex.axis = 1.2, las = 2)


Pro.har.res <- as.data.frame(residuals(protest.har))

