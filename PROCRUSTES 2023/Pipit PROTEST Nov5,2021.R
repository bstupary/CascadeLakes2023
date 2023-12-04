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

PROpipit.tax <- Pip20[,-c(3:5)]
PROpipit.cwm <- PipCWM20[,-c(3,4)]
Pipit.years <- Pip20$Year

protest.pip <- protest(PROpipit.tax, PROpipit.cwm)
print(protest.pip, digits = 6)

graphics.off()

par(pty="s")

Pipit.p1.figure <- plot(protest.pip, kind = 1, main = "Pipit Lake Procrustes Superimposition", lwd = 1.5, cex = 1.5, cex.axis = 1.2, cex.lab = 1.2)


Pipit.p2.figure<- plot(protest.pip, kind = 2, type = "points", main = "Pipit Lake Procrustes Residuals", lwd = 10, xaxt = 'n', cex.axis = 1.2, xlab = "Year", cex.lab = 1.2)
axis(side = 1,labels = Pipit.years, at = c(1:27), cex.axis = 1.2, las = 2)



Pro.Pip.res <- as.data.frame(residuals(protest.pip))
