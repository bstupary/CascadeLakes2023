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

load("C:/Users/Blake Stuparyk/Desktop/CFFR Feb 24th/Feb 16/Abnd Vis Feb16/Feb 16 - Trimmed DCA and NMDS.RData")

load("C:/Users/Blake Stuparyk/Desktop/CFFR Feb 24th/Feb 16/Feb 16 2022 - CWM-traits/Feb 16- NSR20 CWMs.RData")


setwd("C:/Users/Blake Stuparyk/Desktop/CFFR Feb 24th/Feb 16/PROCRUSTES")

#############
NSR20.tax.nmds <- NSR20.NMDS.Sitescores14[,-c(3,4)]
NSR20.cwm.nmds <- CWM.Sites20[,-c(3,4)]

site.names <- row.names(NSR20.NMDS.Sitescores14)


protest <- protest(NSR20.tax.nmds,
                   NSR20.cwm.nmds)

print(protest, digits = 6)
summary(protest)


graphics.off()
par(pty="s")

cntrl.p1.figure <- plot(protest, kind = 1, main = "NSR20 Procrustes Superimposition", lwd = 1.5, cex = 1.5, cex.axis = 1.2, cex.lab = 1.2)


cntrl.p2.figure <- plot(protest, kind = 2, type = "points", main = "NSR20 Procrustes Residuals", lwd = 10, ylim=c(0.011,0.32), xaxt = 'n', cex.axis = 1.2, xlab = "Year", cex.lab = 1.2)
axis(side = 1,labels = site.names, at = c(1:42), cex.axis = 1.1, las = 2)



