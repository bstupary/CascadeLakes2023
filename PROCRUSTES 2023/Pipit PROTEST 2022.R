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

PROpipit.tax <- Pip22[,-c(3:5)]
PROpipit.cwm <- PipCWM22.rm[,-c(3:5)]
Pipit.years <- Pip22$Year

protest.pip <- protest(PROpipit.tax, PROpipit.cwm)
print(protest.pip, digits = 6)

graphics.off()

par(pty="s")

Pipit.p1.figure <- plot(protest.pip, kind = 1, main = "Pipit Lake Procrustes Superimposition", lwd = 1.5, cex = 1.5, cex.axis = 1.2, cex.lab = 1.2)


Pipit.p2.figure<- plot(protest.pip, kind = 2, type = "points", main = "Pipit Lake Procrustes Residuals", lwd = 10, xaxt = 'n', cex.axis = 1.2, xlab = "Year", cex.lab = 1.2)
axis(side = 1,labels = Pipit.years, at = c(1:28), cex.axis = 1.2, las = 2)



Pro.Pip.res <- data.frame(Residuals = residuals(protest.pip))
