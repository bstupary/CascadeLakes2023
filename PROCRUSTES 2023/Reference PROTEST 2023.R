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

PROref.tax <- Cntl22[,-c(3:5)]
PROref.cwm <- CWM.Sites22.controls.rm[,-c(3,4)]
ref.years <- Cntl22$Year

protest.Ref <- protest(PROref.tax, PROref.cwm)
print(protest.Ref, digits = 6)

graphics.off()

par(pty="s")

ref.p1.figure <- plot(protest.Ref, kind = 1, main = "ref Lake Procrustes Superimposition", lwd = 1.5, cex = 1.5, cex.axis = 1.2, cex.lab = 1.2)


ref.p2.figure<- plot(protest.Ref, kind = 2, type = "points", main = "ref Lake Procrustes Residuals", lwd = 10, xaxt = 'n', cex.axis = 1.2, xlab = "Year", cex.lab = 1.2)
axis(side = 1,labels = ref.years, at = c(1:43), cex.axis = 1.2, las = 2)



Pro.Ref.res <- data.frame(Residuals = residuals(protest.Ref))
