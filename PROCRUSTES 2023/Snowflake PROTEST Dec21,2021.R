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

PROSnw.tax <- Snw20.b[,-c(3,4)]
PROSnw.cwm <- SnwCWM20.b[,-c(3,4)]
snw.years <- Snw20.b$Year

protest.snw <- protest(PROSnw.tax, PROSnw.cwm)
print(protest.snw, digits = 6)
summary(protest.snw)


graphics.off()
par(pty="s")

snw.p1.figure <- plot(protest.snw, kind = 1, main = "Snowflake Lake Procrustes Superimposition", lwd = 1.5, cex = 1.5, cex.axis = 1.2, cex.lab = 1.2)


snw.p2.figure <- plot(protest.snw, kind = 2, type = "points", main = "Snowflake Lake Procrustes Residuals", lwd = 10, xaxt = 'n', ylim=c(0.011,0.3), cex.axis = 1.2, xlab = "Year", cex.lab = 1.2)
axis(side = 1,labels = snw.years, at = c(1:32), cex.axis = 1.2, las = 2)



Pro.snw.res <- as.data.frame(residuals(protest.snw))

