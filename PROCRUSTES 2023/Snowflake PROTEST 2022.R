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

PROSnw.tax <- Snw22[-12,-c(3:5)]
PROSnw.cwm <- SnwCWM22.rm[,-c(3:5)]
snw.years <- Snw22$Year[-12]

protest.snw <- protest(PROSnw.tax, PROSnw.cwm)
print(protest.snw, digits = 6)
summary(protest.snw)


graphics.off()
par(pty="s")

snw.p1.figure <- plot(protest.snw, kind = 1, main = "Snowflake Lake Procrustes Superimposition", lwd = 1.5, cex = 1.5, cex.axis = 1.2, cex.lab = 1.2)


snw.p2.figure <- plot(protest.snw, kind = 2, type = "points", main = "Snowflake Lake Procrustes Residuals", lwd = 10, xaxt = 'n', ylim=c(0.011,0.3), cex.axis = 1.2, xlab = "Year", cex.lab = 1.2)
axis(side = 1,labels = snw.years, at = c(1:33), cex.axis = 1.2, las = 2)



Pro.snw.res <- data.frame(Residuals = residuals(protest.snw))

