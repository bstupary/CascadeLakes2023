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
library(grid)



############# Load in workspaces from other projects ###############

load("C:/Users/Blake Stuparyk/Desktop/CFFR Feb 24th/Feb 16/Abnd Vis Feb16/Feb 16 - Trimmed DCA and NMDS.RData")

load("C:/Users/Blake Stuparyk/Desktop/CFFR Feb 24th/Feb 16/Feb 16 2022 - CWM-traits/Feb 16- NSR20 CWMs.RData")




setwd("C:/Users/Blake Stuparyk/Desktop/CFFR Feb 24th/Feb 16/PROCRUSTES")

nsr.meta16 <- read.csv("C:/Users/Blake Stuparyk/Desktop/CFFR Feb 24th/Feb 16/PROCRUSTES/NSR20 Site Meta - Feb14, 2022.csv", stringsAsFactors=TRUE)
View(nsr.meta16)


ecoregion <- c("Dry Mixedwood" = "red", "Central Parkland" = "orange", "Central Mixedwood" = "blue", "Foothills" = "darkgreen", "Mixed Alpine" = "darkorchid1")

eco.shapes <- c("Dry Mixedwood" = "red", "Central Parkland" = "orange", "Central Mixedwood" = "blue", "Foothills" = "darkgreen", "Mixed Alpine" = "darkorchid1")

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


cntrl.p1.figure

cntrl.p2.figure <- plot(protest, kind = 2, type = "points", main = "NSR20 Procrustes Residuals", lwd = 10, ylim=c(0.011,0.32), xaxt = 'n', cex.axis = 1.2, xlab = "Year", cex.lab = 1.2)
axis(side = 1,labels = site.names, at = c(1:42), cex.axis = 1.1, las = 2)



resids <- as.data.frame(residuals(protest))
colnames(resids)[1] <- "ProRes"

resids$site <- nsr.meta16$Site
resids$Ecoregion <- nsr.meta16$Ecoregion


######### Back on my bullshit ##############

ctest <- data.frame(NMDS1 = protest$Yrot[,1],
                    NMDS2 = protest$Yrot[,2],
                    xNMDS1 = protest$X[,1],
                    xNMDS2 = protest$X[,2],
                    Ecoregion = nsr.meta16[,2])

write.csv(ctest, "ctest.csv")

ctest2 <- data.frame(NMDS1 = protest$Yrot[,1],
                    NMDS2 = protest$Yrot[,2],
                    xNMDS1 = protest$X[,1],
                    xNMDS2 = protest$X[,2])

View(ctest)

# Visualize?

pro.test.super <- ggplot(data = ctest) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 15,
                        face = "bold"),
    axis.text = element_text(color = "black"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.99,0.25),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6,6,6,6),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  labs(x = "Dimension 1",
       y = "Dimention 2") +
  geom_point(aes(x = NMDS1,
                 y = NMDS2,
                 fill = Ecoregion),
             shape = 21,
             size = 5) +
  geom_point(aes(x = xNMDS1,
                 y = xNMDS2,
                 fill = Ecoregion),
             shape = 22,
             size = 5)+
  geom_segment(aes(x = NMDS1,
                   y = NMDS2,
                   xend = xNMDS1,
                   yend = xNMDS2,
                   color = Ecoregion),
               arrow = arrow(length = unit(0.25, "cm")),
               size = 0.8) +
  coord_fixed() +
  scale_fill_d3("category20") +
  scale_color_d3("category20")

pro.test.super







# Faceted?


pro.test.super.facet <- ggplot(data = ctest) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 15,
                        face = "bold"),
    axis.text = element_text(color = "black"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6,6,6,6),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  labs(x = "Dimension 1",
       y = "Dimention 2") +
  geom_point(aes(x = NMDS1,
                 y = NMDS2,
                 shape = 21),
             shape = 21,
             fill = "grey",
             show.legend = TRUE,
             size = 5) +
  geom_point(aes(x = xNMDS1,
                 y = xNMDS2,
                 shape = 22),
             fill = "grey",
             shape = 22,
             show.legend = TRUE,
             size = 5) +
    geom_segment(aes(x = NMDS1,
                   y = NMDS2,
                   xend = xNMDS1,
                   yend = xNMDS2),
               arrow = arrow(length = unit(0.4, "cm")),
               color  = "black",
               size = 0.8) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  coord_fixed() +
  scale_fill_d3("category20") +
  scale_color_d3("category20") +
  facet_wrap(vars(Ecoregion))

pro.test.super.facet


### Residuals Plot



pro.resid.gg <- ggplot(data = resids,
                       aes(y = ProRes,
                           x = as.factor(ORDER))) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 15,
                        face = "bold"),
    axis.text = element_text(color = "black"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6,6,6,6),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  labs(y = "Procrustes residuals",
       x = "Sites",
       color = "Ecoregion",
       fill = "Ecoregion",
       shape = "Ecoregion") +
  geom_hline(yintercept = 0.155, linetype = 2, size = 1.1) +
  geom_hline(yintercept = 0.09, linetype = 2, size = 1.1) +
  geom_hline(yintercept = 0.123, size = 1.1) +
  geom_point(aes(color = Ecoregion,
                 shape = Ecoregion),
            # shape = 18,
             size = 5) +
  geom_segment(aes(x = ORDER,
                   xend = ORDER,
                   y = 0,
                   yend = ProRes,
                   color = Ecoregion),
               linetype = 1,
               size = 0.8) +
  scale_fill_d3("category20") +
  scale_color_d3("category20") +
  scale_x_discrete(labels = resids$site,
                   expand = expansion(mult = c(0.02,0.02))
                   ) +
  scale_shape_manual(values = c(16,17,15,18,6),
                     labels = c("Dry Mixedwood", "Central Parkland", "Central Mixedwood", "Foothills", "Mixed Alpine"))+
  scale_y_continuous(n.breaks = 6, expand = c(0,0.008)) +
  coord_flip()


pro.resid.gg



