##### Install / Load Packages #####

library(vegan)
library(ggplot2)
library(ggalt)
library(ggforce)
library(ggrepel)
library(dplyr)
library(tidyr)
library(ggsci)


#Set Working Directory
setwd("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/BIOMASS 2022")


######## Load in your datasets ###############
Casc.Ref.Bm.22 <- read.csv("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/BIOMASS 2022/Casc Reference Biomass 2022.csv", stringsAsFactors=TRUE)



View(Casc.Ref.Bm.22)

Casc.Ref.Bm.22$Lake <- as.factor(Casc.Ref.Bm.22$Lake)
Casc.Ref.Bm.22$Year <- as.factor(Casc.Ref.Bm.22$Year)




str(Casc.Ref.Bm.22)



############ Visualize Bar Plot of Ref Lakes Mean Annual Biomass ##################




Casc.Ref.Fig <- ggplot(Casc.Ref.Bm.22,
                         aes(x = Year,
                             y = Yr.mass,
                             fill = Lake,
                             color = Lake)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill = "transparent", colour = NA),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    strip.clip = "off",
    legend.title.align = 0.5
  ) +
  labs(y = "Mean Annual Biomass (µg/L)",
       fill = "Reference\nLake",
       color = "Reference\nLake") +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  facet_grid(rows = vars(Lake),
             scales = "free")



Casc.Ref.Fig


#######


############## Visualize Ref Lakes Biomass of just D.midd, H.arct ##########

#we want proper names when ploting facets of the taxa, not abreviations

Casc.Ref.Bm.22.D_H <- Casc.Ref.Bm.22[,-5]

Casc.Ref.Bm.22.D_H <- Casc.Ref.Bm.22.D_H %>%
  pivot_longer(!c(Lake,Year), names_to = "Taxa", values_to = "Biomass")

str(Casc.Ref.Bm.22.D_H)


Casc.Ref.Bm.22.D_H$Taxa <- as.factor(Casc.Ref.Bm.22.D_H$Taxa) #make it a factor so it has levels

Casc.Ref.Bm.22.D_H$Year <- as.factor(Casc.Ref.Bm.22.D_H$Year)

levels(Casc.Ref.Bm.22.D_H$Taxa) <- c("Daphnia middendorffiana", "Hesperodiaptomus arcticus") #changes the values of the factor to new vector of strings (order is important for renaming)

Casc.Ref.d_h <- ggplot(data = Casc.Ref.Bm.22.D_H,
                   aes(x = Year,
                       y = Biomass,
                       color = Lake
                   )) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill = "transparent", colour = NA),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    strip.clip = "off",
    legend.title.align = 0.5
  ) +
  labs(y = "Mean Annual Biomass (µg/L)") +
  geom_line(aes(group = Lake),
    linewidth = 1.1) +
  geom_smooth(color = "black",
              fill = "#1b9e77",
              linetype = 5,
              alpha = 0.15)+
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0)+
  facet_grid(cols = vars(Taxa),
             rows = vars(Lake))

Casc.Ref.d_h

################


