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
library(dplyr)
library(tidyverse)


###### Can We Facet The Protest Figs? ##########

XY.distances <- read.csv("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/Abundance NMDS/Sites22.rm with distances.csv", stringsAsFactors=TRUE)

str(XY.distances)
XY.distances$Year <- factor(XY.distances$Year)

XY.distances$Plot.Order <- as.factor(XY.distances$Plot.Order)

Lakes <-c("1" = "Bighorn",
          "2" = "Snowflake",
          "3" = "Pipit",
          "4" = "Harrison",
          "5" = "Eiffel",
          "6" = "Sentinel",
          "7" = "Hungabee",
          "8" = "Oesa",
          "9" = "Opabin")

##### Time for Viz ######
set.seed(42)

XY.distances.start.rm <- drop_na(XY.distances)
options(max.print = 9999999)
print(XY.distances)


raw.dist.facet <- ggplot(data = XY.distances.start.rm,
                        aes(y = Dist.From.Last,
                            x = Year)) +
  theme(
    aspect.ratio = 1,
    text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black", face = "plain"),
    strip.text = element_text(size = 13),
    legend.position = "right",
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(hjust = 0.5),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  labs(x = "Year",
       y = "Raw Distance",
       fill = "Fish\nStatus") +
  geom_col(aes(fill = FishStatus),na.rm = T, color = "black") +
  facet_wrap(vars(Plot.Order),
             labeller = labeller(Plot.Order = Lakes),
             scales = "free_x") +
  scale_x_discrete(na.translate = FALSE)+
  scale_fill_manual(
    values = c(
      "Present" = "red1",
      "Decline" = 'blue1',
      "Absent" = "green1"
    ),
    breaks = c("Present", "Decline", "Absent"),
    labels = c("Present", "Decline", "Absent")
  )


raw.dist.facet


std.dist.facet <- ggplot(data = XY.distances.start.rm,
                         aes(y = Std.dist.f.last,
                             x = Year)) +
  theme(
    aspect.ratio = 1,
    text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    strip.text = element_text(size = 13),
    legend.position = "right",
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(hjust = 0.5),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  labs(x = "Year",
       y = "Standardized Distance By #Years Passed",
       fill = "Fish\nStatus") +
  geom_col(aes(fill = FishStatus),na.rm = T, color = "black") +
  facet_wrap(vars(Plot.Order),
             labeller = labeller(Plot.Order = Lakes),
             scales = "free_x") +
  scale_x_discrete(na.translate = FALSE)+
  scale_fill_manual(
    values = c(
      "Present" = "red1",
      "Decline" = 'blue1',
      "Absent" = "green1"
    ),
    breaks = c("Present", "Decline", "Absent"),
    labels = c("Present", "Decline", "Absent")
  )


std.dist.facet






dist.to.ref.facet <- ggplot(data = XY.distances,
                         aes(y = Dist.Frm.Centroid,
                             x = Year)) +
  theme(
    aspect.ratio = 1,
    text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black"),
    strip.text = element_text(size = 13),
    legend.position = "right",
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(hjust = 0.5),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  labs(x = "Year",
       y = "Distance To Reference Centroid",
       fill = "Fish\nStatus") +
  geom_col(aes(fill = FishStatus),na.rm = T, color = "black") +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(Plot.Order),
             labeller = labeller(Plot.Order = Lakes),
             scales = "free_x") +
  scale_x_discrete(na.translate = FALSE)+
  scale_fill_manual(
    values = c(
      "Present" = "red1",
      "Decline" = 'blue1',
      "Absent" = "green1"
    ),
    breaks = c("Present", "Decline", "Absent"),
    labels = c("Present", "Decline", "Absent")
  )


dist.to.ref.facet
