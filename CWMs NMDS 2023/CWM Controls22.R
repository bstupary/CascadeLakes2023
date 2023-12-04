##### Install / Load Packages #####
library(vegan)
library(ggplot2)
library(ggalt)
library(ggforce)
library(ggrepel)
library(ggpmisc)

#################### Set Working Directory #################
setwd("E:/2020 Thesis Work/New Control Tinkering Jan 17, 2020/CWMs May 2020")

############ Load Workspace ENV ###############
load(
  "E:/2020 Thesis Work/New Control Tinkering Jan 17, 2020/CWMs May 2020/CMWs May2020.Rdata"
)

############### Stuff Loaded, time to analyze CWM (DIST, NMDS) #################
CWM.Sites22.controls <-
  CWM.Sites22[103:145, ] #Extract only control lakes


################### Time to Visualize ##################
set.seed(42)

CWM.22.Controls <- ggplot() +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(
      size = 40,
      face = "bold",
      hjust = 0.5,
      margin = margin(0, 0, 12, 0, unit = "pt")
    ),
    plot.subtitle = element_text(
      size = 20,
      face = "italic",
      hjust = 0.5,
      margin = margin(0, 0, 0, 0, unit = "pt")
    ),
    axis.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.text = element_text(size = 15, color = "#000000", face = "bold"),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.key.size = unit(2, 'lines'),
    legend.key.width = unit(4, "line"),
    legend.box.background = element_rect(
      fill = "transparent",
      colour = "transparent",
      size = 1.2
    ),
    legend.text = element_text(size = 15),
    plot.tag = element_text(size = 30)
  ) +
  xlab("NMDS 1") +
  ylab("NMDS 2") +
  geom_path(
    data = CWM.Sites22.controls,
    aes(x = NMDS1,
        y = NMDS2,
        col = Lake),
    show.legend = TRUE,
    size = 1.5,
  ) +
  geom_point(
    data = CWM.Sites22.controls,
    #Makes circles at year points
    aes(
      x = NMDS1,
      y = NMDS2,
      group = Lake
    ),
    col = "black",
    pch = 21,
    size = 3,
    stroke = 1.4
  ) +
  geom_text_repel(
    data = CWM.Traits22,
    #Place Trait names
    aes(
      x = NMDS1,
      y = NMDS2,
      label = row.names(CWM.Traits22)
    ),
    fontface = "italic",
    color = "black",
    size = 6,
    hjust = "outward",
    seed = 42 # to keep the positions consistent across plots
  ) +
  stat_ellipse(
    data = CWM.Sites22.controls,
    aes(x = NMDS1,
        y = NMDS2),
    linetype = 2,
    size = 1.5
  ) +
  stat_ellipse(
    data = CWM.Sites22.controls,
    aes(x = NMDS1,
        y = NMDS2),
    geom = "polygon",
    fill = "darkorchid2",
    alpha = 0.3)
  # ) +
  # scale_x_continuous(
  #   limits = c(-1.5, 1.5),
  #   labels = waiver(),
  #   breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)
  # ) +
  # scale_y_continuous(
  #   limits = c(-0.6, 1.1),
  #   labels = waiver(),
  #   breaks = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
  # ) +
  # scale_colour_brewer(palette = "Set1") # Recolor groups

CWM.22.Controls

########### Pull out the "Zone of Control" of Traits for use elsewhere ##################

Zone.Of.CWM.Controls <- ggplot() +
  stat_ellipse(
    data = CWM.Sites22.controls,
    aes(x = NMDS1,
        y = NMDS2),
    linetype = 2,
    size = 1.5
  ) +
  stat_ellipse(
    data = CWM.Sites22.controls,
    aes(x = NMDS1,
        y = NMDS2),
    geom = "polygon",
    fill = "darkorchid2",
    alpha = 0.3
  )
