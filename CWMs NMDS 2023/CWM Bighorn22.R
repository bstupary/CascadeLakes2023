##### Install / Load Packages #####
library(vegan)
library(ggplot2)
library(ggalt)
library(ggforce)
library(ggrepel)
library(ggpmisc)

############ Set Working Directory ##############
setwd("C:/Users/Blake/Desktop/CASCADE 2022/CWMs NMDS 2022")




####### Subset to make Bighorn coords dataframe ########
BigCWM22 <- CWM.Sites22[1:23, ]

BigCWM22.rm <- CWM.Sites22.rm[1:23,-4]


################### Time to Visualize ##################
set.seed(42)


BigCWM.22 <- ggplot(data = BigCWM22.rm,
                    mapping = aes(x = NMDS1,
                                  y = NMDS2)) +
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
    legend.title = element_text(size = 22, face = "bold"),
    legend.key = element_rect(fill = "transparent"),
    legend.key.size = unit(2, 'lines'),
    legend.key.width = unit(2, "line"),
    legend.position = c(0.86,0.88),
    legend.box.background = element_rect(
      fill = "transparent",
      colour = "transparent",
      size = 1.2
    ),
    legend.text = element_text(size = 15),
  ) +
  labs(x = "NMDS 1",
       y = "NMDS 2",
       color = "Bighorn Lake\nStocked 1965-66") +
  geom_path(
    aes(color = Fish.Status,
        group = Lake),
    show.legend = TRUE,
    size = 1.5
  ) +
  geom_text_repel(aes(label = Year)) +
  geom_point(
    col = "black",
    pch = 21,
    size = 3,
    stroke = 1.4
  ) +
  geom_text_repel(
    data = CWM.Traits22.rm,
    #Place Trait names
    aes(
      x = NMDS1,
      y = NMDS2,
      label = row.names(CWM.Traits22.rm)
    ),
    fontface = "italic",
    color = "black",
    size = 6,
    hjust = "outward",
    xlim = c(-1,1),
    seed = 42 # to keep the positions consistent across plots
  ) +
  stat_ellipse(
    data = CWM.Sites22.controls.rm,
    aes(x = NMDS1,
        y = NMDS2),
    linetype = 2,
    size = 1.5
  ) +
  stat_ellipse(
    data = CWM.Sites22.controls.rm,
    aes(x = NMDS1,
        y = NMDS2),
    geom = "polygon",
    fill = "darkorchid2",
    alpha = 0.3
  ) +

  geom_label(aes(
    x = NMDS1[1],
    y = NMDS2[1],
    label = "1966"),
    fill = "white",
    size = 5) +
  geom_label(aes(
    x = NMDS1[23],
    y = NMDS2[23],
    label = "2022"),
    fill = "white",
    size = 5) +
  scale_color_manual(values = c("Present" = "red","Decline" = 'blue',"Absent" = "green"),
                     breaks = c("Present","Decline","Absent"),
                     labels = c("Present", "Gill Netted", "Absent")

  ) +
  scale_x_continuous(limits = global.x.lims) +
  scale_y_continuous(limits = global.y.lims)

BigCWM.22


# dont forget to swap "decline" with "Gill Netted" of the legend in post