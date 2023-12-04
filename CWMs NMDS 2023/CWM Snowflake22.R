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

####### Subset to make Snwit coords dataframe ########
SnwCWM22 <- CWM.Sites22[69:102,]

SnwCWM22.rm <- CWM.Sites22.rm[69:101,-4]


################### Time to Visualize ##################
set.seed(42)

SnwCWM.22.rm <- ggplot(data = SnwCWM22.rm,
                       mapping = aes(x = NMDS1,
                                     y = NMDS2)) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
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
       color = "Snowflake Lake\nStocked 1960-66") +
  geom_path(
    aes(color = Fish.Status,
        group = Lake),
    show.legend = TRUE,
    size = 1.5
  ) +
  geom_text_repel(aes(label = Year),
                  max.overlaps = 25) +
  geom_point(
    col = "black",
    pch = 21,
    size = 3,
    stroke = 1.4
  ) +
  geom_text_repel(
    data = CWM.Traits22.rm,#Place Trait names
    aes(
      x = NMDS1,
      y = NMDS2,
      label = row.names(CWM.Traits22.rm)
    ),
    fontface = "italic",
    color = "black",
    size = 6,
    hjust = "outward",
    seed = 42 # to keep the positions consistent across plots
  ) +
  geom_point(data=SnwCWM22.rm, #Makes mark at Hespero innoculation
             aes(x=NMDS1[14],
                 y=NMDS2[14]),
             col= "black",
             pch = 24, #Upward triangle shape
             bg = "purple",
             size = 6,
             stroke = 1.4,
             show.legend = FALSE)+
  geom_point(data=SnwCWM22.rm, #Makes mark at Hespero innoculation
             aes(x=NMDS1[14],
                 y=NMDS2[14]),
             col= "black",
             pch = 25, #Downward triangle shape
             bg = "purple",
             size = 6,
             stroke = 1.4,
             show.legend = FALSE)+

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
    x = NMDS1[33],
    y = NMDS2[33],
    label = "2022"),
    fill = "white",
    size = 5) +
  scale_color_manual(values = c("Present" = "red","Decline" = 'blue',"Absent" = "green"),
                     breaks = c("Present","Decline","Absent")
                     ) +
  scale_x_continuous(limits = global.x.lims) +
  scale_y_continuous(limits = global.y.lims)


SnwCWM.22.rm

# Make Hepero inoc label in post