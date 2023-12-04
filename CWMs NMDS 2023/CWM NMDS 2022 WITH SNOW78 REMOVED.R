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

rm.row <- "Sno78"

CWM.traits.all.78rm <- CWM.traits.all[!(row.names(CWM.traits.all) %in% rm.row),]


CWM.NMDS.2022.RM <-
  metaMDS(CWM.traits.all.78rm,
          distance = "bray",
          k = 2,
          trymax = 100000) #NMDS convergent solution after 36 runs. 0.0619672 stress

Casc22_Site_Meta.rm <- Casc22_Site_Meta[-80,]

Casc22_Site_Meta.rm["Fish.Status"][Casc22_Site_Meta.rm["Fish.Status"] == "Stocked"] <- "Present"


#Pull out some scores so I can graph it
CWM.Sites22.rm <-
  as.data.frame(scores(CWM.NMDS.2022.RM, "sites")) # Get the site coordinates
CWM.Sites22.rm$Lake <- Casc22_Site_Meta.rm$Lake #add context
CWM.Sites22.rm$Year <- Casc22_Site_Meta.rm$Year #add context
CWM.Sites22.rm$Fish.Status <- Casc22_Site_Meta.rm$Fish.Status #add context
write.csv(CWM.Sites22.rm, "CWM.Sites22.rm.csv") #Write it as a excel table so you have a "hardcopy"




CWM.Traits22.rm <- as.data.frame(scores(CWM.NMDS.2022.RM, "species"))  # Get the spp coordinates

row.names(CWM.Traits22.rm) <- c(
  "Carnivore",
  "Herbivore",
  "Omnivore",
  "Filter Feeding",
  "Raptorial",
  "Large Body",
  "Medium Body",
  "Small Body",
  "Facultative Asexual",
  "Obligate Sexual",
  "No Pigment",
  "Pigmented",
  "Rapid Movement",
  "Slow Movment"
)
write.csv(CWM.Traits22, "CWM.Traits22.csv") #Write as excel table so you have a "hardcopy"


################### Time to Visualize ##################

CWM.22.rm <- ggplot() +
  theme_classic() +
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
    axis.text = element_text(size = 15, color = "#000000"),
    axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
    axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1.7
    ),
    #axis.line.x = element_line(size=1.2),
    #axis.line.y = element_line(size=1.2),
    legend.position = "top right",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.title = element_blank(),
    legend.key.size = unit(2, 'lines'),
    legend.key.width = unit(4, "line"),
    legend.box.margin = margin(t = 0.5, unit = "cm"),
    legend.box.background = element_rect(
      fill = "transparent",
      colour = "transparent",
      size = 1.2
    ),
    legend.text = element_text(size = 15),
    plot.tag = element_text(size = 30),
    legend.justification = c(0.5, 0.5)
  ) +
  labs(x = "NMDS 1",
       y = "NMDS 2") +
  geom_path(
    data = CWM.Sites22.rm,
    aes(x = NMDS1,
        y = NMDS2,
        col = Lake),
    show.legend = TRUE,
    size = 1.5,
    arrow = arrow()
  ) +
  geom_point(
    data = CWM.Sites22.rm,#Makes circles at year points
    aes(x = NMDS1,
        y = NMDS2),
    col = "black",
    pch = 21,
    size = 3,
    stroke = 1.4
  ) +
  geom_text_repel(data = CWM.Traits22.rm, #Place Trait names
    aes(
      x = NMDS1,
      y = NMDS2,
      label = row.names(CWM.Traits22)
    ),
    fontface = "italic",
    color = "black",
    size = 6,
    hjust = "outward",
    seed = 42) # to keep the positions consistent across plots
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
  # )

CWM.22.rm


# x-range
global.x.lims <- layer_scales(CWM.22.rm)$x$range$range

#y-range
global.y.lims <- layer_scales(CWM.22.rm)$y$range$range
