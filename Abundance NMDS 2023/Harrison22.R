
####### Create GGplots For Each Lake ###################
library(ggplot2)
library(GGally)
library(ggsci)
library(ggpp)
library(ggrepel)
library(ggstatsplot)
library(cowplot)
library(ggthemes)



##### Visualize Pre-isolated Dataframe of site coords per lake #######

Harrison22 <- ggplot(data = Har22,
                  mapping = aes(x = NMDS1,
                                y = NMDS2)
) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    axis.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.text = element_text(size = 15, color = "#000000", face = "bold"),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.title = element_text(size = 22, face = "bold"),
    legend.key.size = unit(2, 'lines'),
    legend.key.width = unit(2, "line"),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.position = c(0.2,0.85),
    legend.title.align = 0.5,
    legend.box.background = element_rect(
      fill = "transparent",
      colour = "transparent",
      size = 1.2
    ),
    legend.text = element_text(size = 15),
  ) +
  labs(x = "NMDS 1",
       y = "NMDS 2",
       color = "Harrison Lake\nNatural Bull Trout") +
  geom_text(data = Spp22,aes(
    x = NMDS1,
    y = NMDS2,
    label = spp.names
  ),
  fontface = "italic",
  color = "black",
  size = 6,
  hjust = "middle"
  ) +
  stat_ellipse(
    data = Cntl22,
    linetype = 2,
    size = 1.5
  ) +
  stat_ellipse(data = Cntl22,
               geom = "polygon",
               fill = "darkorchid2",
               alpha = 0.3) +
  geom_path(color = "red",
    size = 1.5,
    show.legend = T,
  ) +
  geom_text_repel(aes(label = Year)) +
  geom_point(
    col = "black",
    pch = 21,
    size = 3,
    stroke = 1.4
  ) +

  geom_label(aes(x = NMDS1[1], y = NMDS2[1], label = "1966"), size = 5) +

  geom_label(aes(x = NMDS1[17], y = NMDS2[17], label = "2022"), size = 5)+
  scale_color_manual(values = c("Stocked" = "red"),
                     labels = c("Present"))+
  scale_x_continuous(limits = global.x.lims,
                     labels = c(-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5),
                     breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5)
  ) +
  scale_y_continuous(limits = global.y.lims,
                     labels = c(-1.5,-1,-0.5,0,0.5,1,1.5),
                     breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5)
  )


Harrison22

