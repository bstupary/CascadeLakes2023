
####### Create GGplots For Each Lake ###################
library(ggplot2)
library(GGally)
library(ggsci)
library(ggpp)
library(ggrepel)
library(ggstatsplot)
library(cowplot)
library(ggthemes)

bighorn22 <- ggplot(data = Big22,
    mapping = aes(x = NMDS1,
                  y = NMDS2),
    group = FishStatus,
    color = FishStatus
  ) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    axis.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.text = element_text(size = 15, color = "#000000", face = "bold"),
    panel.background = element_rect(fill = "transparent", colour = NA),
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

  geom_text(data = Spp22,aes(
      x = NMDS1,
      y = NMDS2,
      label = rownames(Spp22)
    ),
    fontface = "italic",
    color = "black",
    size = 6,
    hjust = "outward"
  ) +
  geom_path(
    aes(
      color = FishStatus,
      group = Lake),
      size = 1.5,
      # arrow = arrow(ends = "last"),
      show.legend = T
    ) +
      geom_text_repel(aes(label = Year)) +
      geom_point(
        col = "black",
        pch = 21,
        size = 3,
        stroke = 1.4
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

  scale_color_igv()



bighorn22

