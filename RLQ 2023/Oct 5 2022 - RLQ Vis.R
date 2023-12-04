##### Install / Load Packages #####
library(ade4)
library(adegraphics)
library(vegan)
library(ggplot2)
library(ggrepel)
library(ggpmisc)
library(ggsci)




######## Visualize NSR RLQ ############

rlq.env.gg <- ggplot(data = rlq.Casc22.ENV) +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 15,
                        face = "bold"),
    axis.text = element_text(color = "black"),
    plot.background = element_blank(),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = 2, size = 1, alpha = 0.6, color = "grey3") +
  geom_vline(xintercept = 0, linetype = 2, size = 1, alpha = 0.6, color = "grey3") +
  labs(x = "Axis 1",
       y = "Axis 2")+
  geom_segment(aes(x = 0,
                   xend = 0 + CS1,
                   y = 0,
                   yend = 0 + CS2),
               size = 1.2,
               linejoin = "mitre",
               lineend = "butt",
               arrow = arrow()) +
  geom_text_repel(aes(x = CS1,
                      y = CS2,
                      label = row.names(rlq.Casc22.ENV)),
                  fontface = "bold")

rlq.env.gg
rlq.env.gg2 <- ggplotGrob(rlq.env.gg)

rlq.Casc.22 <- ggplot() +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 15,
                        face = "bold"),
    axis.text = element_text(color = "black"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = "left",
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = 2, size = 1, alpha = 0.6) + #FARTS
  geom_vline(xintercept = 0, linetype = 2, size = 1, alpha = 0.6) +
  labs(x = "RLQ Axis 1 (95.28% co-intertia)",
       y = "RLQ Axis 2 (3.67% co-intertia)",
       fill = "Order",
       color = "Trait Class",
       shape = "Trait Class") +
  geom_jitter(data = rlq.traits.22,
              aes(x = CS1,
                  y = CS2,
                  color = Group,
                  shape = Group),
              size = 5,
              width = 0.06,
              height = 0.06,
              show.legend = TRUE) +
  geom_label_repel(data = rlq.traits.22,
                   aes(x = CS1,
                       y = CS2,
                       label = row.names(rlq.traits.22),
                       color = Group),
                   segment.size = 2,
                   point.padding = 0.5,
                   show.legend = FALSE,
                   force = 5,
                   size = 5) +
  geom_text_repel(data = rlq.spp.22,
                  aes(x = CS1,
                      y = CS2,
                      label = row.names(rlq.spp.22)),
                  fontface = "italic",
                  show.legend = FALSE,
                  force = 2,
                  size = 5) +
  coord_fixed() +
  annotation_custom(grob = rlq.env.gg2,
                    xmin = 2.5,
                    xmax = 6.1,
                    ymin = -3.5,
                    ymax = -1.0
                      )+
  scale_fill_d3("category20") +
  scale_color_d3("category20") +
  xlim(-1,6.5)



rlq.Casc.22 #Retitle Legend in AI
graphics.off()
dev.off()
