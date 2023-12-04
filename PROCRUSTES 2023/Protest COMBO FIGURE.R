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



###### Can We Facet The Protest Figs? ##########

cBIG <- data.frame(NMDS1 = protest.big$Yrot[,1],
                    NMDS2 = protest.big$Yrot[,2],
                    xNMDS1 = protest.big$X[,1],
                    xNMDS2 = protest.big$X[,2],
                    Lake = "Bighorn",
                   Year = BigCWM22.rm$Year,
                   Fish.Status = BigCWM22.rm$Fish.Status)

cPIP <- data.frame(NMDS1 = protest.pip$Yrot[,1],
                   NMDS2 = protest.pip$Yrot[,2],
                   xNMDS1 = protest.pip$X[,1],
                   xNMDS2 = protest.pip$X[,2],
                   Lake = "Pipit",
                   Year = PipCWM22.rm$Year,
                   Fish.Status = PipCWM22.rm$Fish.Status)

cSNW <- data.frame(NMDS1 = protest.snw$Yrot[,1],
                   NMDS2 = protest.snw$Yrot[,2],
                   xNMDS1 = protest.snw$X[,1],
                   xNMDS2 = protest.snw$X[,2],
                   Lake = "Snowflake",
                   Year = SnwCWM22.rm$Year,
                   Fish.Status = SnwCWM22.rm$Fish.Status
                   )

cHAR <- data.frame(NMDS1 = protest.har$Yrot[,1],
                   NMDS2 = protest.har$Yrot[,2],
                   xNMDS1 = protest.har$X[,1],
                   xNMDS2 = protest.har$X[,2],
                   Lake = "Harrison",
                   Year = Har22$Year,
                   Fish.Status = "Present")

cREF <- data.frame(NMDS1 = protest.Ref$Yrot[,1],
                   NMDS2 = protest.Ref$Yrot[,2],
                   xNMDS1 = protest.Ref$X[,1],
                   xNMDS2 = protest.Ref$X[,2],
                   Lake = Cntl22$Lake,
                   Year = Cntl22$Year,
                   Fish.Status = "Absent")

##### Make a master dataframe of values since they were all run seperate #####

ctest1 <- rbind(cBIG,cPIP)
ctest2 <- rbind(cSNW,cHAR)
ctest <- rbind(ctest1,ctest2)
ctest <- rbind(ctest,cREF)

ctest$Lake

str(ctest.order)
ctest.order$Plot.Order <- as.factor(ctest.order$Plot.Order)

Lakes <- c("1" = "Bighorn",
           "2" = "Pipit",
           "3" = "Snowflake",
           "4" = "Harrison",
           "5" = "Opabin",
           "6" = "Oesa",
           "7" = "Eiffel",
           "8" = "Hungabee",
           "9" = "Sentinel")




##### Time for Viz ######
set.seed(42)

str(ctest$Lake)
ctest$Lake <- as.factor(ctest$Lake)


pro.test.super.facet <- ggplot(data = ctest.order) +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 15,
                        face = "bold"),
    axis.text = element_text(color = "black"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    #legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  labs(x = "Dimension 1",
       y = "Dimention 2") +
  geom_point(
    aes(x = NMDS1,
        y = NMDS2,
       # fill = Fish.Status
       ),
    shape = 21,
    fill = "grey",
    show.legend = TRUE,
    size = 5
  ) +
  geom_point(
    aes(
      x = xNMDS1,
      y = xNMDS2,
     # fill = Fish.Status
    ),
    fill = "grey",
    shape = 22,
    show.legend = TRUE,
    size = 5
  ) +
  geom_segment(
    aes(
      x = NMDS1,
      y = NMDS2,
      xend = xNMDS1,
      yend = xNMDS2
    ),
    arrow = arrow(length = unit(0.4, "cm")),
    color  = "black",
    size = 0.8
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
   coord_fixed() +
  facet_wrap(vars(Plot.Order), labeller = labeller(Plot.Order = Lakes))
  # scale_fill_manual(
  #   values = c(
  #     "Present" = "red1",
  #     "Decline" = 'blue1',
  #     "Absent" = "green1"
  #   ),
  #   breaks = c("Present", "Decline", "Absent"),
  #   labels = c("Present", "Decline", "Absent"))


pro.test.super.facet

