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

cRES <- list(Pro.big.res,Pro.Pip.res, Pro.snw.res, Pro.har.res, Pro.Ref.res)

cRES <- cbind(cRES)

cRES1 <- cRES %>% reduce(full_join, by="Residuals")

cRES <- data.frame(Residuals = cRES1$Residuals,
                   Year = ctest.order$Year,
                   Lake = ctest.order$Lake,
                   Fish.Status = ctest.order$Fish.Status)


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
cRES$Plot.Order <- as.factor(ctest.order$Plot.Order)



Lakes <- c("1" = "Bighorn",
          "2" = "Snowflake",
          "3" = "Pipit",
          "4" = "Harrison",
          "5" = "Eiffel",
          "6" = "Sentinel",
          "7" = "Hungabee",
          "8" = "Oesa",
          "9" = "Opabin")

str(cRES1)
cRES1$Year <- factor(cRES1$Year)
cRES1$Plot.Order <- factor(cRES1$Plot.Order)

##### Time for Viz ######
set.seed(42)


write.csv(cRES, "pro residuals.csv")


pro.RES.facet <- ggplot(data = cRES1,
                        aes(y = Residuals,
                            x = Year)) +
  theme(
    aspect.ratio = 1,
    text = element_text(face = "bold", color = "black"),
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
       y = "Procrustes Residuals",
       fill = "Fish\nStatus") +
  geom_col(aes(fill = Fish.Status), color = "black") +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(Plot.Order),
             labeller = labeller(Plot.Order = Lakes),
             scales = "free_x") +
  scale_fill_manual(
    values = c(
      "Present" = "red1",
      "Decline" = 'blue1',
      "Absent" = "green1"
    ),
    breaks = c("Present", "Decline", "Absent"),
    labels = c("Present", "Decline", "Absent")
  )


pro.RES.facet

