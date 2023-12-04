##### Install / Load Packages #####

library(vegan)
library(ggplot2)
library(ggalt)
library(ggforce)
library(ggrepel)
library(ggpmisc)
library(dplyr)
library(tidyr)
library(ggsci)


#Set Working Directory
setwd("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/BIOMASS 2022")


######## Load in your datasets ###############

all.lake.bio <- read.csv("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/BIOMASS 2022/ALL LAKES BIOMASSES.csv", stringsAsFactors=TRUE)

######## depreciated? ##########
Casc.Bm.22 <- read.csv("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/BIOMASS 2022/Casc Biomass 2022 (HARP+Sno78 rm).csv")

FourLakeBiomasses <- read_csv("FourLakeBiomasses.csv")
View(FourLakeBiomasses)

FourLakeBiomasses$Lake <- as.factor(FourLakeBiomasses$Lake)
FourLakeBiomasses$Year <- as.factor(FourLakeBiomasses$Year)
FourLakeBiomasses$Fish.Status <- as.factor(FourLakeBiomasses$Fish.Status)



str(FourLakeBiomasses)

FourLakeBiomasses.nomean <- FourLakeBiomasses[,-15]

Casc.Bm.22$Lake <- Casc22.meta[-80,1]


Casc.Bm.22$Year <- as.factor(Casc22.meta[-80,2])

Casc.Bm.22$Status <- Casc22.meta[-80,3]

########### Reshape data ############
Casc.Bm.22.tib <- as_tibble(Casc.Bm.22)
str(Casc.Bm.22.tib)

# WIDE <- Casc.Bm.22 %>%
#   pivot_longer(!c(Lake,Year,Status), names_to = "Taxa", values_to = "Biomass")

WIDE.four <- FourLakeBiomasses %>%
  pivot_longer(!c(Lake,Year,Fish.Status), names_to = "Taxa", values_to = "Biomass")

WIDE.four.nomean <- FourLakeBiomasses.nomean %>%
  pivot_longer(!c(Lake,Year,Fish.Status), names_to = "Taxa", values_to = "Biomass")

str(WIDE.four)

WIDE.FOUR <- as.data.frame(WIDE.four)

#################

############ Visualize Plot of Four Study Lakes Mean Annual Biomass ##################

# str(Ref.Yr.mass)
#
# Ref.Yr.mass$Year <- as.factor(Ref.Yr.mass$Year)
# Ref.Yr.mass <- Ref.Yr.mass[,-3]


all.lake.bio$Lake.Order <- as.factor(all.lake.bio$Lake.Order)
all.lake.bio$Year <- as.factor(all.lake.bio$Year)

Lakes <- c("1" = "Bighorn",
           "2" = "Snowflake",
           "3" = "Pipit",
           "4" = "Harrison",
           "5" = "Eiffel",
           "6" = "Sentinel",
           "7" = "Hungabee",
           "8" = "Oesa",
           "9" = "Opabin")
spp.names <-
  c(
    "Acanthocyclops vernalis",
    "Alona guttata",
    "Chydorus sphaericus",
    "Cyclopoida spp.",
    "Daphnia middendorffiana",
    "Daphnia pulex",
    "Diacyclops thomasi",
    "Eucyclops spp.",
    "Hesperodiaptomus arcticus",
    "Leptodiaptomus tyrrelli",
    "Macrocyclops albidus"
  )

# according to descriptive stats from excel, average annual biomass across all years of reference lakes [RMAB] is 67.24789262, w/ Confidence Level(95.0%) [RMACL]	17.85991206

RMAB = 67.24789262
RMACL = 17.85991206

X4Lake_means2<- X4Lake_means
X4Lake_means2$Ref.Mean = RMAB
X4Lake_means2$Ref.UCI = 85.10780467
X4Lake_means2$Ref.LCI = 49.38798056

write.csv(X4Lake_means2, file = "x4Lake_means2 w plot orders.csv")

X4Lake_means2.plotorders <-
  read.csv(
    "C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/BIOMASS 2022/x4Lake_means2 w plot orders.csv",
    stringsAsFactors = TRUE
  )
View(X4Lake_means2.plotorders)

str(X4Lake_means2.plotorders)

X4Lake_means2.plotorders$Lake.Order <- as.factor(X4Lake_means2.plotorders$Lake.Order)
X4Lake_means2.plotorders$Year <- as.factor(X4Lake_means2.plotorders$Year)


Casc.Bio22.Fig <- ggplot(X4Lake_means2.plotorders,
                         aes(x = Year,
                             y = Yr.mass,
                             fill = Fish.Status)) +

  theme(
    aspect.ratio = 1,
    text = element_text(size = 15, face = "bold", color = "black"),
    axis.text = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, face = "bold", color = "black"),
    strip.text = element_text(size = 13, color = "black"),
    legend.position = "right",
    legend.box.just = "right",
    legend.title = element_text(hjust = 0.5),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  labs(y = "Mean Annual Biomass (µg/L)",
       fill = "Fish\nStatus") +
  facet_wrap(vars(Lake.Order),
             labeller = labeller(Lake.Order = Lakes),
             scales = "free") +
  geom_col(aes(fill = Fish.Status), color = "black") +
  geom_ribbon(
    aes(ymin = Ref.LCI,
        ymax = Ref.UCI,
      group = 1),
    fill = "grey20",
    color = "black",
    outline.type = "both",
    linetype = 2,
    alpha = 0.35,
    show.legend = F
  ) +
  geom_hline(yintercept = RMAB, linetype = 1) +
geom_hline(yintercept = 0) +
  scale_fill_manual(
    values = c(
      "Present" = "red",
      "Decline" = 'blue',
      "Absent" = "green"
    ),
    labels = c("Present", "Decline", "Absent"),
    breaks =   c("Present", "Decline", "Absent")
  )





Casc.Bio22.Fig







#######



all.lake.bio$Lake.Order <- as.factor(all.lake.bio$Lake.Order)
all.lake.bio$Year <- as.factor(all.lake.bio$Year)

Lakes <- c("1" = "Bighorn",
           "2" = "Snowflake",
           "3" = "Pipit",
           "4" = "Harrison",
           "5" = "Eiffel",
           "6" = "Sentinel",
           "7" = "Hungabee",
           "8" = "Oesa",
           "9" = "Opabin")

all.bio <- ggplot(all.lake.bio,
                         aes(x = Year,
                             y = Yr.mass,
                             fill = Fish.Status)
                  ) +
    theme(
    aspect.ratio = 1,
    text = element_text(face = "bold", size = 15),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    strip.text = element_text(size = 13),
    legend.position = "right",
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(hjust = 0.5),
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  labs(y = "Mean Annual Biomass (µg/L)",
       fill = "Fish\nStatus") +
  geom_col(aes(fill = Fish.Status), color = "black") +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(Lake.Order),
             labeller = labeller(Lake.Order = Lakes),
             scales = "free") +
  scale_fill_manual(
    values = c(
      "Present" = "red",
      "Decline" = 'blue',
      "Absent" = "green"
    ),
    labels = c("Present", "Decline", "Absent"),
    breaks =   c("Present", "Decline", "Absent")
  )



all.bio



############## Visualize Four Study Lake Biomass per Taxa ##########

#we want proper names when ploting facets of the taxa, not abreviations
spp.names<-
c("Acanthocyclops vernalis","Alona guttata","Chydorus sphaericus","Cyclopoida spp.","Daphnia middendorffiana","Daphnia pulex","Diacyclops thomasi","Eucyclops spp.","Hesperodiaptomus arcticus","Leptodiaptomus tyrrelli","Macrocyclops albidus")

WIDE.four.nomean$Taxa <- as.factor(WIDE.four.nomean$Taxa) #make it a factor so it has levels
levels(WIDE.four.nomean$Taxa) <- spp.names #changes the values of the factor to new vector of strings (order is important for renaming)

Casc.Bio22.taxa <- ggplot(data = WIDE.four.nomean,
                          aes(x = Year,
                              y = Biomass,
                              fill = Fish.Status)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill = "transparent", colour = NA),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    strip.clip = "off",
    legend.title.align = 0.5
  ) +
  labs(y = "Mean Annual Biomass (µg/L)",
       fill = "Fish\nStatus") +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  facet_grid(
    rows = vars(Lake),
    cols = vars(Taxa),
    scales = "free",
    drop = TRUE,
    labeller = label_wrap_gen(multi_line = TRUE, width = 5)
  ) +
  scale_fill_manual(
    values = c(
      "Present" = "red",
      "Decline" = 'blue',
      "Absent" = "green"
    ),
    labels = c("Present", "Decline", "Absent"),
    breaks =   c("Present", "Decline", "Absent")
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

Casc.Bio22.taxa

################



############## Visualize Four Study Lake Biomass OF just D.midd, H.arc##########

#we want proper names when ploting facets of the taxa, not abreviations


WIDE.four_d_h <- FOUR_D_H %>%
  pivot_longer(!c(Lake,Year,Fish.Status), names_to = "Taxa", values_to = "Biomass")

WIDE.four_d_h$Lake <- as.factor(WIDE.four_d_h$Lake)
WIDE.four_d_h$Year <- as.factor(WIDE.four_d_h$Year)
WIDE.four_d_h$Fish.Status <- as.factor(WIDE.four_d_h$Fish.Status)
WIDE.four_d_h$Taxa <- as.factor(WIDE.four_d_h$Taxa) #make it a factor so it has levels

levels(WIDE.four_d_h$Taxa) <- c("Daphnia middendorffiana", "Hesperodiaptomus arcticus") #changes the values of the factor to new vector of strings (order is important for renaming)

Casc.d_h <- ggplot(data = WIDE.four_d_h,
                   aes(
                     x = Year,
                     y = Biomass,
                     color = Fish.Status,
                     group = Fish.Status
                   )) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill = "transparent", colour = NA),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    strip.clip = "off",
    legend.title.align = 0.5
  ) +
  labs(y = "Mean Annual Biomass (µg/L)",
       color = "Fish\nStatus",
       shape = "Fish\nStatus") +
  geom_path(aes(color = Fish.Status,
                group = Fish.Status),
            linewidth = 1.1) +
  geom_point(size = 1.5,
             col = "black") +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(Lake, Taxa),
             scales = "free",
             nrow = 4,
             ncol = 2) +
  scale_color_manual(
    values = c(
      "Present" = "red",
      "Decline" = 'blue',
      "Absent" = "green"
    ),
    labels = c("Present", "Decline", "Absent")
  )
Casc.d_h

################


