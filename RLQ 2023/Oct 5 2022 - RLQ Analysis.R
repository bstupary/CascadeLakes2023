##### Install / Load Packages #####
library(ade4)
library(adegraphics)
library(vegan)
library(ggplot2)
library(ggrepel)
library(ggpmisc)
library(ggsci)
library(plyr)
library(dplyr)

#Set Working Directory
setwd("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/RLQ Feb2020")

setwd("E:/2020 Thesis Work/New Control Tinkering Jan 17, 2020/RLQ Feb2020")

#Import Workspace from RLQ
load("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/R February/NMDS Master Feb24.RData")




############ Import Data From wd #######################

Casc22.abnd <- read.csv("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/RLQ 2022/CascAbund 2022 CORRECT AND TRIM FOR RLQ.csv", row.names=1)

Casc22.ENV <- read.csv("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/RLQ 2022/CascENV Feb2020.csv", row.names=1, stringsAsFactors=TRUE)

Casc22.ENV <- Casc22.ENV[,-c(3)]


Casc22.Traits <- read.csv("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/RLQ 2022/CascTraits Feb2020.csv", row.names=1, stringsAsFactors=TRUE)
View(Casc22.Traits)


################# RUN THE RLQ ######################


########### March 02, 2020- RLQ Analysis (including all species) ##################

#Prelim Abund CA # Arguments scannf = to show eigenvalue plot, nf = for number of axis calculated
Abund.CA.22 <- dudi.coa(Casc22.abnd, scannf = F, nf = 4) # 4 axis

#Prelim Enviro hillsmith (since mixed factors [numbers and words])
ENV.Hill.22 <- dudi.hillsmith(Casc22.ENV, scannf = F, row.w = Abund.CA.22$lw, nf = 6) #6 axis
str(Casc22.ENV)

# Casc22.ENV.r <- Casc22.ENV
#
# Casc22.ENV.recode <- recode(.x = Casc22.ENV$FISH, "YES" = 1, "NO" = 0)
#
# Casc22.ENV.r$FISH <- as.factor(Casc22.ENV.recode)
#
# str(Casc22.ENV.r)
#
# ENV.Hill.22.fishy <- dudi.hillsmith(Casc22.ENV.r, scannf = F, row.w = Abund.CA.22$lw, nf = 6)


#Prelim Traits multiple corrispondance analysis (since all are factors [words])
Trait.ACM.22 <- dudi.acm(Casc22.Traits, scannf = F, row.w = Abund.CA.22$cw, nf = 4)#4 axis

#RLQ GOGOGO nf = 2

rlq1.22 <- rlq(dudiR = ENV.Hill.22, dudiL = Abund.CA.22, dudiQ = Trait.ACM.22, scannf = F, nf = 2)

# rlq1.fishy <- rlq(dudiR = ENV.Hill.22.fishy, dudiL = Abund.CA.22, dudiQ = Trait.ACM.22, scannf = F, nf = 2)

# Testing the ploting and significance
# RLQplot.fishy <- plot(rlq1.fishy)
# RLQplot.fishy
# RLQsum1.22.fishy <- as.data.frame(summary(rlq1.fishy))
# RLQsum1.22.fishy

# Testing the ploting and significance
RLQplot. <- plot(rlq1.22)
RLQplot.
RLQsum1.22 <- as.data.frame(summary(rlq1.22))
RLQsum1.22


#Test significance of total inertia (link between Traits(Q) and Env(R))
F12 <- randtest(rlq1.22, modeltype = 6, nrepet = 49999) #Model 2 significant (p=0.00004), but model 4 not (p=0.32560)
F12


#Test significance of total inertia (link between Traits(Q) and Env(R))
# F12.fishy <- randtest(rlq1.fishy, modeltype = 6, nrepet = 49999) #Model 2 significant (p=0.00004), but model 4 not (p=0.32560)
# F12.fishy

####### Extract Coords for GGplot #############

#Enviro coords dataframe from column l1
rlq.Casc22.ENV <- rlq1.22$l1
names(rlq.Casc22.ENV) <- c("CS1","CS2")
s.arrow(rlq1.22$l1)
s.label(rlq1.22$mQ)

#April 13th
rlq.Casc22.ENV <- rlq1.22$l1
names(rlq.env.apl13) <- c("CS1","CS2")
s.arrow(rlq1.22$l1)
s.label(rlq1.22$mQ)
### Remove FISH YES condition as its partner is the significant one
rlq.Casc22.ENV <- rlq.Casc22.ENV[-2,]

row.names(rlq.Casc22.ENV) <- c("FISH.Absence", "TEMP", "TDN", "TDP", "DOC")

# Trait coordinates from column c1
rlq.traits.22 <- rlq1.22$c1
row.names(rlq.traits.22) <- c("Carnivore", "Herbivore", "Omnivore", "Filter Feeding", "Raptorial", "Large Body", "Medium Body", "Small Body", "Facultative Asexual", "Obligate Sexual", "No Pigment", "Pigmented", "Rapid Movement", "Slow Movment")
rlq.traits.22$Group <- as.factor(c("1", "1", "1", "2", "2", "3", "3", "3", "4", "4", "5", "5", "6", "6"))

#April 13th
rlq.traits.22 <- rlq1.22$c1
row.names(rlq.traits.22) <- c("Carnivore", "Herbivore", "Omnivore", "Filter Feeding", "Raptorial", "Large Body", "Medium Body", "Small Body", "Facultative Asexual", "Obligate Sexual", "No Pigment", "Pigmented", "Rapid Movement", "Slow Movment")
rlq.traits.22$Group <- as.factor(c("1", "1", "1", "2", "2", "3", "3", "3", "4", "4", "5", "5", "6", "6"))
levels(rlq.traits.22$Group) <- c("1"  = "Trophic Level","2" = "Feeding Method", "3" = "Body Size", "4" = "Reproduction", "5" = "Pigmentation", "6" = "Motility")

str(rlq.traits.22)
#Species coords from lQ
rlq.spp.22 <- rlq1.22$mQ
names(rlq.spp.22) <- c("CS1","CS2")

