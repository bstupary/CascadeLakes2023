##### Install / Load Packages #####
library(vegan)
library(ggplot2)
library(ggalt)
library(ggforce)
library(ggrepel)
library(ggpmisc)
library(FD)


#Set Working Directory
setwd("C:/Users/Blake/Desktop/CASCADE 2022/CWMs NMDS 2022")

#Import Workspace from CWMs May 2020


############ Import Data From wd #######################

Casc22.abnd <- read.csv("C:/Users/Blake/Desktop/CASCADE 2022/CWMs NMDS 2022/Casc22.abundance.csv", row.names=1, stringsAsFactors=TRUE)

Casc22.traits <- read.csv("C:/Users/Blake/Desktop/CASCADE 2022/CWMs NMDS 2022/CWMs May 2020/CascTraits Feb2020.csv", row.names=1, stringsAsFactors=TRUE)

############# OR load env ##########################


############## Convert abundance data.frame to matrix, as functcomp needs a = matrix ##############
Abund.matrix <- data.matrix(Casc22.abnd, rownames.force = TRUE)


################ Calculate Community Weighted Trait Means ################
# Aim to have a data.frame that links sites with numeric values per trait type

str(CascTraits.2020)
str(Abund.matrix)

CWM.traits.all <- functcomp(x = Casc22.traits,
                            a = Abund.matrix,
                            CWM.type = "all")

write.csv(CWM.traits.all, file = "CWM22 of traits (all possible).csv ")
#returned each variation of trait group as "species" column, w/ numbers in cells

CWM.traits.dom <- functcomp(x = Casc22.traits,
                            a = Abund.matrix,
                            CWM.type = "dom")
write.csv(CWM.traits.dom, file = "CWM22 of traits (dominant trait returned).csv ")
#returned each trait group as "species" column, w/ dominant trait factor in cells
