#### Load your Packages #####
library(vegan)
library(dplyr)

###### Set up your WD and load your Workspace #######

setwd("C:/Users/Blake Stuparyk/Desktop/CASCADE 2022")



###### Load up abundance ######

Casc22.abnd <-
  read.csv(
    "C:/Users/Blake Stuparyk/Desktop/CASCADE 2022/SEPT 28 - CORRECT CASC DENSITIES (Historical and 2022).csv",
    row.names = 1,
    stringsAsFactors = TRUE
  )

View(Casc22.abnd)

rm.row <- "Sno78"

Casc22.abnd.78rm <- Casc22.abnd[!(row.names(Casc22.abnd) %in% rm.row),]





###### Run NMDS #####
set.seed(42)

Global.Casc.NMDS.rm <- metaMDS(Casc22.abnd.78rm,
                            distance = 'bray',
                            k = 2,
                            trymax = 1000
                            )
Global.Casc.NMDS.rm #



BCdist <- as.matrix(vegdist(Casc22.abnd.78rm))



Casc22.78rm.site.coords

write.csv(Sites22.rm, "Sites22.rm.csv", row.names = F)


