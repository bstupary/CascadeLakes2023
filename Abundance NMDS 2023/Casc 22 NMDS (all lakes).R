#### Load your Packages #####
library(vegan)


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


###### Run NMDS #####

Global.Casc.NMDS <- metaMDS(Casc22.abnd,
                            distance = 'bray',
                            k = 2,
                            trymax = 1000
                            )
Global.Casc.NMDS #
