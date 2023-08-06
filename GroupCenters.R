library(devtools)
install_github("olobiolo/centroidr")
library(centroidr)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(centroidr,dbscan,fpc,jsonlite,shiny,GGally,WVPlots,ggplot2,SamSPECTRAL,ggrepel,ggpubr,gridExtra,mda)

df <- as.data.frame(training[,c(1,2,15,40,41,42,43,44)]) # takes id, group and MDA 1 to 5

# group centers

centroid(df[,3:7])

group.dist <- dist(as.matrix(df[,3:7]))

prototypes <- classCenter(df[,3:7], df[,2], group.dist)