'DATA PREPARATION'

library(fpc) #for cluster statistics
library(mlr) #for creating dummy variables
library(psych) #for factor analysis
library(cluster) #for silhouette plot
library(RColorBrewer) #for coloring silhouette plot

#read in data
setwd("/Users/Bisa/Documents/Studium/Masterstudium/3. Semester/CACI I/Final Assignment")

idList <- read.csv("idListJeans.csv")
mxl_betai <- read.csv("mxl_betaiJeans.csv")
indivData <- read.csv("indivDataJeans.csv")
MarketSimulation <- read.csv("marketsimulationJeans.csv")
id <- idList$X576309 # REPLACE WITH YOUR STUDENT ID 
id <- as.data.frame(id)
mxl_betai <- merge(mxl_betai,id, by="id")
indivData <- merge(indivData, id, by="id")
# check the dimensions
nrow(indivData) == 650 # the answer must be “TRUE”
nrow(mxl_betai) == 650 # the answer must be “TRUE”

mxl_betai <- cbind(mxl_betai[,1], mxl_betai[,10:11],mxl_betai[,2:9])
colnames(mxl_betai)[1] <- "id"

rm(idList, id)

