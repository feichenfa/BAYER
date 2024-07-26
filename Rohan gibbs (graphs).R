# Rohan's notes on Berkley's Gibbs code- also with a more tidyverse approach, which Rohan prefers (maybe not)
# install.packages(c("HMPTrees", "akima"))

library(HMPTrees)
library(data.table)
library(tidyverse)
library(akima)


# read in data
# data2 <- fread("/mnt/DATA/Fei/cstr_functional_ontology_synethic_08192022.csv")
data <- fread("/mnt/DATA/Fei/cstr_functional_ontology_synethic_08242022.csv")

# split between normal and abnormal
nData <- data[Mode == "Normal"]
aData <- data[Mode == "Abnormal"]

# pull out a subset of the data
nodeCols <- c(2:7, 9)
tData <- t(data[, ..nodeCols])
nodeNData <- t(nData[, ..nodeCols])
nodeAData <- t(aData[, ..nodeCols])
nodeAData <- nodeAData[, -1]


# plot on an mds
# plotTreeDataMDS(list(Normal=nodeNData, Abnormal=nodeAData), main="Normal vs Abnormal:\nAll Samples")



# split into groups of 50
# numGrps <- c(5, 5)
numGrps <- c(90, 10)

gdata <- vector("list", numGrps[1])
temp <- rep(NA, numGrps[1])
for(i in 1:numGrps[1]){
  selCols <- 1:50 + (i-1)*50
  gdata[[i]] <- nodeNData[, selCols]
  temp[i] <- mean(gdata[[i]][1,])
  gdata[[i]] <- gdata[[i]][-1,]
}

gdata2 <- vector("list", numGrps[2])
temp2 <- rep(NA, numGrps[2])
for(i in 1:numGrps[2]){
  selCols <- 1:50 + (i-1)*50
  gdata2[[i]] <- nodeAData[, selCols]
  temp2[i] <- mean(gdata2[[i]][1,])
  gdata2[[i]] <- gdata2[[i]][-1,]
}
gdata <- c(gdata, gdata2)
names(gdata) <- paste(rep(c("Normal", "Abnormal"), numGrps), 1:numGrps)

temp <- c(temp, temp2)
names(temp) <- paste(rep(c("Normal", "Abnormal"), numGrps), 1:numGrps)

# plot colored by groups of 50
# plotTreeDataMDS(gdata, main="Normal vs Abnormal:\n All Samples by Groups")



# get gstar of each 50 group
gsdata <- data.frame(matrix(0, nrow(gdata[[1]]), sum(numGrps)))
for(i in 1:length(gdata))
  gsdata[,i] <- getMLEandLoglike(gdata[[i]])$mleTree

# plot on an mds of only gstars
cords <- plotTreeDataMDS(list(Normal=gsdata[,1:numGrps[1]], Abnormal=gsdata[,(numGrps[1]+1):sum(numGrps)]), 
                         main="Normal vs Abnormal:\n Group Gstars", returnCoords=TRUE)


# Get the data to plot ourselves
plotData <- data.frame(X=cords$x, Y=cords$y, Z=c(temp, NA, NA, NA)) 
mycol <- c(rep("red", numGrps[1]), rep("blue", numGrps[2]), "red", "blue", "black")
mycex <- c(rep(1.6, sum(numGrps)), rep(3, 3))

# get the temp gradient data
intData <- plotData[1:sum(numGrps),]
li <- interp(intData$X, intData$Y, intData$Z)

# get the text data
textData <- plotData[-c(1:sum(numGrps)),]

# plot the points
plot(plotData$X, plotData$Y, col=mycol, cex=mycex, pch=16, ylab="MDS 2", xlab="MDS 1", main="Normal vs Abnormal:\n Group Gstars")

# plot the temp gradient
image(li, add=TRUE, col = topo.colors(600, alpha=.25))
contour(li, add=TRUE, nlevels=6) 

# add text
text(textData$X, textData$Y, c("Normal G-Star", "Abnormal G-Star", "Combined G-Star"), pos=3)



# get pvalue
pval <- compareTwoDataSets(gsdata[,1:numGrps[1]], gsdata[,(numGrps[1]+1):sum(numGrps)])
pval

### get the gstar for the normal, abnormal, and all the data
normgs <- getMLEandLoglike(nodeNData)$mleTree
norm_Gstar <- getMLEandLoglike(nodeNData)
#normtau <- getMLEandLoglike(nodeNData)$tau
abnormgs <- getMLEandLoglike(nodeAData)$mleTree
#abnormtau <- getMLEandLoglike(nodeAData)$tau
allgs <- getMLEandLoglike(tData)$mleTree
#allTau <- getMLEandLoglike(tData)$tau




# get MLEs and tau
nRes <- getMLEandLoglike(nodeNData)
nmle <- nRes$mleTree
ntau <- nRes$tau
aRes <- getMLEandLoglike(nodeAData)
amle <- aRes$mleTree
atau <- aRes$tau

dist(rbind(normgs,abnormgs))



save.image(".RData")


