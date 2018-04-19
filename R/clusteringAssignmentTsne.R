
clustSaveTsne <- function(numClust,wardCluster_tsne, path) {
  labels <- list(
    "combined Gap NAcost=0","combined Gap NAcost=2",
    "combined NoGap NAcost=0","combined NoGap NAcost=2",
    "binary Gap NAcost=0","binary Gap NAcost=2",
    "binary NoGap NAcost=0","binary NoGap NAcost=2",
    "nominal Gap NAcost=0","nominal Gap NAcost=2",
    "nominal NoGap NAcost=0","nominal NoGap NAcost=2"
  )
  cl.wardCluster_tsne<-lapply(wardCluster_tsne,cutree, k=numClust)
  ifelse(!dir.exists(file.path(path)), dir.create(file.path(path)), FALSE)
  filename<-paste0(path,"clusterAssignmentTsne90",numClust,".RData")
  save(cl.wardCluster_tsne,file=filename)
}


#t-sne clustering

clustAssigTsne<-function(disttsne,pathO) {

  tsneBasedDist<-lapply(disttsne, dist)
  ifelse(!dir.exists(file.path(pathO)), dir.create(file.path(pathO)), FALSE)
  filename<-paste0(pathO,"tsneDistances90.RData")
  save(tsneBasedDist,file=filename)

  wardCluster_tsne <- lapply(tsneBasedDist, hclust, method="ward.D2")
  allplots<-lapply(2:5,function(x,y,z) clustSaveTsne(x,y,z),y=wardCluster_tsne,z=pathO)

}

#pathI <- paste0("./inst/extdata/input/tsnePy500/")
#pathO <- paste0("./inst/extdata/output/clusterAssignTsnePy500/")
#inputFile<-paste0(pathI,"tsneResults30.RData")
#load(inputFile)
#clustAssigTsne(disttsne,pathO)
