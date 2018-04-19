#initial clustering

clusterSave <- function(numClust,wardCluster,path) {
  labels <- list(
    "combined Gap NAcost=0","combined Gap NAcost=2",
    "combined NoGap NAcost=0","combined NoGap NAcost=2",
    "binary Gap NAcost=0","binary Gap NAcost=2",
    "binary NoGap NAcost=0","binary NoGap NAcost=2",
    "nominal Gap NAcost=0","nominal Gap NAcost=2",
    "nominal NoGap NAcost=0","nominal NoGap NAcost=2"
  )
  cl.wardCluster<-lapply(wardCluster,cutree, k=numClust)
  ifelse(!dir.exists(file.path(path)), dir.create(file.path(path)), FALSE)
  filename<-paste0(path,"clusterAssignmentInitial",numClust,".RData")
  save(cl.wardCluster,file=filename)
}

clustAssigInitial<-function(mcdist.om1,path) {
  mcdist.om1<-lapply(mcdist.om1, as.dist)
  wardCluster <- lapply(mcdist.om1, hclust, method="ward.D2")

  allplots<-lapply(2:5,function(x,y,z) clusterSave(x,y,z), y=wardCluster, z=path)
}


#pathO = paste0("./data/clusterAssigInitial/")
#load(file="./data/distances.RData")
#clustAssigInitial(mcdist.om1,pathO)



