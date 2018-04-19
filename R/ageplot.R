library(dplyr)

plotAges<-function(cl.mward4,lb,p) {

clust1=which(cl.mward4==1,arr.ind = T)
clust2=which(cl.mward4==2,arr.ind = T)
clust3=which(cl.mward4==3,arr.ind = T)
clust4=which(cl.mward4==4,arr.ind = T)

age1=age[clust1,"BY"]
age2=age[clust2,"BY"]
age3=age[clust3,"BY"]
age4=age[clust4,"BY"]
#countsDF<-data.frame(matrix(, nrow=0, ncol=22))

counts1<-as.data.frame(table(age1))
counts1<-t(counts1)
colnames(counts1)<-counts1[1,]
counts1<-data.frame(counts1)
#countsDF<-bind_rows(countsDF,counts1)

counts2<-as.data.frame(table(age2))
counts2<-t(counts2)
colnames(counts2)<-counts2[1,]
counts2<-data.frame(counts2)

counts3<-as.data.frame(table(age3))
counts3<-t(counts3)
colnames(counts3)<-counts3[1,]
counts3<-data.frame(counts3)

counts4<-as.data.frame(table(age4))
counts4<-t(counts4)
colnames(counts4)<-counts4[1,]
counts4<-data.frame(counts4)


countsDF<-bind_rows(counts1,counts2,counts3,counts4)
countsDFnew<-as.matrix(countsDF[c(2,4,6,8),])
countsDFnew<-apply(countsDFnew, 2, as.numeric)

countsDFnew <- countsDFnew[,colSums(is.na(countsDFnew))<nrow(countsDFnew)]
countsDFnew[is.na(countsDFnew)] <- 0
countsDFnew<-countsDFnew[,order(colnames(countsDFnew))]
colnames(countsDFnew)<-c( "1931","1932","1933","1934","1935","1936","1937", "1938",
                       "1939", "1940", "1941", "1942", "1943", "1944", "1945", "1946", "1947",
                       "1948", "1949", "1950", "1951", "1952")

filen<-paste0(p,"ageDistInitial",lb,".png")
png(filename=filen,
    type="cairo",
    units="in",
    width=6,
    height=4,
    pointsize=12,
    res=90
)
par(mar=c(4,4.2,3,0.2))
colAD <- palette(rainbow(4)) #heat.colors(4, alpha = 1) #brewer.pal(4,"Blues")
barplot(countsDFnew,col=colAD,beside=FALSE, legend=T,font=2,xlab="Birth Year", ylab = "Frequency",font.lab=2,cex.lab = 1.5,ylim=c(0,120),axes=FALSE)
axis(2,at=seq(0,120,20),labels=seq(0,120,20),pos=0,font=2)
axis(side=1,at=seq(131,151,10),labels=seq(131,151,10),font=2)
title(main = list(lb, font = 4))
legend("topleft", c("Cluster 1","Cluster 2","Cluster 3","Cluster 4"), cex=1.3, bty="n", fill=colAD)
dev.off()
}

plotAgeBarPlots<-function(path1,path2,pathO) {
    numClust=4

    labels <- list(
      "Combined GAP NAcost=0","Combined GAP NAcost=2",
      "Combined No GAP NAcost=0","Combined No GAP NAcost=2",
      "Binary GAP NAcost=0","Binary GAP NAcost=2",
      "Binary No GAP NAcost=0","Binary No GAP NAcost=2",
      "Nominal GAP NAcost=0","Nominal GAP NAcost=2",
      "Nominal No GAP NAcost=0","Nominal No GAP NAcost=2"
    )
    filename<-paste0(path1,"clusterAssignmentInitial",numClust,".RData")
    load(file=filename)
    plotsa<-mapply(plotAges,cl.wardCluster,labels,MoreArgs=list(p=pathO))
    filename<-paste0(path2,"clusterAssignmentTsne",numClust,".RData")
    load(file=filename)
    plotsa<-mapply(plotAges,cl.wardCluster_tsne,labels,MoreArgs=list(p=pathO))
}

path1<-"./inst/extdata/output/clusterAssignInitial/"
path2 <- "./inst/extdata/output/clusterAssignTsnePy500/"
pathO <- "./plots/6.ageBarPlots/"
plotAgeBarPlots(path1,path2,pathO)


