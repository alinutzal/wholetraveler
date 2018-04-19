library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)
library(kernlab)
library(tsne)
library(Rtsne)
#library(dimRed)

pm<-function(method,lb,distM,clustAssign) {
  if (method == 'Kernel PCA') {
    lab<-lb
  }
  else {
    lab<-''
  }
  numClust<-4
  colors=palette(rainbow(numClust))
  myplot<-ggplot(data=distM,aes(distM[,1],distM[,2],color=factor(clustAssign))) +
   geom_point(size=3,alpha=I(0.4)) +
   theme(axis.text=element_text(size=10,face="bold",color='black'),
         axis.title=element_text(size=12,face="bold"),
         title=element_text(size=12,face="bold", color='Black')) +
   xlab("") + ylab(lab) + ggtitle(method) +
   scale_colour_manual(values = colors) +
   theme(legend.position="none") +
   theme(plot.title = element_text(hjust = 0.5))
   #guides(colour = guide_legend("Initial Clusters"))
  return(myplot)
}

plotRow<-function(l,d,c) {
 methods = c('Kernel PCA', 'SE', 'MDS','t-SNE')
 pl<-lapply(methods, pm, lb=l, distM=data.frame(d), clustAssign=c)
 return(pl)
}

scatterPlotDimRed<-function(path1, path2, pathO) {
    numClust<-4
    labels<-c("Combined\nWith Gaps\nNA Cost = 0","Combined\nWith Gaps\nNA Cost = 2",
            "Combined\nNo Gaps\nNA Cost = 0","Combined\nNo Gaps\nNA Cost = 2",
            "Binary\nWith Gaps\nNA Cost = 0","Binary\nWith Gaps\nNA Cost = 2",
            "Binary\nNo Gaps\nNA Cost = 0","Binary\nNo Gaps\nNA Cost = 2",
            "Nominal\nWith Gaps\nNA Cost = 0","Nominal\nWith Gaps\nNA Cost = 2",
            "Nominal\nNo Gaps\nNA Cost = 0","Nominal\nNo Gaps\nNA Cost = 2")


    perplexities = c(10, 30, 50, 70, 90, 110)
    rows<-c(1:12)

    filename<-paste0(path1,"clusterAssignmentInitial",numClust,".RData")
    load(file=filename)
    perplexity="90"
    filename<-paste0(path2,"tsneResults",perplexity,".RData")
    load(file=filename)
    #dist1<-disttsne
    # filename<-paste0(path2,"KernelPCAResults.RData")
    # load(file=filename)
    # dist2<-disttsne
    # filename<-paste0(path2,"MDSResults.RData")
    # load(file=filename)
    # dist3<-disttsne
    # filename<-paste0(path2,"SEResults.RData")
    # load(file=filename)
    # dist4<-disttsne

    myPlots<-list()
    myPlots<-mapply(plotRow,labels[1:4],disttsne[1:4],cl.wardCluster[1:4])

    myPlots<-t(myPlots)
    ml <- marrangeGrob(myPlots, nrow=4, ncol=4)
    filename<-paste0(pathO,"combined.pdf")
    ggsave(filename, ml, width=15, height=16)

    myPlots<-mapply(plotRow,labels[5:8],disttsne[5:8],cl.wardCluster[5:8])
    myPlots<-t(myPlots)
    ml <- marrangeGrob(myPlots, nrow=4, ncol=4)
    filename<-paste0(pathO,"binary.pdf")
    ggsave(filename, ml, width=15, height=16)

    myPlots<-mapply(plotRow,labels[9:12],disttsne[9:12],cl.wardCluster[9:12])
    myPlots<-t(myPlots)
    ml <- marrangeGrob(myPlots, nrow=4, ncol=4)
    filename<-paste0(pathO,"nominal.pdf")
    ggsave(filename, ml, width=15, height=16)
}

#path1 <- "./inst/extdata/output/clusterAssignInitial/"
#path2 <- "./inst/extdata/input/dimRed/"
#pathO <- "./plots/scatterPlotsDimRed/"
#scatterPlotDimRed(path1,path2,pathO)
