library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)
library(kernlab)
library(tsne)
library(Rtsne)
#library(dimRed)

pmP<-function(perplexity,lb,distM,clustAssign) {
  if (perplexity == 10) {
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
   xlab("") + ylab(lab) + ggtitle(perplexity) +
   scale_colour_manual(values = colors) +
   theme(legend.position="none") +
   theme(plot.title = element_text(hjust = 0.5))
   #guides(colour = guide_legend("Initial Clusters"))
  return(myplot)
}

plotRowP<-function(l,d,c) {
 perplexities = c(10, 30, 50, 70, 90, 110)
 pl<-lapply(perplexities, pmP, lb=l, distM=data.frame(d), clustAssign=c)
 return(pl)
}

scatterPlotPerplexity<-function(path1, path2, pathO) {
    numClust<-4
    labels<-c("Combined\nWith Gaps\nNA Cost = 0","Combined\nWith Gaps\nNA Cost = 2",
            "Combined\nNo Gaps\nNA Cost = 0","Combined\nNo Gaps\nNA Cost = 2",
            "Binary\nWith Gaps\nNA Cost = 0","Binary\nWith Gaps\nNA Cost = 2",
            "Binary\nNo Gaps\nNA Cost = 0","Binary\nNo Gaps\nNA Cost = 2",
            "Nominal\nWith Gaps\nNA Cost = 0","Nominal\nWith Gaps\nNA Cost = 2",
            "Nominal\nNo Gaps\nNA Cost = 0","Nominal\nNo Gaps\nNA Cost = 2")



    rows<-c(1:12)

    filename<-paste0(path1,"clusterAssignmentInitial",numClust,".RData")
    load(file=filename)
    perplexity="30"
    filename<-paste0(path2,"tsneResults",perplexity,".RData")
    load(file=filename)



    myPlots<-list()

    myPlots<-mapply(plotRowP,labels[1:4],disttsne[1:4],cl.wardCluster[1:4])
    myPlots<-t(myPlots)
    ml <- marrangeGrob(myPlots, nrow=4, ncol=6)
    filename<-paste0(pathO,"combined.pdf")
    ggsave(filename, ml, width=21, height=16)

    myPlots<-mapply(plotRowP,labels[5:8],disttsne[5:8],cl.wardCluster[5:8])
    myPlots<-t(myPlots)
    ml <- marrangeGrob(myPlots, nrow=4, ncol=6)
    filename<-paste0(pathO,"binary.pdf")
    ggsave(filename, ml, width=21, height=16)

    myPlots<-mapply(plotRowP,labels[9:12],disttsne[9:12],cl.wardCluster[9:12])
    myPlots<-t(myPlots)
    ml <- marrangeGrob(myPlots, nrow=4, ncol=6)
    filename<-paste0(pathO,"nominal.pdf")
    ggsave(filename, ml, width=21, height=16)
}

scatterPlotPerplexityCombined<-function(path1, path2, pathO) {
  numClust<-4
  labels<-c("Combined\nWith Gaps\nNA Cost = 0","Combined\nWith Gaps\nNA Cost = 2",
            "Combined\nNo Gaps\nNA Cost = 0","Combined\nNo Gaps\nNA Cost = 2",
            "Binary\nWith Gaps\nNA Cost = 0","Binary\nWith Gaps\nNA Cost = 2",
            "Binary\nNo Gaps\nNA Cost = 0","Binary\nNo Gaps\nNA Cost = 2",
            "Nominal\nWith Gaps\nNA Cost = 0","Nominal\nWith Gaps\nNA Cost = 2",
            "Nominal\nNo Gaps\nNA Cost = 0","Nominal\nNo Gaps\nNA Cost = 2")



  rows<-c(1:12)

  filename<-paste0(path1,"clusterAssignmentInitial",numClust,".RData")
  load(file=filename)
  perplexity="30"
  filename<-paste0(path2,"tsneResults",perplexity,".RData")
  load(file=filename)



  myPlots<-list()

  myPlots<-mapply(plotRowP,labels[1:4],disttsne[1:4],cl.wardCluster[1:4])
  myPlots<-t(myPlots)
  ml <- marrangeGrob(myPlots, nrow=4, ncol=6)
  filename<-paste0(pathO,"combined.pdf")
  ggsave(filename, ml, width=21, height=16)
  dev.off()

  myPlots<-mapply(plotRowP,labels[5:8],disttsne[5:8],cl.wardCluster[1:4])
  myPlots<-t(myPlots)
  ml <- marrangeGrob(myPlots, nrow=4, ncol=6)
  filename<-paste0(pathO,"binary.pdf")
  ggsave(filename, ml, width=21, height=16)
  dev.off()

  myPlots<-mapply(plotRowP,labels[9:12],disttsne[9:12],cl.wardCluster[1:4])
  myPlots<-t(myPlots)
  ml <- marrangeGrob(myPlots, nrow=4, ncol=6)
  filename<-paste0(pathO,"nominal.pdf")
  ggsave(filename, ml, width=21, height=16)
  dev.off()
}

path1 <- "./inst/extdata/output/clusterAssignInitial/"
path2 <- "./inst/extdata/input/tsnePy500/"
pathO <- "./plots/scatterPlotsPerplexity/"
scatterPlotPerplexity(path1,path2,pathO)
