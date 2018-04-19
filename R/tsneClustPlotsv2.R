library(ggplot2)
#library(plotly)


scatterPlotTsne <- function(initial, tsne,lb,var1,k) {
    par(mar=c(4.5,4.5,4.5,4))
    var1<-data.frame(var1)

    colors <- palette(rainbow(k+1))
    myplot<-ggplot(data=var1,aes(var1[,1],var1[,2],color=factor(initial),shape=factor(tsne))) +
      geom_point(size=3) +
      theme(axis.text=element_text(size=16,face="bold",color='black'),
            axis.title=element_text(size=20,face="bold"),
            title=element_text(size=20,face="bold", color='Black')) +
            xlab("X") + ylab("Y") + ggtitle(paste("t-SNE ",lb,"k=",k)) +
      scale_colour_manual(values = colors) +
      guides(colour = guide_legend("Initial Clusters"), shape = guide_legend("t-SNE Clusters"))
    print(myplot)
    #myplot<-ggplotly(myplot)

}


numClust<-5
clustPlotsTsne <- function(numClust, df, tsne,p1,p2,p3,pO) {
  labels <- list(
                 "combined Gap NAcost=0","combined Gap NAcost=2",
                 "combined NoGap NAcost=0","combined NoGap NAcost=2",
                 "binary Gap NAcost=0","binary Gap NAcost=2",
                 "binary NoGap NAcost=0","binary NoGap NAcost=2",
                 "nominal Gap NAcost=0","nominal Gap NAcost=2",
                 "nominal NoGap NAcost=0","nominal NoGap NAcost=2"
                 )
  filename<-paste0(p1,"clusterAssignmentInitial",numClust,".RData")
  load(filename)
  filename<-paste0(p2,"clusterAssignmentTsne",numClust,".RData")
  load(file=filename)

  filename<-paste0(p3,"tsneResults30.RData")
  load(file=filename)

  pdf(paste0(pO,"tsnePlotk",numClust,".pdf"),width = 11, height = 8.5,onefile=T)
  plotsm<-mapply(scatterPlotTsne,cl.wardCluster,cl.wardCluster_tsne,labels,disttsne,MoreArgs=list(k=numClust))
  dev.off()
}



scatterPlotsTsne<-function(path1, path2, path3, pathO) {
allplots<-lapply(2:5,function(x,y,z,p1,p2,p3,pO) clustPlotsTsne(x,y,z,p1,p2,p3,pO), y=disttsne, z=stringtsne,
                 p1=path1, p2=path2, p3=path3, pO=pathO)
}


#path1 <- "./inst/extdata/output/clusterAssignInitial/"
#path2 <- "./inst/extdata/output/clusterAssignTsnePy500/"
#path3 <- "./inst/extdata/input/tsnePy500/"
#pathO <- "./plots/scatterPlotsTsne/"
#scatterPlotsTsne(path1,path2,path3,pathO)


