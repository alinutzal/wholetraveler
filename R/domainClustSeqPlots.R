library(ggplot2)
library(RColorBrewer)
library(TraMineR)


setLayout<-function(matl, wi) {
  nl<-layout(matl, widths=wi, heights=c(1.5,1.5,1.5,1.5,1.5,0.7,0.3))
}

domainSeqPlot<- function(var2, lb, k, p, ini) {
  load(file="./data/gender.RData")
  labels <- list(
    "combined Gap NAcost=0","combined Gap NAcost=2",
    "combined NoGap NAcost=0","combined NoGap NAcost=2",
    "binary Gap NAcost=0","binary Gap NAcost=2",
    "binary NoGap NAcost=0","binary NoGap NAcost=2",
    "nominal Gap NAcost=0","nominal Gap NAcost=2",
    "nominal NoGap NAcost=0","nominal NoGap NAcost=2"
  )
  labelsshort <- list(
    "combined.gap.na0","combined.gap.na2",
    "combined.nogap.na0","combined.nogap.na2",
    "binary.gap.na0","binary.gap.na2",
    "binary.nogap.na0","binary.nogap.na2",
    "nominal.gap.na0","nominal.gap.na2",
    "nominal.nogap.na0","nominal.nogap.na2"
  )
  fileP<-paste0(pathP,ini,"/")
  ifelse(!dir.exists(file.path(fileP)), dir.create(file.path(fileP)), FALSE)
  
  filename<-paste0(p,ini,"/",labelsshort[match(lb,labels)],".pdf")
  pdf(filename,width = 11, height = 8.5,onefile=T)
  #load(file="./data/sequences.RData")
  if(match(lb,labels) %in% c(1,2,5,6,9,10)){
    var1<-filesGap.seq }
  else {
    var1<-filesNoGap.seq }

  par(mai=c(1.02,0.5,0.82,0.5)) #mar=c(4,12,4.5,2))
  if (k==2) {
    matL<-matrix(c(1:18,19,19,19),nrow = 7, ncol=3, byrow = TRUE)
    wid<-c(2.5,2.5,6)
  }
  else if (k==3) {
    matL<-matrix(c(1:24,25,25,25,25),nrow = 7, ncol=4, byrow = TRUE)
    wid<-c(2.5,2.5,2.5,3.5)
    }
  else if (k==4) {
    matL<-matrix(c(1:30,31,31,31,31,31),nrow = 7, ncol=5, byrow = TRUE)
    wid<-c(2.5,2.5,2.5,2.5,1)
    }
  else {
    matL<-matrix(c(1:36,37,37,37,37,37,37),nrow = 7, ncol=6, byrow = TRUE)
    wid<-c(2.5,2.5,2.5,2.5,2.5,1)
    }
  setLayout(matL,wid)

  messageAll<-paste("Gaps and cost of NAs: ", lb,
                    ", Number of Clusters: ", k)
  cat(paste(messageAll,"\n"))
  labels<-c("Total in FU", "Children under 8","Employment","High School", "Married")
  count=1
  for (j in 1:5) {
    temp.seq<-var1[[j]]
    if (j==1 || j==2) {cpal(temp.seq) <- brewer.pal(6,"Blues")}
    for (i in 1:k) {
      tfu.seqTemp<-temp.seq[which(var2==i),]
      par(mar=c(2,2.5,2,0.2))
      seqdplot(tfu.seqTemp,border = NA, main=paste("Cluster",i,"-", labels[count]),
               xtstep=5,with.legend=F,sortv="from.start",cex.lab=1.5, cex.axis=1.5,
               cex.main=1.4, cex.sub=1.5)
    }
    par(mar=c(1,.8,2,0))
    seqlegend(temp.seq, cex = 1)
    count=count+1
  }

  for (i in 1:k) {
    numb<-length(which(var2==i))
    percentpop<-signif(length(which(var2==i))/length(var2),4)
    numMen<-length(which(gender[which(var2==i),3]==1))
    numW<-length(which(gender[which(var2==i),3]==2))
    message<-paste0("     Count Cluster ",i,": ",numb,"\n     Percentage: ",
                    percentpop,"\n     Men: ",numMen," Women: ",numW,"\n")
    #cat(message)
    par(mar=c(0,0,0,0))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(.5,.5,message,cex=1.4,font=2)#,family="serif",font=2,cex=1.4)#,valign="top",halign="center",vadj=0)
  }
  plot.new()
  par(mar=c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(.5,.5,messageAll,cex=1.4,font=2)#,family="serif",font=2,cex=1.4)#,valign="top",halign="center",vadj=0)

  while(!par('page')) plot.new()  #go to new page

  count=1
  for (j in 1:5) {
    temp.seq<-var1[[j]]
    if (j==1 || j==2) {cpal(temp.seq) <- brewer.pal(6,"Blues")}
    for (i in 1:k) {
      tfu.seqTemp<-temp.seq[which(var2==i),]
      par(mar=c(2,2.5,2,0.2))
      #isort<-disttsne[[match(lb,labels)]][,1]
      seqIplot(tfu.seqTemp,border = NA, main=paste("Cluster",i,"-", labels[count]),xtstep=5,
               with.legend=F,sortv="from.start",cex.lab=1.5, cex.axis=1.5, cex.main=1.4, cex.sub=1.5)
    }
    par(mar=c(1,.8,2,0))
    seqlegend(temp.seq, cex = 1)
    count=count+1
  }

  for (i in 1:k) {
    numb<-length(which(var2==i))
    percentpop<-signif(length(which(var2==i))/length(var2),4)
    numMen<-length(which(gender[which(var2==i),3]==1))
    numW<-length(which(gender[which(var2==i),3]==2))
    message<-paste0("     Count Cluster ",i,": ",numb,"\n     Percentage: ",
                    percentpop,"\n     Men: ",numMen," Women: ",numW,"\n")
    #cat(message)
    par(mar=c(0,0,0,0))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(.5,.5,message,cex=1.4,font=2)#,family="serif",font=2,cex=1.4)#,valign="top",halign="center",vadj=0)
  }
  plot.new()
  par(mar=c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(.5,.5,messageAll,cex=1.4,font=2)#,family="serif",font=2,cex=1.4)#,valign="top",halign="center",vadj=0)
  #while(!par('page')) plot.new() #go to new page
  dev.off()
  return()
}

domainClustSeqPlots <- function(numClust,pathI,pathP,initial=TRUE,tsne=TRUE) {
  labels <- list(
    "combined Gap NAcost=0","combined Gap NAcost=2",
    "combined NoGap NAcost=0","combined NoGap NAcost=2",
    "binary Gap NAcost=0","binary Gap NAcost=2",
    "binary NoGap NAcost=0","binary NoGap NAcost=2",
    "nominal Gap NAcost=0","nominal Gap NAcost=2",
    "nominal NoGap NAcost=0","nominal NoGap NAcost=2"
  )
  if (initial) {
    filename<-paste0(pathI,"clusterAssignInitial/clusterAssignmentInitial",numClust,".RData")
    load(file=filename)
    plotsm<-mapply(domainSeqPlot,var2=cl.wardCluster,lb=labels, MoreArgs=list(k=numClust,p=pathP,"initial"))
  }
  if (tsne) {
    filename<-paste0(pathI,"clusterAssignTsnePy1000/clusterAssignmentTsne",numClust,".RData")
    load(file=filename)
    plotsm<-mapply(domainSeqPlot,var2=cl.wardCluster_tsne,lb=labels, MoreArgs=list(k=numClust,p=pathP,"tsne"))
  }
}

allDomainClustSeqPlots <- function(pathI,pathP,lowerl,upperl) {
  allplots<-lapply(lowerl:upperl,function(x,pi,pp,y,z) domainClustSeqPlots(x,pi,pp,y,z), pi=pathI,pp=pathP,y=TRUE, z=TRUE)
}

#pathI<-"./inst/extdata/output/"
#pathP<-"./plots/tsnePy500/domainSeqPlots/"
#allDomainClustSeqPlots(pathI,pathP,2,5)

#load('./inst/extdata/output/clusterAssignTsnePy500/tsneDistances30.RData')
