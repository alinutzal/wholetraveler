library(TraMineR)
#library(purrr)
#library(dplyr)

readInputData <- function(pathInput, pathOutput) {
fileNames<-c("totalFUSmall","childSmall", "empSmall","highSmall", "marSmall")

gapPath = paste0(pathInput,"flat/")
nogapPath = paste0(pathInput,"flatNoGap/")

#age <- read.csv("./data/input/flat/ageSmall.csv",check.names=F,
#                header=TRUE,sep=",")
#gender <- read.csv("./data/input/flat/genderSmall.csv",check.names=F,
#                   header=TRUE,sep=",")

#filename<-"./data/age.RData"
#save(age,file=filename)
#filename<-"./data/gender.RData"
#save(gender,file=filename)


#filesGap
fileWithPath<-paste0(gapPath,fileNames,".csv")
filesGap<-lapply(fileWithPath,read.csv,check.names=F,header=TRUE,sep=",")
filesGap <- lapply(filesGap,function(x) x[,3:43])
filesGap[[1]][filesGap[[1]]>6] <- 6


#filesNoGap
fileWithPath<-paste0(nogapPath,fileNames,".csv")
filesNoGap<-lapply(fileWithPath,read.csv,check.names=F,header=TRUE,sep=",")
filesNoGap <- lapply(filesNoGap,function(x) x[,3:43])
filesNoGap[[1]][filesNoGap[[1]]>6] <- 6

tfu.labels <- c("Family of 1","Family of 2","Family of 3","Family of 4","Family of 5","Family >= 6")
tfu.states <- c("1","2","3","4","5",">= 6")

filesGap.seq <- lapply(filesGap,seqdef,xtstep = 5,start=20,right=NA)
filesGap.seq[[1]] <- seqdef(filesGap[[1]],xtstep = 5,start=20,right=NA,states=tfu.states,
                            labels=tfu.labels)

filesNoGap.seq <- lapply(filesNoGap,seqdef,xtstep = 5,start=20,right=NA)
filesNoGap.seq[[1]] <- seqdef(filesNoGap[[1]],xtstep = 5,start=20,right=NA,states=tfu.states,
                              labels=tfu.labels)


dataChannels <-list(
                    filesGap.seq,filesGap.seq,
                    filesNoGap.seq,filesNoGap.seq,
                    filesGap.seq[3:5],filesGap.seq[3:5],
                    filesNoGap.seq[3:5],filesNoGap.seq[3:5],
                    filesGap.seq[1:2],filesGap.seq[1:2],
                    filesNoGap.seq[1:2],filesNoGap.seq[1:2]
                    )
l5<-list("TRATE","TRATE","TRATE","TRATE","TRATE")
l3<-list("TRATE","TRATE","TRATE")
l2<-list("TRATE","TRATE")
smList <- list(l5,l5,l5,l5,l3,l3,l3,l3,l2,l2,l2,l2)
naCost <- list(0,2,0,2,0,2,0,2,0,2,0,2)

dataFile<-paste0(pathOutput,"sequences.RData")
save(filesGap.seq,filesNoGap.seq,file=dataFile)

## Using transition rates to compute substitution costs on each channel
mcdist.om1<-mapply(seqdistmc, channels=dataChannels, sm=smList, miss.cost=naCost,
                   MoreArgs=list(method="OM",with.missing = T,norm="none"),SIMPLIFY = FALSE)
dataFile<-paste0(pathOutput,"distances.RData")
save(mcdist.om1,file=dataFile)


rm(list=ls())
}

#pathI = paste0("./inst/extdata/input/")
#pathO = paste0("./data/")
#readInputData(pathI,pathO)
