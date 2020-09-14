setwd("/path/to/workDirectory/")    #give the path to the work directory where you files are and/or where you would like to output files to go
popmap="popmap_spicatum.txt"        #give the name of the popmap if it is in the work directory or the full path if it is somewhere else
outputname="spicatum"               #give the prefix for all of the output files
QFile="spicatum_K4_snmf.csv"        #give the name of the Qmatrix file that was created by my sNMF or TESS3 script
coordinates_file="coordinates.txt"  #give the name of the coordinates file if it is in the work directory or the full path if it is somewhere else


pop <- read.delim(popmap, header = FALSE)
pop_sorted<-pop[order(pop[,2]),]
popnames <- unique(as.character(pop[,2]))

coordinates <- read.delim(coordinates_file, sep="\t", header = FALSE) #loads in the coordinates file into a table
Qmatrix <- read.delim(QFile, sep=",", header = TRUE)

if (file.exists(paste(outputname,"_popAverage.csv",sep=""))) {
  file.remove(paste(outputname,"_popAverage.csv",sep=""))
}

p=2
while (p<length(coordinates[[1]])){
  AverList=list()
  i=1
  print(p)
  while (pop[[p-1,2]]==pop[[p,2]]){
    AverList[[i]]=Qmatrix[p-1,2:length(Qmatrix[1,])]
    p=p+1
    i=i+1
    print(p)
    if (p > length(coordinates[[1]])){
      break
    }
  }
  AverList[[i]]=Qmatrix[p-1,2:length(Qmatrix[1,])]
  
  x=1
  popmean <- vector()
  while (x <= length(AverList[[1]])-1) {
    y=1
    meanList <- vector()
    while (y <= length(AverList)) {
      meanList[y] <- unlist(AverList[[y]][x])
      y=y+1
    }
    popmean[x] <- mean(meanList)
    x=x+1
  }
  lastQ <- 1-sum(popmean)
  popmean[length(AverList[[1]])] <- lastQ
  
  output<-vector()
  output[1]<-as.character(pop[p-1,2])
  output[2]<-coordinates[p-1,1]
  output[3]<-coordinates[p-1,2]
  o=1
  while (o <= length(popmean)) {output[o+3]=popmean[o];o=o+1}
  cat(file = paste(outputname,"_popAverage.csv",sep=""),
            x=c(output), append = TRUE,sep = ",",fill = T)
  print("start")
  print(AverList)
  print("end")
  print(pop[p,2])
  p=p+1
}


