library(rworldmap)
library(plotrix)
#give path to csv file

setwd("/path/to/workDirectory/")    #give the path to the work directory where you files are and/or where you would like to output files to go
inputfile.name = "spicatum_K4_popAverage.csv"
outputfile.name="spicatum_K4"

print.label = TRUE
pieChartSize = 0.4

inputfile <- read.csv(inputfile.name,header=FALSE, as.is=TRUE)

plot.piechart <- function(admixData, lab, pCS) {
  #plots map and axes
  worldmap <- getMap(resolution = "low")
  plot(worldmap, xlim = c(min(admixData[,2]), max(admixData[,2])), ylim = c(min(admixData[,3]), max(admixData[,3])))
  box(which="plot")
  axis(1)
  axis(2)
  
  bordCol = "black"
  
  chart.data <- admixData[4:ncol(admixData)]
  #plots the piecharts, admixData$AverK3_1 etc can be changed and extended if needed, example: admixData$AverK4_1[x],admixData$AverK4_2[x],admixData$AverK4_3[x],admixData$AverK4_4[x]
  #Just make sure the column names in your input csv matches those in the for loop! 
  for (x in 1:nrow(admixData)) {
    floating.pie(admixData[x,2], admixData[x,3], unlist(chart.data[x,]), 
                 radius=pCS, col=c("blue", "red", "orange", "darkgreen", "purple","brown","black"), lwd = 1, border = bordCol)
  
  }
  if (lab) {
    #function to put labels in map
    for (x in 1:nrow(admixData)) {
      text(admixData[x,2], admixData[x,3], labels=admixData[x,1], cex= 0.7, pos = 3, offset = 1)
    }
  }
}

pdf(file = paste(outputfile.name,".pdf",sep=""),width =10, height = 10)
plot.piechart(inputfile,print.label,pieChartSize)
dev.off()

png(filename = paste(outputfile.name,".png",sep=""),width =1000, height = 1000)
plot.piechart(inputfile,print.label,pieChartSize)
dev.off()