require(ggplot2)
allData <- read.csv(file="StatFile2.txt",head=TRUE,sep=" ")
allData$LogCycles <- log10(allData$Cycles)
allData$LogTime <- log10(allData$Time)
clause6Data <-subset(allData, Clauses==15)
summary(allData)
for (i in 1:4) {
  for (j in 1:4) {
    for (w in 3:8) {
      
    }
  }
}
boxplot(allData$Time)
boxplot(allData$Cycles)
boxplot(allData$LogCycles)
boxplot(allData$LogTime)
plot(allData$Constants, allData$LogTime)
boxplot(LogTime~Functions, data=clause6Data)
cor(clause6Data$LogTime, clause6Data$Clauses)
cor(clause6Data$LogTime, clause6Data$Constants)
cor(clause6Data$LogTime, clause6Data$Functions)
