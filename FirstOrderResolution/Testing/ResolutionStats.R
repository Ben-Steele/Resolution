require(ggplot2)
allData <- read.csv(file="StatFile2.txt",head=TRUE,sep=",")
allData$LogCycles <- log10(allData$Cycles)
allData$LogTime <- log10(allData$Time)
allData$miliseconds <- allData$Time * 1000
allData$logmiliseconds <- log10(allData$miliseconds)
TI <-subset(allData, Algorithm == "TI")
TC <-subset(allData, Algorithm == "TC")
TI1f <-subset(TI, Functions == 1)
TI2f <-subset(TI, Functions == 2)
TI3f <-subset(TI, Functions == 3)
TI4f <-subset(TI, Functions == 4)
TC4f <-subset(TC, Functions == 4)
TC3f <-subset(TC, Functions == 3)
TC2f <-subset(TC, Functions == 2)
TC1f <-subset(TC, Functions == 1)
TC1c <-subset(TC, Constants == 1)
TC2c <-subset(TC, Constants == 2)
TC3c <-subset(TC, Constants == 3)
TC4c <-subset(TC, Constants == 4)
TI1c <-subset(TI, Constants == 1)
TI2c <-subset(TI, Constants == 2)
TI3c <-subset(TI, Constants == 3)
TI4c <-subset(TI, Constants == 4)
summary(TI)
for (i in 1:4) {
  for (j in 1:4) {
    for (w in 3:8) {
      
    }
  }
}
boxplot(allData$Time)
boxplot(TI$Cycles, ylim = c(0,20000000))
boxplot(TC$Cycles, ylim = c(0,20000000))
boxplot(allData$LogCycles)
boxplot(allData$LogTime)
averageTI <- aggregate(Cycles~Functions, data=TI, FUN=function(TI) c(mean=mean(TI)))
averageTC <- aggregate(Cycles~Functions, data=TC, FUN=function(TC) c(mean=mean(TC)))
plot(averageTI$Functions, averageTI$Cycles, ylim = c(0,80000000), main = "Functions vs Cycles", ylab = "Cycles", xlab = "Functions", type = "n")
lines(averageTI$Functions, averageTI$Cycles, type="b", col="blue")
lines(averageTC$Functions, averageTC$Cycles, type="b", col="red")
legend(3.5,79000000, c("Iterative","Classic"), lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red"))
plot(TI$Clauses, allData$LogCycles)
abline(reg1)
abline(h=-1)
boxplot(TI4f$Cycles~TI4f$Clauses, ylim = c(0,80000000))
cor(clause6Data$LogTime, clause6Data$Clauses)
cor(clause6Data$LogTime, clause6Data$Constants)
cor(clause6Data$LogTime, clause6Data$Functions)
