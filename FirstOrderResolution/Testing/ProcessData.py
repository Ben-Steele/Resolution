import pylab
import numpy as np

data = []

def cleanData():
    dataFile = open("StatFile.txt", "r")
    writeFile = open("StatFile2.txt", "w")
    for line in dataFile:
        if  "%" in line:
            None
        else:
            line = line.replace(",", "")
            line = line.replace(" ", ",")
            writeFile.write(line)

cleanData()
            
def loadData():
    dataFile = open("StatFile.txt", "r")
    for line in dataFile:
        if  "%" in line:
            None
        else:
            line = line.split()
            line[6] = line[6].replace(",", "")
            data.append(line)
            


def makegraph():
    x1 = []
    y1 = []
    x2 = []
    y2 = []
    totalC = 0
    totalI = 0
    clockC = 0
    clockI = 0
    countC = 0
    countI = 0
    
    for line in data:
        if (line[0] == "TC") and (line[1] == "T"):
            x1.append(1)
            y1.append(line[5])
            totalC += float(line[5])
            clockC += int(line[6])
            countC += 1
        elif (line[0] == "TI") and (line[1] == "T"):
            x2.append(2)
            y2.append(line[5])
            totalI += float(line[5])
            clockI += int(line[6])
            countI += 1
    averageC = totalC / countC
    averageCC = clockC / countC
    averageI = totalI / countI
    averageCI = clockI / countI
    print("countI: " + str(countI))
    print("countC: " + str(countC))
    print("average Classic: " + str(averageC))
    print("average Iterative: " + str(averageI))
    print("average CC: " + str(averageCC))
    print("average CI: " + str(averageCI))
    pylab.plot(x1, y1, 'bo', label='classic')
    pylab.plot(x2, y2, 'ro', label='Iterative')
    pylab.legend()
    pylab.ylim(0, 0.5)
    pylab.xlim(0,3)
    pylab.show()


    
