
KB = []
query = []

def readData(data):
    f = open(data, 'r')
    for line in f:
        s = line.split()
        sentence = s[:2]
        sentence.append([s[2],s[4]])
        KB.append(sentence)

def convertToCNF(knowledge):
    for c in knowledge:
        while 
    
def resolution(knowlegdeBase, query):
    CNF = convertToCNF(knowledgeBase)
    


def resolve(clause1, clause2):

    pass

resolution(KB, query)
