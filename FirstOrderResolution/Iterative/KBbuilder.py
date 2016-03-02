import random

variables = ["$A", "$B", "$C", "$D", "$E", "$F", "$G", "$H", "$I", "$J", "$K", "$L", "$M", "$N", "$O", "$P", "$Q", "$R", "$S", "$T", "$U", "$V", "$W", "$X", "$Y", "$Z"]

constants = ["DOG","CAT","MOUSE","SNAKE","FISH","WOLF","JOHN","ADAM","RICK","DAVID"]

functions = ["A","B","C","D","E","F","G","H","I","J","K","L"]
KB = []

for i in range(random.randint(5,20)):
    addClause()

def addClause():
    
