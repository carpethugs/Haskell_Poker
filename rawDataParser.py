import os.path
import json
import sys
#reference linke
#https://stackabuse.com/reading-and-writing-json-to-a-file-in-python/


#first argument: New of file in RawData folder (do not add RawData/)
#seond argument: the selector of langue (0=small, 1=elixir)

def readFileOnChar(typeCheck):

    charWrap=''
    if(typeCheck==0):#smalltalk 
        charWrap ='('
    elif(typeCheck==1):#elixir
        charWrap ='['
    else:
        print("exiting, typeCheck incorrect")
        exit()

    data = {}
    data["data"] = []
    if(len(sys.argv) <2):
        print("insufficent amount of arguments")
        exit()
    filePath = "RawData/" + sys.argv[1]
    parseFile = open(filePath,"r")
    lines = parseFile.readlines()
    for lineNum in range(len(lines)-1):
        line = lines[lineNum]
        if( line.find("FAILED") != -1 and line.find("Error") == -1):
            mainLine = line[(line.find(charWrap))+1:-2]
            if(typeCheck==1):#elixir 
                mainLine = mainLine.replace(",","")
            mainLine = mainLine.split(' ') 
            saidLine = lines[lineNum+1]
            whatYouSaid = saidLine[(saidLine.find(charWrap))+1:-2]
            if(typeCheck ==0):#smalltalk
                whatYouSaid = whatYouSaid.replace("'","")
            if(typeCheck==1):#elixir 
                whatYouSaid = whatYouSaid.replace('"',"")
                whatYouSaid = whatYouSaid.replace(",","")
            whatYouSaidClean = whatYouSaid.split(' ')
            corLine = lines[lineNum+2]
            correctAnswer = corLine[(corLine.find(charWrap))+1:-2]
            if(typeCheck ==0):#smalltalk
                correctAnswer = correctAnswer.replace("'","")
            if(typeCheck==1):#elixir 
                correctAnswer = correctAnswer.replace('"',"")
                correctAnswer = correctAnswer.replace(",","")
            correctAnswerClearn = correctAnswer.split(' ')
            data["data"].append({
                'input' : mainLine,
                'answer' : correctAnswerClearn,
                'whatWasSaid' : whatYouSaidClean
            })

    return data

def readSmallTalkFile():
    data = {}
    data["data"] = []
    if(len(sys.argv) <2):
        print("insufficent amount of arguments")
        exit()
    filePath = "RawData/" + sys.argv[1]
    parseFile = open(filePath,"r")
    lines = parseFile.readlines()
    for lineNum in range(len(lines)-1):
        line = lines[lineNum]
        if( line.find("FAILED") != -1):
            mainLine = line[(line.find('('))+1:-2]
            mainLine = mainLine.split(' ') 
            saidLine = lines[lineNum+1]
            whatYouSaid = saidLine[(saidLine.find('('))+1:-2]
            whatYouSaidClean = whatYouSaid.replace("'","")
            whatYouSaidClean = whatYouSaidClean.split(' ')
            corLine = lines[lineNum+2]
            correctAnswer = corLine[(corLine.find('('))+1:-2]
            correctAnswerClearn = correctAnswer.replace("'",'')
            correctAnswerClearn = correctAnswerClearn.split(' ')
            #print("Main line: " + str(mainLine) )
            #print("sec: " + str(whatYouSaidClean))
            #print("third line: " + str(correctAnswerClearn))
            data["data"].append({
                'input' : mainLine,
                'answer' : correctAnswerClearn,
                'whatWasSaid' : whatYouSaidClean
            })

    return data

def appendOddData(data, pathToFile):
    with open(pathToFile) as jsonFile:
        oldData = json.load(jsonFile)
        data["data"].extend(oldData["data"])

def writeOutputToUniqueFile(data):
    #output to unqie file
    outputPath = "Output/out"
    pathNumber = 0

    while ( os.path.isfile(outputPath + str(pathNumber) + ".txt") and pathNumber < 20):
        print("There is already a file at:" + str(pathNumber) )
        pathNumber+=1
    
    if(pathNumber>0):
        appendOddData(data, outputPath + str(pathNumber-1) + ".txt")

    with open(outputPath + str(pathNumber) + ".txt", 'w') as outfile:
        json.dump(data, outfile)


#newData = readSmallTalkFile()
newData = readFileOnChar(int(sys.argv[2]))
writeOutputToUniqueFile(newData)

print("easy 100")
#time.sleep(5)



