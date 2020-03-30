import json
import os.path
import sys
#first arg is the name of input/data file (don't add Output/ )
#second arg is the name of your output file

testCase = ''
answers = ''

with open("Output/" + sys.argv[1]) as jsonFile:
    data = json.load(jsonFile)
    for case in data['data']:
        testCase+= str(case['input']).replace("'",'')  +','
        answers+= str(case['answer']).replace("'",'"') +','

writeFile = open(sys.argv[2],"w")
writeFile.write( testCase + '\n')
writeFile.write( answers)
