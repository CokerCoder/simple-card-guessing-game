# This program is used to calculate the average quality of the card-guessing game

with open('log.txt') as f:
    total = 0
    count = 0
    for line in f:
        if '=' in line:
            count = count + 1
            index1 = line.index('=')
            index2 = line.index('%')
            total = total + float(line[index1+2:index2])
    
    print("The average quality is "+"{0:.5f}".format(total/count))