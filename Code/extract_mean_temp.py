#!/usr/bin/env python3

"""Script getting mean temperature for required grid squares from daily txt
files and producing combined csv files
Author: David Bridgwood"""

__author__ = 'David Bridgwood (dmb2417@ic.ac.uk)'

# imports
import csv
import glob
import os

################################################################################
#
################################################################################

os.chdir("../Data/Climate_Source")

prefix = "MeanTemp_"

temp_array = []
MeanTemp_array = []

for yr in range(1960,2016):
    print("Working " + str(yr))
    os.chdir(str(yr))
    for file in sorted(glob.glob("*.txt")):
        temp_array = []
        print("Processing: " + file)

        with open(file, 'r') as csvfile:
            spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')
            for row in spamreader:
                temp_array.append(row)

		#Temp_array now contains the day's data
        for x in range(3):
            MeanTemp_array.append([])
            for y in range(3):
                MeanTemp_array[x].append([])
                date = file.split("_")[4]
                MeanTemp_array[x][y].append([131+x, 157+y, date[0:4], date[4:6],
                                             date[6:8], temp_array[y+162][x+130]])

    os.chdir("../")

os.chdir("../Climate")

for x in range(3):
	for y in range(3):
		with open(prefix + str(x+131) + '-' + str(y+157) + '.csv', 'w') as csvfile:
			spamwriter = csv.writer(csvfile, delimiter=',')
			for i in MeanTemp_array[x][y]:
				spamwriter.writerow(i)

i = 1

for filename in sorted(glob.glob("*.csv")):
    os.rename(filename, prefix + str(i), + "csv")
    i = i+1

os.chdir("../../Code")

print("Finished")
