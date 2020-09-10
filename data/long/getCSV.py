import os
import pandas as pd
import datetime
import numpy as np
import shutil
import sys

month = 8
day = 26

directory = 'C:/master/mstudy/data/FTP/lowSIMcomp/'
inputfiles = [directory + i for i in os.listdir(directory)]

#inputfiles = ['C:/master/mstudy/data/FTP/10msFTP/exam1.txt']
outputfile = 'C:/master/mstudy/data/FTP/lowSIMcomp/log-' + str(month) + str(day) + '.txt'

log = pd.DataFrame(
    [],
    columns=[
        'datetime',
        'ping'
        ])

for inputfile in inputfiles:
    with open(inputfile,'r') as fr:
        addlog = []
        count = -1
        for line in fr.read().splitlines():
            data = line.split()
            if not data:
                ''
            elif data[0][0:4] == '2020':
                addlog.append(data[0] + ' ' + data[1])
                count = 5
            elif count == 0 and data[0] == 'rtt':
                try:
                    addlog.append(data[3].split('/')[0])
                except:
                    print addlog
                    addlog.append(np.nan)
                log = log.append(pd.DataFrame(addlog, index=log.columns).T)
                addlog = []
            elif data[0] == 'rtt':
                print inputfile + ' : ' + line
            count -= 1

    log = log.sort_values('datetime')
    log = log.reset_index(drop=True)

start = datetime.datetime(2020, month, day, 0, 0, 0, 0)
end = datetime.datetime(2020, month, day, 23, 59, 59, 99)

with open(outputfile, 'w') as fw:
    fw.write('datetime,ping\n')
    for i in range(len(log)):
        x = datetime.datetime.strptime(log['datetime'][i], '%Y-%m-%d %H:%M:%S')
        if (x >= start and x <= end):
            fw.write(log['datetime'][i] + ',' + log['ping'][i] + '\n')