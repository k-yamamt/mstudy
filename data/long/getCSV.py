import os
import pandas as pd
import datetime
import numpy as np
import shutil
import sys

month = 7
day = 1

directory = 'C:/master/mstudy/data/long/data/'

inputfiles = [directory + i for i in os.listdir(directory)]

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
            elif data[0] == 'datetime':
                addlog.append(data[1] + ' ' + data[2])
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

outputfile = 'C:/master/mstudy/data/long/csv/' + str(month) + '-' + str(day) + '.csv'

with open(outputfile, 'w') as fw:
    fw.write('datetime,ping\n')
    for i in range(len(log)):
        x = datetime.datetime.strptime(log['datetime'][i], '%Y-%m-%d %H:%M:%S')
        if (x >= start and x <= end):
            fw.write(log['datetime'][i] + ',' + log['ping'][i] + '\n')