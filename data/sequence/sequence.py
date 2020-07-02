import os
import pandas as pd
import datetime
import numpy as np
import shutil
import sys

directory = 'C:/master/mstudy/data/sequence/'
inputfile = directory + 'ex.txt'
outputfile = directory + 'sequence.csv'

with open(inputfile,'r') as fr:
    log = pd.DataFrame(
        [],
        columns=[
            'datetime',
            'ping'
            ])
    addlog = []
    count = -1
    for line in fr.read().splitlines():
        data = line.split()
        if not data:
            ''
        elif data[0][0:4] == '2020':
            addlog.append(line)
            count = 5
        elif data[0] == 'rtt':
            if count == 0:
                addlog.append(data[3].split('/')[0])
            else:
                print 'e'
            log = log.append(pd.DataFrame(addlog, index=log.columns).T)
            addlog = []
        count -= 1

    log = log.sort_values('datetime')
    log = log.reset_index(drop=True)

    with open (outputfile,'w') as fw:
        fw.write('datetime,ping\n')
        for i in range(len(log)):
            fw.write(log['datetime'][i] + ',' + log['ping'][i] + '\n')