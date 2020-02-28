import os
import pandas as pd
import datetime
import numpy as np
import shutil
import sys

files = []
for file in os.listdir('./'):
    if file[-4:] == '.txt':
        files.append(file)

for file in files:
    with open(file,'r') as fr:
        log = pd.DataFrame(
            [],
            columns=['datetime',
                '1st-id','1st-rtt',
                '2nd-id','2nd-rtt',
                '3rd-id','3rd-rtt',
                '4th-id','4th-rtt',
                '5th-id','5th-rtt',
                '6th-id','6th-rtt',
                '7th-id','7th-rtt',
                '8th-id','8th-rtt',
                '9th-id','9th-rtt',
                '10th-id','10th-rtt',
                '11th-id','11th-rtt',
                '12th-id','12th-rtt',
                '13th-id','13th-rtt',
                '14th-id','14th-rtt',
                '15th-id','15th-rtt',
                '16th-id','16th-rtt',
                '17th-id','17th-rtt',
                '18th-id','18th-rtt',
                '19th-id','19th-rtt',
                '20th-id','20th-rtt',
                '21th-id','21th-rtt',
                '22th-id','22th-rtt',
                '23th-id','23th-rtt',
                '24th-id','24th-rtt',
                '25th-id','25th-rtt',
                '26th-id','26th-rtt',
                '27th-id','27th-rtt',
                '28th-id','28th-rtt',
                '29th-id','29th-rtt',
                '30th-id','30th-rtt',
                'ping'
                ])
        addlog = []
        for line in fr.read().splitlines():
            data = line.split()
            if not data:
                continue
            else:
                if data[0] == 'datetime':
                    addlog.append(data[1] + ' ' + data[2])
                elif data[0].isdigit() and data[1] != 'packets':
                    number = int(data[0])
                    if data[1] == '*':
                        addlog.append(data[1])
                        addlog.append(np.nan)
                    else:
                        addlog.append(data[1]+data[2])
                        addlog.append(data[3])
                elif data[0] == 'rtt':
                    while(number < 30):
                        addlog.append('')
                        addlog.append(np.nan)
                        number += 1
                    addlog.append(data[3].split('/')[0])
                    log = log.append(pd.DataFrame(addlog, index=log.columns).T)
                    addlog = []
 
    log = log.sort_values('datetime')
    log = log.reset_index(drop=True)
    log.to_csv('./' + sys.argv[1] + '/' + file[:-4] + '.csv',index = False)

    with open ('./' + sys.argv[1] + '/' + file[:-4] + '-ping.csv','w') as fw:
        fw.write('datetime,ping\n')
        for i in range(len(log)):
            fw.write(log['datetime'][i] + ',' + log['ping'][i] + '\n')

for file in files:
    shutil.move(file,'./' + sys.argv[1] + '/')