import numpy as np
import pandas as pd
import os
import statsmodels.api as sm

def getADFList(distination, process):
    files = []
    if (distination == 'AWS'):
        for filename in os.listdir('C:/master/mstudy/data/AWS/'):
            if filename[-9:] == '-ping.csv':
                files.append('C:/master/mstudy/data/AWS/' + filename)
    elif (distination == 'SINET'):
        for filename in os.listdir('C:/master/mstudy/data/SINET/csv/15s/'):
            files.append('C:/master/mstudy/data/SINET/csv/15s/' + filename)
    else:
        print('error distination')
        exit()
    
    ADFList = []

    for filename in files:
        y = pd.read_csv(filename, index_col=0)['ping']
        if (process == 'diff'):
            y = np.diff(y)
        elif (process != 'norm'):
            print('error process')
            exit()

        adf_result = sm.tsa.stattools.adfuller(y)

        if(adf_result[1]<0.01):
            ADFList.append(str(0.01))
        else:
            ADFList.append(str(adf_result[1]))
    
    return ADFList

AWS_norm = '\n'.join(getADFList('AWS','norm'))
with open("adf_AWS_norm.txt", 'a') as f:
    f.write(AWS_norm)

AWS_diff = '\n'.join(getADFList('AWS','diff'))
with open("adf_AWS_diff.txt", 'a') as f:
    f.write(AWS_diff)

SINET_norm = '\n'.join(getADFList('SINET','norm'))
with open("adf_SINET_norm.txt", 'a') as f:
    f.write(SINET_norm)

SINET_diff = '\n'.join(getADFList('SINET','diff'))
with open("adf_SINET_diff.txt", 'a') as f:
    f.write(SINET_diff)