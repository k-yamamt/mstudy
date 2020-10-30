import os
import numpy as np
from scipy.fftpack import fft
from scipy.fftpack import fftfreq
from scipy.fftpack import ifft
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.pyplot as plt
from sklearn import linear_model
import statsmodels.api as sm

def data(filename):
    y = []
    with open('C:/master/mstudy/data/long/csv/' + filename, 'r') as fr:
        for line in fr:
            value = line.rstrip('\n').split(',')[1]
            if(any(chr.isdigit() for chr in value)):
                y.append(float(value))
    
    return y

def checkBreakPoint(filename):
    x = lowpass(filename)
    a,b = getPoint(filename)

    xrange = [[0,2000],[2000,4000],[4000,6000]]

    for k in range(len(xrange)):
        ax1 = plt.subplot()
        ax1.plot(x, c='black')
        ax1.set(xlim=xrange[k],ylim=[50,120])
        ax1.set_xlabel('index')
        ax1.set_ylabel('Delay[ms]',color='black')
        for i in a:
            ax1.plot(i,x[i],marker='.',color='red',markersize=8)
        for i in b:
            ax1.plot(i,x[i],marker='.',color='blue',markersize=8)
        for i in range(len(b)):
            X = [ [j] for j in range(a[i],b[i]+1) ]
            clf = linear_model.LinearRegression()
            clf.fit(X,x[a[i]:(b[i]+1)])
            ax1.plot(X,clf.predict(X))

        savefile = ('C:/master/mstudy/analysis/model/method4/' + filename[:-4] + '_' + str(k) + '.pdf')
        pp = PdfPages(savefile)
        pp.savefig()
        pp.close()
        plt.close()

def lowpass(filename):  #filter=0.008Hz
    y = data(filename)

    sample_freq =fftfreq(len(y),d=15)
    sig_fft=fft(y)

    sig_fft[np.abs(sample_freq) > 0.008] = 0
    x =np.real(ifft(sig_fft))

    return x

def getLine(a,b,filename):
    x = lowpass(filename)
    alpha = []
    xhat = []

    for i in range(len(b)):
        X = [ [j] for j in range(a[i],b[i]+1) ]
        clf = linear_model.LinearRegression()
        clf.fit(X,x[a[i]:(b[i]+1)])
        alpha.append(clf.coef_[0])
        xhat.append(clf.predict(X)[0])

    return [alpha,xhat]

def getLength(a,b):
    L1 = []
    L2 = []
    for i in range(len(b)):
        L1.append(b[i] - a[i] + 1)
        L2.append(a[i+1] - b[i] - 1)

    return [L1,L2]

def getNoise(a,b,filename):
    x = lowpass(filename)
    xl = np.nan

    noise = [np.nan] * a[0]

    for i in range(len(b)):
        X = [ [j] for j in range(a[i],b[i]+1) ]
        clf = linear_model.LinearRegression()
        clf.fit(X,x[a[i]:(b[i]+1)])
        add1 = np.array(x[a[i]:(b[i]+1)]) - np.array(clf.predict(X))   

        xr = clf.predict(X)[0]

        if (not np.isnan(xl)):
            for j in range(b[i-1]+1,a[i]):
                noise.append( x[j] - ((j-b[i-1])*xr + (a[i]-j)*xl)/(a[i]-b[i-1]))

        xl = clf.predict(X)[-1]

        noise = noise + add1.tolist()

    noise = noise + [np.nan] *(len(x) - len(noise))

    return noise
 
def getPoint(filename):    #w,l,alpha
    l = 20
    w = 25
    beta = 1.0
    x = lowpass(filename)
    N = len(x)
    a = []
    b = [-1]  
    point = 'left'

    while(True):
        if(point == 'left'):
            left = b[-1] + 1
            left_prev = -100.0
            while(left + l <= N):
                X = [[i] for i in range(left,left+l)]
                clf = linear_model.LinearRegression()
                clf.fit(X,x[left:(left+l)])
                if(left_prev > 0 and clf.coef_ / left_prev <= beta):
                    a.append(left-1)
                    point = 'right'
                    break
                left_prev = clf.coef_
                left = left + 1

            if (left + l > N):
                point = 'end'

        elif(point == 'right'):
            left = a[-1]
            right = left + l - 1
            X = [[i] for i in range(left,right)]    
            count = []
            right_prev = np.nan
            while(right < N):
                X.append([right])      
                clf = linear_model.LinearRegression()
                clf.fit(X,x[left:(right+1)])
                if (not np.isnan(right_prev)):
                    if(clf.coef_ < right_prev):
                        count.append(1)
                    else:
                        count.append(0)

                if(len(count) > w*1.1):
                    count.pop(0)
                
                if(sum(count) == w):
                    right_check = right - len(count) + count.index(1)
                    X1 = [[j] for j in range(left,right_check+1)]
                    clf1 = linear_model.LinearRegression()
                    clf1.fit(X1,x[left:(right_check+1)])
                    if(clf1.coef_>0):
                        b.append(right_check)
                        point = 'left'
                        break
                right_prev = clf.coef_
                right = right + 1

            if (right >= N):
                point = 'end'

        elif point == 'end':
            break
    
    b.pop(0)
    if(len(a) == len(b)):
        b.pop(-1)

    return [a,b]

def makeData():
    for filename in os.listdir("C:/master/mstudy/data/long/csv/"):
        a,b = getPoint(filename)
        L1,L2 = getLength(a,b)
        alpha,xhat = getLine(a,b,filename)
        noise = getNoise(a,b,filename)
        
        with open("C:/master/mstudy/analysis/model/method4/dataset/alpha_" + filename, 'w') as fa:
            for i in alpha:
                fa.write(str(i) + '\n')
        with open("C:/master/mstudy/analysis/model/method4/dataset/xhat_" + filename, 'w') as fb:
            for i in xhat:
                fb.write(str(i) + '\n')
        with open("C:/master/mstudy/analysis/model/method4/dataset/L1_" + filename, 'w') as fc:
            for i in L1:
                fc.write(str(i) + '\n')
        with open("C:/master/mstudy/analysis/model/method4/dataset/L2_" + filename, 'w') as fd:
            for i in L2:
                fd.write(str(i) + '\n')
        with open("C:/master/mstudy/analysis/model/method4/dataset/noise_" + filename, 'w') as fe:
            for i in noise:
                fe.write(str(i) + '\n')

def main():
    makeData()
    
    #checkBreakPoint("6-23.txt")

if __name__ == '__main__':
    main()