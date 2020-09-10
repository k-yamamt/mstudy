import os
import numpy as np
import math
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

def checkBreakPoint(C,filename):
    main_sig = lowpass(filename)
    x = range(len(main_sig))

    ax1 = plt.subplot()
    ax1.plot(main_sig, c='black')
    ax1.set(xlim=[1000,1500],ylim=[0,120])
    ax1.set_xlabel('index')
    ax1.set_ylabel('Delay[ms]',color='black')
    for i in C:
        ax1.plot(i,main_sig[i],marker='.',color='red',markersize=8)
    for i in range(len(C)-1):
        X = [[j] for j in x[C[i]:C[i+1]]]
        clf = linear_model.LinearRegression()
        clf.fit(X,main_sig[C[i]:C[i+1]])
        ax1.plot(X,clf.predict(X))
    savepdf('C:/master/mstudy/analysis/model/checkBreakPoint.pdf')

def savepdf(filename):
    pp = PdfPages(filename)
    pp.savefig()
    pp.close()
    plt.close()

def lowpass(filename):
    y = data(filename)

    sample_freq =fftfreq(len(y),d=15)
    sig_fft=fft(y)

    sig_fft[np.abs(sample_freq) > 0.008] = 0
    main_sig =np.real(ifft(sig_fft))

    return main_sig

def getSlope(C,filename):
    main_sig = lowpass(filename)
    x = range(len(main_sig))

    A = []

    for i in range(len(C)-1):
        X = [[j] for j in x[C[i]:C[i+1]]]
        clf = linear_model.LinearRegression()
        clf.fit(X,main_sig[C[i]:C[i+1]])
        A.append(clf.coef_[0])

    return A

def getBreakPoint(filename):
    main_sig = lowpass(filename)
    x = range(len(main_sig))

    C = [0]

    while(True):
        if(C[-1]+200 > len(x)):
            break
        else:
            Z = []
            X = [[i] for i in x[C[-1]:(C[-1]+20)]]
            Xtemp = [[i] for i in x[(C[-1]+20):(C[-1]+201)]]
            for i in range(C[-1]+21,(C[-1]+181)):
                X.append(Xtemp.pop(0))
                clf = linear_model.LinearRegression()
                clf.fit(X,main_sig[C[-1]:i])
                clftemp = linear_model.LinearRegression()
                clftemp.fit(Xtemp,main_sig[i:(C[-1]+201)])
                if(clf.coef_[0] > 0 and clftemp.coef_[0] > 0):
                    Z.append(clf.score(X, main_sig[C[-1]:i]) * clftemp.score(Xtemp, main_sig[i:(C[-1]+201)]))
                elif(clf.coef_[0] > 0):
                    Z.append(-100 + clf.score(X, main_sig[C[-1]:i]))
                else:
                    Z.append(-200)

            C.append(C[-1] + Z.index(max(Z)) + 21)
    
    return C

def makeData():
    for filename in os.listdir("C:/master/mstudy/data/long/csv/"):
        C = getBreakPoint(filename)
        A = getSlope(C,filename)
        B = np.diff(C).tolist()

        with open("C:/master/mstudy/data/long/model/A_" + filename, 'w') as fa:
            for i in A:
                fa.write(str(i) + '\n')
        with open("C:/master/mstudy/data/long/model/B_" + filename, 'w') as fb:
            for i in B:
                fb.write(str(i) + '\n')
        with open("C:/master/mstudy/data/long/model/C_" + filename, 'w') as fc:
            for i in C:
                fc.write(str(i) + '\n')

def Aplot(A,filename):
    ax1 = plt.subplot()
    ax1.hist(A, range=(0, 1), bins=20)
    savepdf('C:/master/mstudy/analysis/model/A_hist_' + filename + '.pdf')

def Bplot(B,filename):
    ax1 = plt.subplot()
    ax1.hist(B, range=(0, 200), bins=20)
    savepdf('C:/master/mstudy/analysis/model/B_hist_' + filename + '.pdf')

def Aallplot(A_all):
    ax1 = plt.subplot()
    ax1.hist(A_all, range=(0, 1), bins=50)
    ax1.set_xlabel('inclination')
    ax1.set_ylabel('Number of inclination')
    savepdf('C:/master/mstudy/analysis/model/A_hist_all.pdf')

def Ballplot(B_all):
    ax2 = plt.subplot()
    ax2.hist(B_all, range=(0, 180), bins=90)
    ax2.set_xlabel('length')
    ax2.set_ylabel('Number of length')
    savepdf('C:/master/mstudy/analysis/model/B_hist_all.pdf')

def corr(A_all,B_all):
    corr = np.corrcoef(A_all, B_all)[0,1]
    ax1 = plt.subplot()
    ax1.scatter(A_all, B_all)
    ax1.set_title('Corr = ' + str(corr))
    ax1.set_xlabel('inclination')
    ax1.set_ylabel('length')
    savepdf('C:/master/mstudy/analysis/model/corr.pdf')

def autoCorr(y,savename):
    sm.graphics.tsa.plot_acf(y, lags=20)
    plt.xlabel('lags')
    plt.ylabel('corr')
    savepdf(savename)

def main():
    A_all = []
    B_all = []

    for filename in os.listdir("C:/master/mstudy/data/long/model/"):
        A = []
        B = []
        C = []

        if(filename[0]=='A'):
            with open("C:/master/mstudy/data/long/model/" + filename, 'r') as fa:
                for line in fa:
                    A.append(float(line))
            A_all = A_all + A

            #Aplot(A,filename)
        elif(filename[0]=='B'):
            with open("C:/master/mstudy/data/long/model/" + filename, 'r') as fb:
                for line in fb:
                    B.append(float(line))
            B_all = B_all + B

            #Bplot(B,filename)
        elif(filename[0]=='C'):
            with open("C:/master/mstudy/data/long/model/" + filename, 'r') as fc:
                for line in fc:
                    C.append(float(line))

    Aallplot(A_all)
    Ballplot(B_all)
    corr(A_all,B_all)
    autoCorr(A_all,'C:/master/mstudy/analysis/model/autoCorr-A.pdf')
    autoCorr(B_all,'C:/master/mstudy/analysis/model/autoCorr-B.pdf')

if __name__ == '__main__':
    main()

