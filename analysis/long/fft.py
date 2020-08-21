import os
import numpy as np
import math
from scipy.fftpack import fft
from scipy.fftpack import fftfreq
from scipy.fftpack import ifft
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.pyplot as plt
from sklearn import linear_model

def over():
    date = '6-23'

    y = []
    with open('C:/master/mstudy/data/long/csv/' + date + '.txt', 'r') as fr:
        for line in fr:
            value = line.rstrip('\n').split(',')[1]
            if(any(chr.isdigit() for chr in value)):
                y.append(float(value))

    N = len(y)
    dt = 15
    freq = np.linspace(0,1.0/dt,N)
    yf = fft(y)/(N/2)

    ax = plt.subplot()
    ax.plot(freq,np.abs(yf),c='blue')
    ax.set_xlabel("Frequency[Hz]")
    ax.set_ylabel("Amplitude")
    plt.xlim([0.0, 0.0334])
    plt.ylim([0, 3])

    pp = PdfPages('C:/master/mstudy/analysis/long/fft_' + date + '.pdf')
    pp.savefig()
    pp.close()
    plt.close()

def low():
    date = '6-25'

    y = []
    with open('C:/master/mstudy/data/long/fft/low_' + date + '.txt', 'r') as fr:
        for line in fr:
            y.append(float(line))

    N = len(y)
    dt = 15
    freq = np.linspace(0,1.0/dt,N)
    yf = fft(y)/(N/2)
    for i in range(len(yf)):
        if(np.abs(yf[i])>0.9 and np.abs(yf[i]) < 1.0):
            if(freq[i]>=0 and freq[i]<= 0.0334):
                print (freq[i])

    #ax = plt.subplot()
    #ax.plot(freq,np.abs(yf),c='blue')
    #ax.set_xlabel("Frequency[Hz]")
    #ax.set_ylabel("Amplitude")
    #plt.xlim([0.0, 0.0334])
    #plt.ylim([0, 3])

    #pp = PdfPages('C:/master/mstudy/analysis/long/fft_low_' + date + '.pdf')
    #pp.savefig()
    #pp.close()
    #plt.close()

def main():
    lowpass()

def lowpass():
    date = '6-23'

    y = []
    with open('C:/master/mstudy/data/long/csv/' + date + '.txt', 'r') as fr:
        for line in fr:
            value = line.rstrip('\n').split(',')[1]
            if(any(chr.isdigit() for chr in value)):
                y.append(float(value))

    ax = plt.subplot()

    x = range(len(y))
    sample_freq =fftfreq(len(y),d=15)
    sig_fft=fft(y)

    #pidxs = np.where(sample_freq > 0)
    #freqs, power = sample_freq[pidxs], np.abs(sig_fft)[pidxs]
    #ax.plot(freqs, power)

    sig_fft[np.abs(sample_freq) > 0.008] = 0
    main_sig =np.real(ifft(sig_fft))
    #ax.plot(y)
    ax.plot(main_sig, c='black')
    #ax.set(xlim=[0,2000],ylim=[50,180])
    #plt.xlabel('index')
    #plt.ylabel('Delay[ms]')


    C = [0]
    
    while(True):
        if(C[-1]+200 > len(x)):
            break
        else:
            Z = []
            X = [[x[C[-1]]]]
            Xtemp = [[i] for i in x[(C[-1]+1):(C[-1]+200)]]
            for i in range(C[-1]+2,(C[-1]+199)):
                X.append(Xtemp.pop(0))
                clf = linear_model.LinearRegression()
                clf.fit(X,main_sig[C[-1]:i])
                clftemp = linear_model.LinearRegression()
                clftemp.fit(Xtemp,main_sig[i:(C[-1]+200)])
                if(clf.coef_ > 0 and clftemp.coef_ > 0):
                    Z.append(clf.score(X, main_sig[C[-1]:i]) * clftemp.score(Xtemp, main_sig[i:(C[-1]+200)]))
                else:
                    Z.append(-100)

            C.append(C[-1] + Z.index(max(Z)) + 2)
    
    for i in range(len(C)-1):
        X = [[j] for j in x[C[i]:C[i+1]]]
        clf = linear_model.LinearRegression()
        clf.fit(X,main_sig[C[i]:C[i+1]])
        ax.plot(X, clf.predict(X))

    ax.set(xlim=[0,2000],ylim=[50,180])
    plt.xlabel('index')
    plt.ylabel('Delay[ms]')


    pp = PdfPages('C:/master/mstudy/analysis/long/example3.pdf')
    pp.savefig()
    pp.close()
    plt.close()

if __name__ == '__main__':
    main()