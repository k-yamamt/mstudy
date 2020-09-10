import os
import numpy as np
import math
from scipy.fftpack import fft
from scipy.fftpack import fftfreq
from scipy.fftpack import ifft
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.pyplot as plt
from sklearn import linear_model
import statistics

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

def data():
    date = '6-23'

    y = []
    with open('C:/master/mstudy/data/long/csv/' + date + '.txt', 'r') as fr:
        for line in fr:
            value = line.rstrip('\n').split(',')[1]
            if(any(chr.isdigit() for chr in value)):
                y.append(float(value))
    
    return y

def savepdf(filename):
    pp = PdfPages(filename)
    pp.savefig()
    pp.close()
    plt.close()

def lowpass():
    y = data()
    #x = range(len(y))

    sample_freq =fftfreq(len(y),d=15)
    sig_fft=fft(y)

    #pidxs = np.where(sample_freq > 0)
    #freqs, power = sample_freq[pidxs], np.abs(sig_fft)[pidxs]
    #ax.plot(freqs, power)

    #ax = plt.subplot()
    sig_fft[np.abs(sample_freq) > 0.008] = 0
    main_sig =np.real(ifft(sig_fft))
    #ax.plot(y)
    #ax.plot(main_sig, c='red')
    #ax.set(xlim=[0,1500],ylim=[50,150])
    #plt.xlabel('index')
    #plt.ylabel('Delay[ms]')

    #savepdf('C:/master/mstudy/analysis/0829/lowpass'+str(i)[-2:]+'.pdf')

    return main_sig

def regression():
    main_sig = lowpass()
    x = range(len(main_sig))

    C = [0]
    
    ax = plt.subplot()
    ax2 = ax.twinx() 

    ax2.plot(-10,-10)

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
                if(clf.coef_ > 0 and clftemp.coef_ > 0):
                    Z.append(clf.score(X, main_sig[C[-1]:i]) * clftemp.score(Xtemp, main_sig[i:(C[-1]+201)]))
                else:
                    Z.append(-100)

            ax2.plot(range(C[-1]+21,(C[-1]+181)),Z)

            C.append(C[-1] + Z.index(max(Z)) + 21)
            
    ax.plot(main_sig, c='black')
    for i in range(len(C)-1):
        X = [[j] for j in x[C[i]:C[i+1]]]
        clf = linear_model.LinearRegression()
        clf.fit(X,main_sig[C[i]:C[i+1]])
        ax.plot(X, clf.predict(X))

    ax.set(xlim=[0,1500],ylim=[0,120])
    ax.set_xlabel('index')
    ax.set_ylabel('Delay[ms]',color='black')

    ax2.set(xlim=[0,1500],ylim=[0,0.5])
    ax2.set_ylabel('Evaluation Value')

    savepdf('C:/master/mstudy/analysis/0829/resultalg1.pdf')

def integral():
    main_sig = lowpass()

    I = []
    x = 0.0  
    for i in main_sig[0:1500]:
        x = x + i
        I.append(x)
    I = I/I[-1]

    for width in [5,10,15,20]:
        for alpha in [0.95,0.97,0.99]:
            ax1 = plt.subplot()
            ax2 = ax1.twinx() 

            ax1.plot(main_sig, c='black')
            ax1.set(xlim=[0,1500],ylim=[0,120])
            ax1.set_xlabel('index')
            ax1.set_ylabel('Delay[ms]')
            
            ax2.plot(I)
            ax2.set(xlim=[0,1500],ylim=[0,2])
            ax2.set_ylabel('Normalized Evaluation Value')

            I_prime = np.gradient(I)
            I_2prime = np.gradient(I_prime)

            for i in range(width,len(I_2prime)-2*width):
                if I_2prime[i] <= 0 and 0 <= I_2prime[i+1] :
                    if sum(main_sig[i:i+width])/sum(main_sig[i-width:i]) <= alpha:
                        ax1.plot(i,main_sig[i],marker='.',color='red',markersize=8)
                        ax2.plot(i,I[i],marker='.',color='red')

            savepdf('C:/master/mstudy/analysis/0829/result_integral_width'+str(width)+'_alpha0'+str(alpha)[-2:]+'.pdf')

def statis():
    main_sig = lowpass()

    for width in [5,10,15,20]:
        for alpha in [2,4,6]:
            Ave = [np.nan]*(width-1)
            for i in range(width-1,1500):
                Ave.append(np.average(main_sig[i-width+1:i+1]))
            ax1 = plt.subplot()
            ax2 = ax1.twinx() 

            ax1.plot(main_sig, c='black')
            ax1.set(xlim=[0,1500],ylim=[0,120])
            ax1.set_xlabel('index')
            ax1.set_ylabel('Delay[ms]')

            ax2.plot(Ave,c='blue')
            ax2.set(xlim=[0,1500],ylim=[60,180])
            ax2.set_ylabel('Average,[ms]',color='blue')

            for x in range(width,1500):
                if(alpha <= Ave[x-1] - Ave[x]):
                    ax1.plot(x,main_sig[x],marker='.',color='red',markersize=8)
                    ax2.plot(x,Ave[x],marker='.',color='red')

            savepdf('C:/master/mstudy/analysis/0829/result_average_width' + str(width) +'_alpha'+str(alpha) + '.pdf')

            Mid = [np.nan]*(width/2)
            for i in range(width/2,1500-width/2):
                Mid.append(statistics.median(main_sig[i-width/2:i-width/2+width]))

            ax1 = plt.subplot()
            ax2 = ax1.twinx() 

            ax1.plot(main_sig, c='black')
            ax1.set(xlim=[0,1500],ylim=[0,120])
            ax1.set_xlabel('index')
            ax1.set_ylabel('Delay[ms]')

            ax2.plot(Mid,c='green')
            ax2.set(xlim=[0,1500],ylim=[60,180])
            ax2.set_ylabel('Median[ms]',color='green')

            for x in range(width/2+1,1500-width/2):
                if(alpha <= Mid[x-1] - Mid[x]):
                    ax1.plot(x,main_sig[x],marker='.',color='red',markersize=8)
                    ax2.plot(x,Mid[x],marker='.',color='red')

            savepdf('C:/master/mstudy/analysis/0829/result_median_width' + str(width) +'_alpha'+str(alpha) + '.pdf')

def decline():
    main_sig = lowpass()

    for l in [15,20,25]:
        C = [0]

        ax1 = plt.subplot()
        ax2 = ax1.twinx()

        ax2.plot(-10,-10)

        ax1.plot(main_sig, c='black')
        ax1.set(xlim=[0,1500],ylim=[0,120])
        ax1.set_xlabel('index')
        ax1.set_ylabel('Delay[ms]',color='black')

        ax2.set(xlim=[0,1500], ylim=[-0.6,1.2])
        ax2.set_ylabel('Evaluation Value')

        frag = True
        while(frag):
            prev = np.nan
            count = 0
            left = C[-1]
            X = [[i] for i in range(left,left+19)]
            a = []
            for i in range(19,1500-left):
                right = left + i
                X.append([right])
                if(i >= 1500 - left -1):
                    frag = False
                    break
                clf = linear_model.LinearRegression()
                clf.fit(X,main_sig[left:right+1])
                a.append(clf.coef_)
                if(not np.isnan(prev)):
                    if(clf.coef_ < prev):
                        count = count + 1
                    else:
                        count = 0
                prev = clf.coef_

                if(count >= l):
                    C.append(right-l+1)
                    ax2.plot(range(left+19,right+1),a)
                    break

        #for i in C:
        #    ax1.plot(i,main_sig[i],marker='.',color='red',markersize=8)
    
        for i in range(len(C)-1):
            clf = linear_model.LinearRegression()
            X = [[j] for j in range(C[i],C[i+1])]
            clf.fit(X,main_sig[C[i]:C[i+1]])
            ax1.plot(X, clf.predict(X))

        savepdf('C:/master/mstudy/analysis/0829/result_decline_width'+str(l)+'.pdf')

def main():
    #lowpass()
    #regression()
    #integral()
    #statis()
    decline()

if __name__ == '__main__':
    main()