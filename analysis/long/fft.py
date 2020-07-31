import os
import numpy as np
import math
from scipy.fftpack import fft
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.pyplot as plt

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

    ax = plt.subplot()
    ax.plot(freq,np.abs(yf),c='blue')
    ax.set_xlabel("Frequency[Hz]")
    ax.set_ylabel("Amplitude")
    plt.xlim([0.0, 0.0334])
    plt.ylim([0, 3])

    pp = PdfPages('C:/master/mstudy/analysis/long/fft_low_' + date + '.pdf')
    pp.savefig()
    pp.close()
    plt.close()

def main():
    low()

if __name__ == '__main__':
    main()