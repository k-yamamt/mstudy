import os
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

def readData():
    alpha = []
    xhat = []
    xhat_next = []
    L1 = []
    L2 = []
    noise = []

    dir = "C:/master/mstudy/analysis/model/method4/dataset/"

    for filename in os.listdir(dir):
        id = filename.split('_')[0]
        if(id == 'alpha'):
            with open(dir + filename, 'r') as fa:
                for line in fa:
                    alpha.append(float(line))

        elif(id == 'xhat'):
            with open(dir + filename, 'r') as fb:
                add = []
                for line in fb:
                    xhat.append(float(line))
                    add.append(float(line))
            xhat_next = xhat_next + add[1:] + [np.nan]

        elif(id == 'L1'):
            with open(dir + filename, 'r') as fc:
                for line in fc:
                    L1.append(float(line))

        elif(id == 'L2'):
            with open(dir + filename, 'r') as fd:
                for line in fd:
                    L2.append(float(line))
        
        elif(id == 'noise'):
            with open(dir + filename, 'r') as fe:
                for line in fe:
                    noise.append(float(line))

    return [alpha, xhat, xhat_next, L1, L2, noise]

def savepdf(filename):
    dir = "C:/master/mstudy/analysis/model/method4/plot/"
    pp = PdfPages(dir + filename)
    pp.savefig()
    pp.close()
    plt.close()

def histAlpha(alpha):
    ax1 = plt.subplot()
    ax1.hist(alpha, range=[0,1.6], bins=160, normed=True)
    ax1.set_xlabel("$\\alpha_i$")
    ax1.set_ylabel("Normalized Number of $\\alpha_i$")
    savepdf('hist_alpha.pdf')

def histXhat(xhat):
    ax1 = plt.subplot()
    ax1.hist(xhat, range=[50,90], bins=80, normed=True)
    ax1.set_xlabel("$\\hat{x}_i$")
    ax1.set_ylabel("Normalized Number of $\\hat{x}_i$")
    savepdf('hist_xhat.pdf')

def histL1(L1):
    ax1 = plt.subplot()
    ax1.hist(L1, range=[0,2500], bins=250, normed=True)
    ax1.set_xlabel("$b_i - a_i$")
    ax1.set_ylabel("Normalized Number of $b_i - a_i$")
    savepdf('hist_L1.pdf')

def histL2(L2):
    ax1 = plt.subplot()
    ax1.hist(L2, range=[0,50], bins=50, normed=True)
    ax1.set_xlabel("$a_{i+1} - b_i$")
    ax1.set_ylabel("Normalized Number of $a_{i+1} - b_i$")
    savepdf('hist_L2.pdf')

def histNoise(noise):
    ax1 = plt.subplot()
    ax1.hist(noise, range=[-50,50], bins=100, normed=True)
    ax1.set_xlabel("$\\epsilon_i$")
    ax1.set_ylabel("Normalized Number of $\\epsilon_i$")
    savepdf('hist_noise.pdf')

def getDF(alpha,xhat,xhat_next,L1,L2):
    df = pd.DataFrame({
        "$\\alpha_i$" : alpha,
        "$\\hat{x}_i$" : xhat,
        "$\\hat{x}_{i+1}$" : xhat_next,
        "$b_i - a_i$" : L1,
        "$a_{i+1} - b_i$" : L2
    })
    
    df = df.dropna()
    
    return df

def scatter(df):
    sns.pairplot(df)
    savepdf("scatter.pdf")

def corr(df):
    corr = df.corr()
    sns.heatmap(corr,
            vmin=-1.0,
            vmax=1.0,
            center=0,
            annot=True,
            fmt='.1f',
            xticklabels=corr.columns.values,
            yticklabels=corr.columns.values
           )

    savepdf("corr.pdf")

def main():
    alpha, xhat, xhat_next, L1, L2, noise = readData()

    
"""
    histAlpha(alpha)
    histXhat(xhat)
    histL1(L1)
    histL2(L2)
    histNoise(noise)
    scatter(getDF(alpha,xhat,xhat_next,L1,L2))
    corr(getDF(alpha,xhat,xhat_next,L1,L2))
"""
if __name__ == '__main__':
    main()