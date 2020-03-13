library(stringr)
library(fGarch)
library(tseries)
library(timeSeries)
library(mclust)
library(cluster)

path <- 'C:/master/mstudy/data/AWS/'
files <- list.files(path, pattern = "-ping.csv", full.names = TRUE)
p <- 2
d <- 1
q <- 2

aicCheck <- function(){

  arimaAIC <- function(data,p,d,q){
  arima(data,order = c(p,d,q))$aic
  }
  
  AIC <- matrix(nrow = 0,ncol = 2)
  colnames(AIC) <- c('(p,q,r)','AIC')
  date <- NULL
  
  for (filename in files){
    df <- read.csv(file = filename, header = TRUE, sep=',')
    date <- c(date,str_sub(filename, start = -20, end = -14))
    best.p <- 0
    best.d <- 0
    best.q <- 0
    min.aic <- 10000
    
    for (pp in 0:p){
      for (dd in 0:d){
        for (qq in 0:q){
          aic <- try(arimaAIC(df$ping,pp,dd,qq))
          if ( aic < min.aic){
            best.p <- pp
            best.d <- dd
            best.q <- qq
            min.aic <- aic
          }
        }
      }
    }

  AIC <- rbind(AIC,c(paste('(',best.p,',',best.d,',',best.q,')',sep = ''),min.aic))
  }
  
  rownames(AIC) <- date
  AIC
}

paramCheck <- function(){
  D <- matrix(nrow = 0,ncol = 5)
  colnames(D) <- c('a_1','a_2','b_1','b_2','sigma^2')
  date <- NULL
  
  for (filename in files){
    df <- read.csv(file = filename, header = TRUE, sep=',')
    
    date <- c(date,str_sub(filename, start = -20, end = -14))
    arima_result <- arima(df$ping,order = c(2,1,2))
    a_1 <- arima_result[['coef']]['ar1']
    a_2 <- arima_result[['coef']]['ar2']
    b_1 <- arima_result[['coef']]['ma1']
    b_2 <- arima_result[['coef']]['ma2']
    sigma2 <- arima_result[['sigma2']]
  
    D <- rbind(D,c(a_1,a_2,b_1,b_2,sigma2))
  }
  
 rownames(D) <- date
 D
}
 
clust <- function(D){
  k = 7
  cutree(hclust(dist(D,method = 'euclidean'),method = 'ward.D2'),k)
}

#AIC <- aicCheck()
  
D <- paramCheck()

cluster <- clust(D)

plot(cluster)
