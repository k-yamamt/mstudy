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
    
    #data <- c(df$ping[1])
    #for (i in 2:length(df$ping)){
    #  data <- c(data,0.1*df$ping[i]+0.9*data[i-1])
    #}
    
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

regplot <- function(){
  normalPlot <- function(y){
    arima_result <- arima(y,order=c(2,0,2))
    #predict.y <- y - arima_result$residuals
    #predict.y.upper <- predict.y + 1.96*sqrt(arima_result$sigma2)
    #predict.y.lower <- predict.y - 1.96*sqrt(arima_result$sigma2)
    #pdf(paste(str_sub(filename,-20,-14),'-arma-normal.pdf',sep=''))
    #plot(predict.y.upper, type='l', col='darkorange', ann = FALSE, axes = FALSE,xlim = c(0,length(predict.y.upper)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(predict.y.lower, type='l', col='darkorange', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y.lower)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(predict.y, type='l', col='red', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(y, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(c(0,length(y)+1),c(30,30), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(60,60), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(90,90), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(120,120), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(150,150), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(180,180), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(50,50),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(100,100),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(150,150),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(200,200),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(mar=c(3, 3, 1, 1))
    #dev.off()
    pdf(paste(str_sub(filename,-20,-14),'-arma-normal-reshist.pdf',sep=''))
    hist(arima_result$residuals, breaks=200,xlim = c(-50,150),ylim=c(0,25),col = 'blue',xlab = 'Delay[ms]',ylab = 'Num',main = '')
    dev.off()
  }
  
  diffPlot <- function(y){
    arima_result <- arima(y,order=c(2,1,2))
    #predict.y <- y - arima_result$residuals
    #predict.y.upper <- predict.y + 1.96*sqrt(arima_result$sigma2)
    #predict.y.lower <- predict.y - 1.96*sqrt(arima_result$sigma2)
    #pdf(paste(str_sub(filename,-20,-14),'-arma-diff.pdf',sep=''))
    #plot(predict.y.upper, type='l', col='darkorange', ann = FALSE, axes = FALSE,xlim = c(0,length(predict.y.upper)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(predict.y.lower, type='l', col='darkorange', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y.lower)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(predict.y, type='l', col='red', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(y, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(c(0,length(y)+1),c(30,30), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(60,60), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(90,90), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(120,120), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(150,150), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(180,180), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(50,50),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(100,100),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(150,150),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(200,200),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(mar=c(3, 3, 1, 1))
    #dev.off()
    pdf(paste(str_sub(filename,-20,-14),'-arma-diff-reshist.pdf',sep=''))
    hist(arima_result$residuals, breaks=200,xlim = c(-50,150),ylim=c(0,25),col = 'blue',xlab = 'Delay[ms]',ylab = 'Num',main = '')
    dev.off()
  }
  
  maPlot <- function(y){
    arima_result <- arima(y,order=c(1,0,1))
    #predict.y <- y - arima_result$residuals
    #predict.y.upper <- predict.y + 1.96*sqrt(arima_result$sigma2)
    #predict.y.lower <- predict.y - 1.96*sqrt(arima_result$sigma2)
    #pdf(paste(str_sub(filename,-20,-14),'-arma-ma.pdf',sep=''))
    #plot(predict.y.upper, type='l', col='darkorange', ann = FALSE, axes = FALSE,xlim = c(0,length(predict.y.upper)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(predict.y.lower, type='l', col='darkorange', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y.lower)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(predict.y, type='l', col='red', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(y, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new = TRUE)
    #plot(c(0,length(y)+1),c(30,30), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(60,60), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(90,90), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(120,120), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(150,150), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(0,length(y)+1),c(180,180), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(50,50),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(100,100),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(150,150),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(new=T)
    #plot(c(200,200),c(0,200), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,200), xaxs = "i", yaxs = "i")
    #par(mar=c(3, 3, 1, 1))
    #dev.off()
    
    pdf(paste(str_sub(filename,-20,-14),'-arma-ma-reshist.pdf',sep=''))
    hist(arima_result$residuals, breaks=25,xlim = c(-5,20),ylim=c(0,90),col = 'blue',xlab = 'Delay[ms]',ylab = 'Num',main = '')
    dev.off()
  }
  
  
  for(filename in files[1:5]){
    df <- read.csv(file = filename, header = TRUE, sep=',')
    
    normalPlot(df$ping)
    
    diffPlot(df$ping)
    
    y <- c(df$ping[1])
    for (i in 2:length(df$ping)){
      y <- c(y,0.1*df$ping[i] + 0.9*y[i-1])
    }
    
    maPlot(y)
  }
}

regplot()

#AIC <- aicCheck()
  
#D <- paramCheck()

#cluster <- clust(D)

#plot(cluster)