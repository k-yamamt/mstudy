library('stringr')

#addr <- 'C:/master/mstudy/data/aws.amazon.com/'
#addr <- 'C:/master/mstudy/data/news.yahoo.co.jp/'
addr <- 'C:/master/mstudy/data/AWS/'

for (filename in list.files(addr, pattern = '-ping.csv', full.names = TRUE)){
  df <- read.csv(file = filename, header = TRUE, sep=',')
  
  pdf(paste(str_sub(filename,-24,-9),'plot.pdf',sep=''))
  y <- c()
  for (temp in df$ping){
    if(temp < 100){
      y <- c(y,temp)
    }
    else{
      y <- c(y,temp/10+90)
    }
  }
  plot(y, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  yvalues <- c(0,20,40,60,80,100,110,120,130)
  ystrings <- c(0,20,40,60,80,100,200,300,400)
  axis(side=2,at=yvalues,labels=ystrings)
  par(new=T)
  plot(c(0,length(y)+1),c(20,20), type='l', lty=3, xlab = '', ylab = '',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  par(new=T)
  plot(c(0,length(y)+1),c(40,40), type='l', lty=3, xlab = '', ylab = '',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  par(new=T)
  plot(c(0,length(y)+1),c(60,60), type='l', lty=3, xlab = '', ylab = '',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  par(new=T)
  plot(c(0,length(y)+1),c(80,80), type='l', lty=3, xlab = '', ylab = '',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  par(new=T)
  plot(c(0,length(y)+1),c(100,100), type='l', lty=3, xlab = '', ylab = '',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  par(new=T)
  plot(c(0,length(y)+1),c(110,110), type='l', lty=3, xlab = '', ylab = '',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  par(new=T)
  plot(c(0,length(y)+1),c(120,120), type='l', lty=3, xlab = '', ylab = '',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  par(new=T)
  plot(c(50,50),c(0,130), type='l', lty=3, xlab = '', ylab = '',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  par(new=T)
  plot(c(100,100),c(0,130), type='l', lty=3, xlab = '', ylab = '',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  par(new=T)
  plot(c(150,150),c(0,130), type='l', lty=3, xlab = '', ylab = '',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  par(new=T)
  plot(c(200,200),c(0,130), type='l', lty=3, xlab = '', ylab = '',xlim = c(0,length(y)+1),ylim = c(0,130),yaxt='n', xaxs = "i", yaxs = "i")
  par(mar=c(3, 3, 1, 1))
  mtext(paste('Mean:',signif(mean(df$ping),4),'[ms] , Var:',signif(var(df$ping),4),seq=''), side = 3, line = -3,cex = 2)
  dev.off()
  
}