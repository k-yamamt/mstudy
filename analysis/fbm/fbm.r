library('SuperGauss')

#addr = 'C:/master/mstudy/data/aws.amazon.com/'
addr = 'C:/master/mstudy/data/news.yahoo.co.jp/'
#addr = 'C:/master/mstudy/data/AWS/'

dT <- 15

fbm.simulation <- function(H,N){
  tseq <- (0:N)*dT
  
  msd <- fbm.msd(tseq = tseq[-1], H = H)
  acf <- msd2acf(msd = msd)
  
  system.time({
    dX <- rSnorm(n = 1, acf = acf, fft = TRUE)
  })
  
  Xt <-cumsum(c(0,dX))
  
  par(mar = c(4.1,4.1,.5,.5))
  plot(tseq[-1], Xt[-1], type = "l",xlab = "Time [s]", ylab = " Displacement[ms]", col = 'red')
}

fbm.acf <- function(H,N) {
  msd <- fbm.msd(1:N*dT, H = H)
  msd2acf(msd)
}

loglik.DL <- function(H,dX,N) {
  dSnormDL(X = dX, acf = fbm.acf(H,N), log = TRUE)
}


fbm.H.list <- c()
for (filename in list.files(addr, pattern = '-ping.csv', full.names = TRUE)){
  df <- read.csv(file = filename, header = TRUE, sep=',')
  if(length(df$ping)>200){
    
    dX <- diff(df$ping)
    min <- 10000
    min.H <- 0
    for (H in seq(from=0.01, to=0.99, by=0.01)){
      m <- -1*loglik.DL(H,dX,length(dX))
      if (m < min){
        min <- m
        min.H <- H
      }
    }
    fbm.H.list <- c(fbm.H.list,min.H)
  }

}

fbm.simulation(0.8,239)
plot((1:length(df$ping))*dT,df$ping,type='l')
