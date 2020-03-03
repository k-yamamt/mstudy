library('SuperGauss')

addr = 'C:/master/mstudy/data/aws.amazon.com/'

fbm.simulation <- function(H){
  tseq <- (0:N)*dT
  
  msd <- fbm.msd(tseq = tseq[-1], H = H)
  acf <- msd2acf(msd = msd)
  
  system.time({
    dX <- rSnorm(n = 1, acf = acf, fft = TRUE)
  })
  
  Xt <-cumsum(c(0,dX))
  
  par(mar = c(4.1,4.1,.5,.5))
  plot(tseq, Xt, type = "l",xlab = "Time [s]", ylab = " Displacement[ms]", col = 'red')
}

fbm.acf <- function(H) {
  msd <- fbm.msd(1:N*dT, H = H)
  msd2acf(msd)
}

loglik.DL <- function(H) {
  dSnormDL(X = dX, acf = fbm.acf(H), log = TRUE)
}

for (filename in list.files(addr, pattern = '-ping.csv', full.names = TRUE)){
  df <- read.csv(file = filename, header = TRUE, sep=',')
  
  dX <- diff(df$ping)
  dT <- 15
  N <- length(dX)
  
  min <- 10000
  min.H <- 0
  for (H in seq(from=0.01, to=0.99, by=0.01)){
    m <- -1*loglik.DL(H)
    if (m < min){
      min <- m
      min.H <- H
    }
  }
  
  fbm.simulation(H)
}

