df <- read.csv(file = 'C:/master/mstudy/data/sequence/sequence.csv', header = TRUE, sep=',')

for(z in 0:23){
  pdf(paste(z*5, '-', (z+1)*5, '.pdf', sep = ''),width = 7, height = 5)
  
  xmin <- z*200 + 1
  xmax <- (z+1)*200
  ymax <- 300
  
  y <- c()
  
  xsplit <- c()
  
  i <- xmin
  while(i <= xmax){
    y <- c(y,df$ping[i:(i+9)])
    y <- c(y,c(NA,NA))
    xsplit <- c(xsplit, length(y)-0.5)
    i <- i + 10
  }
  
  xvalues <- c(1, length(y)-2)
  xstrings <- c(xmin, xmax)
  
  yvalues <- 0:(ymax/30)*30
  ystrings <- 0:(ymax/30)*30
  
  par(mar=c(3, 3, 1, 1))
  par(mgp=c(2, 0.7, 0))
  
  plot(y,
       type='o',
       cex = 0.5,
       col='blue',
       xaxt="n",
       yaxt="n",
       xlim = c(-1,length(y)),
       ylim = c(0,ymax),
       xlab = 't',
       ylab = 'Delay [ms]',
       xaxs = "i",
       yaxs = "i"
       )
  
  for (i in xsplit){
    par(new = TRUE)
    plot(c(i,i), c(0,ymax), type='l', ann = FALSE, axes = FALSE, xlim = c(-1,length(y)), ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  }
  
  for (i in range(ymax/30)){
    
  }
  for (i in 0:(ymax/30)*30){
    par(new = TRUE)
    plot(c(1,length(y)-2), c(i,i), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(-1,length(y)), ylim = c(0,ymax), xaxs = "i", yaxs = "i" )
  }
  
  axis(side=1, at=xvalues, labels=xstrings)
  axis(side=2, at=yvalues, labels=ystrings)

  dev.off()
}