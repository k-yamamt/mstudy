library('stringr')

ymax <- 240
#dir <- 'C:/master/mstudy/data/long/csv'

#for (filename in list.files(dir)){

filename <- 'C:/master/mstudy/data/FTP/15sFTP/exam2-day1-other.txt'

  #df <- read.csv(file = paste(dir, '/', filename, sep = ''), header = TRUE, sep=',')
  df <- read.csv(file = filename, header = TRUE, sep=',')
  
  pdf(paste('C:/master/mstudy/analysis/FTP/15sFTP/exam2-other.pdf',sep=''),
      width = 7,
      height = 5
      )

  par(mar=c(3, 3, 1, 1.5))
  par(mgp=c(2, 0.7, 0))
  
  x <- as.POSIXct(df$datetime)
  y<- df$ping
  #date <- str_sub(x[1],1,11)
  date <- "2020-07-17 "
  xlim <- c(as.POSIXct(paste(date,"16:10:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
            as.POSIXct(paste(date,'17:09:59',sep=''),format="%Y-%m-%d %H:%M:%S")
  )
  
  plot(x,y,
       type='l',
       col='blue',
       xaxt="n",
       yaxt="n",
       xlim = xlim,
       ylim = c(0,ymax),
       xlab = 'time',
       ylab = 'Delay [ms]',
       xaxs = "i",
       yaxs = "i"
  )
  
  for(i in 1:length(x)){
    if(is.na(y[i])){
      par(new = TRUE)
      plot(c(as.numeric(x[i]),as.numeric(x[i])),c(0,ymax),
           lty=3,
           type='l',
           col='red',
           ann = FALSE,
           axes = FALSE,
           xlim = xlim,
           ylim = c(0,ymax),
           xaxs = "i",
           yaxs = "i"
      )
    }
  }
  
  xvalues <- c(as.POSIXct(paste(date,"00:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
               as.POSIXct(paste(date,"04:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
               as.POSIXct(paste(date,"08:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
               as.POSIXct(paste(date,"12:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
               as.POSIXct(paste(date,"16:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
               as.POSIXct(paste(date,"20:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
               as.POSIXct(paste(date,'23:59:59',sep=''),format="%Y-%m-%d %H:%M:%S")
  )
  xstrings <- c('0:00','','','12:00','','','23:59')
  par(new=TRUE)
  axis(side=1, at=xvalues, labels=xstrings)
  
  yvalues <- 0:(ymax/30)*30
  ystrings <- c('0','','60','','120','','180','','240')
  par(new=TRUE)
  axis(side=2, at=yvalues, labels=ystrings)
  
  a <- c(5,20)
  b<- 1
  for(i in 1:length(x)){
    if(!is.na(y[i])){
      if(y[i] > ymax){
        par(new = TRUE)
        text(x[i],ymax-a[b%%2+1], y[i]%/%1, font = 2)
        b <- b + 1
      }
    }
  }

  dev.off()
#}
