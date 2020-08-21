date <- '6-25'

directory <- 'C:/master/mstudy/data/long/csv/'
filename <- paste(directory, date, '.txt',sep='')

df <- read.csv(file = filename, header = TRUE, sep=',')

x <- as.POSIXct(df$datetime)

y <- df$ping
th <- mean(y)

z <- c()
for(i in y){
  if(is.na(i)){
    z <- c(z,0)
  }
  else if(i>80){
    z <- c(z,0)
  }
  else{
    z <- c(z,i)
  }
}

for (i in 1:(length(z)-1)){
  if(z[i] != 0){
    left <- i
    for(j in (i+1):length(z)){
      if(z[j] != 0){
        right <- j
        for(k in left:right){
          z[k] <- ((right-k)*z[left]+(k-left)*z[right])/(right-left)
        }
        i <- right - 1
        break
      }
    }
  }
}

z <- subset(z,z>0)
w <- c(z[1])
for (i in 2:length(z)){
  w[i] <- 0.8*w[i-1] + 0.2*z[i]
}

#out <- file(paste('C:/master/mstudy/data/long/fft/low_',date,'.txt',sep = ''), "w")
#for (i in w) {
#  writeLines(paste(i), out, sep="\n")
#}
#close(out)

pdf(paste('C:/master/mstudy/analysis/long/low_',date,'.pdf',sep=''),
    width = 7,
    height = 5
)

par(mar=c(3, 3, 1, 1.5))
par(mgp=c(2, 0.7, 0))

xlim <- c(as.POSIXct(paste('2020-',date,' 00:00:00',sep=''),format="%Y-%m-%d %H:%M:%S"),
          as.POSIXct(paste('2020-',date,' 23:59:59',sep=''),format="%Y-%m-%d %H:%M:%S")
)

plot(x,y,
     type='l',
     col='blue',
     xaxt="n",
     yaxt="n",
     xlim = xlim,
     ylim = c(0,240),
     xlab = 'time',
     ylab = 'Delay [ms]',
     xaxs = "i",
     yaxs = "i"
)

xvalues <- c(as.POSIXct(paste('2020-',date,"00:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct(paste('2020-',date,"04:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct(paste('2020-',date,"08:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct(paste('2020-',date,"12:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct(paste('2020-',date,"16:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct(paste('2020-',date,"20:00:00",sep=''),format="%Y-%m-%d %H:%M:%S"),
             as.POSIXct(paste('2020-',date,'23:59:59',sep=''),format="%Y-%m-%d %H:%M:%S")
)
xstrings <- c('0:00','','','12:00','16:00','','23:59')
par(new=TRUE)
axis(side=1, at=xvalues, labels=xstrings)

yvalues <- 0:(240/30)*30
ystrings <- c('0','','60','','120','','180','','240')
par(new=TRUE)
axis(side=2, at=yvalues, labels=ystrings)

par(new=TRUE)

plot(x[3:5729],w,
     type='l',
     col='red',
     xaxt="n",
     yaxt="n",
     xlim = xlim,
     ylim = c(0,240),
     xlab = '',
     ylab = '',
     xaxs = "i",
     yaxs = "i"
)

dev.off()
