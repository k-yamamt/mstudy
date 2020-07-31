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

out <- file(paste('C:/master/mstudy/data/long/fft/low_',date,'.txt',sep = ''), "w")
for (i in w) {
  writeLines(paste(i), out, sep="\n")
}
close(out)

#plot(x,y, type='l', ylim = c(0,120), col = 'blue')
#par(new = TRUE)
#plot(x,w, type='l', ylim = c(0,120), col = 'red')
