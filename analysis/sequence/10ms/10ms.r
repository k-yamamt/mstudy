library('stringr')

t <- c(1,120000)

filename <- 'C:/master/mstudy/data/10ms/10ms.txt'

df <- read.csv(file = filename, header = TRUE, sep=',')

pdf(paste('C:/master/mstudy/analysis/sequence/10ms/ext.pdf',sep = '') ,width = 7, height = 5)

par(mar=c(3, 3, 1, 1.5))
par(mgp=c(2, 0.7, 0))

x <- as.POSIXct(df$datetime)
y<- df$ping
ymax <- 300

w <- c()
z <- c()
for(i in 0:(length(x)%/%1500)){
  w <- c(w,i*1500+1)
  z <- c(z,y[i*1500+1])
}

plot(w,z,
     type='o',
     cex = 0.5,
     col='blue',
     xaxt="n",
     yaxt="n",
     xlim = c(0,w[length(w)]),
     ylim = c(0,ymax),
     xlab = 'time',
     ylab = 'Delay [ms]',
     xaxs = "i",
     yaxs = "i"
)

plot(y,
     type='o',
     cex = 0.5,
     col='blue',
     xaxt="n",
     yaxt="n",
     xlim = c(36570,36650),
     #ylim = c(0,ymax),
     xlab = 'time',
     ylab = 'Delay [ms]',
     xaxs = "i",
     yaxs = "i"
)

#xvalues <- c()
#xstrings <- c()
#t <- 0
#for(i in 1:length(x)){
#  if(t == as.integer(str_split(str_split(x[i],' ')[[1]][2],':')[[1]][1])){
#    xvalues <- c(xvalues,x[i])
#    xstrings <- c(xstrings,paste(as.character(t),':00',sep = ''))
#    t <- t + 4
#  }
#}

xvalues <- t
xstrings <- t
axis(side=1, at=xvalues, labels=xstrings)

yvalues <- 0:(ymax/30)*30
ystrings <- 0:(ymax/30)*30
for (i in 0:(ymax/30)*30){
  par(new = TRUE)
  plot(c(0,length(y)), c(i,i), type='l', lty=3, ann = FALSE, axes = FALSE, ylim = c(0,ymax), xaxs = "i", yaxs = "i" )
}
axis(side=2, at=yvalues, labels=ystrings)

a <- c(5,20)
b<- 1
for(i in t){
  if(y[i] > ymax){
    par(new = TRUE)
    text(i,ymax-a[b%%2+1], y[i]%/%1, font = 2)
    b <- b + 1
  }
}

dev.off()
