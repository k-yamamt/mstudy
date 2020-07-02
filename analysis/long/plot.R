library('stringr')

date <- '7-1'
ymax <- 240

directory <- 'C:/master/mstudy/data/long/csv/'
filename <- paste(directory, date, '.csv',sep='')

df <- read.csv(file = filename, header = TRUE, sep=',')
  
pdf(paste('C:/master/mstudy/analysis/long/', 'plot-', date, '.pdf',sep=''),width = 7, height = 5)

par(mar=c(3, 3, 1, 1.5))
par(mgp=c(2, 0.7, 0))

x <- as.POSIXct(df$datetime)
y<- df$ping

plot(x,y,
     type='l',
     col='blue',
     xaxt="n",
     yaxt="n",
     ylim = c(0,ymax),
     xlab = 'time',
     ylab = 'Delay [ms]',
     xaxs = "i",
     yaxs = "i"
)

xvalues <- c()
xstrings <- c()
t <- 0
for(i in 1:length(x)){
  if(t == as.integer(substr(x[i],12,13))){
    xvalues <- c(xvalues,x[i])
    xstrings <- c(xstrings,paste(as.character(t),':00',sep = ''))
    t <- t + 4
  }
}
xvalues <- c(xvalues,x[i])
xstrings <- c(xstrings,paste(as.character(t),':00',sep = ''))
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
for(i in 1:length(x)){
  if(y[i] > ymax){
    par(new = TRUE)
    text(i,ymax-a[b%%2+1], y[i]%/%1, font = 2)
    b <- b + 1
  }
}

dev.off()
