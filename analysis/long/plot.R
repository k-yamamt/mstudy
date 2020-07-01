library('stringr')

date <- '6-23'
ymax <- 180

directory <- 'C:/master/mstudy/data/long/csv/'
filename <- paste(directory, date, '.csv',sep='')

df <- read.csv(file = filename, header = TRUE, sep=',')
  
pdf(paste(directory, 'plot-', date, '.pdf',sep=''),width = 7, height = 5)
  
yvalues <- 0:(ymax/30)*30
ystrings <- 0:(ymax/30)*30
  
par(mar=c(3, 3, 1, 1))
par(mgp=c(2, 0.7, 0))

y <- df$ping

xvalues <- c()
xstrings <- c()

t <- 0
for (i in 1:length(y)){
  if(as.integer(substr(df$datetime[i],11,13)) == t){
    xvalues <- c(xvalues,i)
    xstrings <- c(xstrings,paste(as.character(t),':00',sep = ''))
    t <- t + 1
  }
}

plot(y,
     type='l',
     col='blue',
     xaxt="n",
     yaxt="n",
     xlim = c(0,length(y)),
     ylim = c(0,ymax),
     xlab = 'time',
     ylab = 'Delay [ms]',
     xaxs = "i",
     yaxs = "i"
)

for (i in 0:(ymax/30)*30){
  par(new = TRUE)
  plot(c(0,length(y)), c(i,i), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)), ylim = c(0,ymax), xaxs = "i", yaxs = "i" )
}

axis(side=1, at=xvalues, labels=xstrings)
axis(side=2, at=yvalues, labels=ystrings)
  
dev.off()