library('stringr')

df1 <- read.csv(file = 'C:/master/mstudy/data/sequence/comp/comp1.txt', header = TRUE, sep=',')
df2 <- read.csv(file = 'C:/master/mstudy/data/sequence/comp/comp2.txt', header = TRUE, sep=',')
x1 <- as.POSIXct(df1$datetime)
y1 <- df1$ping
x2 <- as.POSIXct(df2$datetime)
y2 <- df2$ping

ymax <- 400
start <- "05:11.500"
end <- "05:12.500"

pdf('C:/master/mstudy/analysis/sequence/10ms/05_11.pdf',width = 7, height = 5)

par(mar=c(3, 3, 1, 1.5))
par(mgp=c(2, 0.7, 0))

xlim <- c(as.POSIXct(paste("2020-08-03 19:", start, sep=''),format="%Y-%m-%d %H:%M:%OS"),
          as.POSIXct(paste("2020-08-03 19:", end, sep=''),format="%Y-%m-%d %H:%M:%OS")
)

plot(x1,y1,
     type='o',
     col='blue',
     xaxt="n",
     yaxt="n",
     xlim = xlim,
     ylim = c(0,ymax),
     xlab = 'time [s]',
     ylab = 'Delay [ms]',
     xaxs = "i",
     yaxs = "i"
)

par(new=TRUE)

plot(x2,y2,
     type='o',
     col='red',
     xaxt="n",
     yaxt="n",
     xlim = xlim,
     ylim = c(0,ymax),
     xlab = '',
     ylab = '',
     xaxs = "i",
     yaxs = "i"
)

xvalues <- xlim
xstrings <- c('0','1')
par(new=TRUE)
axis(side=1, at=xvalues, labels=xstrings)

yvalues <- c(0,100,200,300,400)
ystrings <- c(0,100,200,300,400)
par(new=TRUE)
axis(side=2, at=yvalues, labels=ystrings)

dev.off()

