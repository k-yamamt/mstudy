regression <- function(id){
  for (filename in files){
    df <- read.csv(file = filename, header = TRUE, sep=',')
    
    if(id == 'norm'){
      garch_result <- garchFit(formula = ~arma(2,2)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)
      y <- df$ping
      predict.y <- garch_result@fitted
      predict.y.upper <- predict.y + 1.96*garch_result@sigma.t
      predict.y.lower <- predict.y - 1.96*garch_result@sigma.t
      pdf(paste(str_sub(filename,-20,-14),'-plot.pdf',sep=''))
    }
    else if(id == 'diff'){
      garch_result <- garchFit(formula = ~arma(2,2)+garch(1,1), data = diff(df$ping), include.mean = TRUE, trace = FALSE)
      y <- df$ping
      predict.y <- c(y[1],y[1:length(y)-1]+garch_result@fitted)
      predict.y.upper <- predict.y + c(0,1.96*garch_result@sigma.t)
      predict.y.lower <- predict.y - c(0,1.96*garch_result@sigma.t)
      pdf(paste(str_sub(filename,-20,-14),'-plot-diff.pdf',sep=''))
    }
    
    ymax <- 200
    plot(predict.y.upper, type='l', col='green', ann = FALSE, axes = FALSE,xlim = c(0,length(predict.y.upper)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
    par(new = TRUE)
    plot(predict.y.lower, type='l', col='green', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y.lower)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
    par(new = TRUE)
    plot(y, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',xlim = c(0,length(y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
    par(new = TRUE)
    plot(predict.y, type='l', col='red', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
    for (i in 1:ymax%/%50){
      par(new = TRUE)
      plot(c(0,length(y)+1),c(i*50,i*50), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
    }
    for (i in 1:4){
      par(new = TRUE)
      plot(c(i*50,i*50),c(0,ymax), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
    }
    par(mar=c(3, 3, 1, 1))
    dev.off()
  }
}

exsampleRegPlot <- function(id){
  filename <- files[id]
  df <- read.csv(file = filename, header = TRUE, sep=',')
  
  garch_result <- garchFit(formula = ~arma(2,2)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)
  y <- df$ping
  predict.y <- garch_result@fitted
  predict.y.upper <- predict.y + 1.96*garch_result@sigma.t
  predict.y.lower <- predict.y - 1.96*garch_result@sigma.t
  pdf(paste(str_sub(filename,-20,-14),'-plot.pdf',sep=''), family = 'Japan1GothicBBB', width = 7, height = 4.5)
  par(mar=c(4,5,2,2))
  ymax <- 400
  plot(predict.y.upper, type='l', col='green', ann = FALSE, axes = FALSE,xlim = c(0,length(predict.y.upper)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(predict.y.lower, type='l', col='green', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y.lower)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(y, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',cex = 1.2, cex.lab = 1.5, cex.axis = 1.5,xlim = c(0,length(y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(predict.y, type='l', col='red', lwd = 2, ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  for (i in 1:ymax%/%50){
    par(new = TRUE)
    plot(c(0,length(y)+1),c(i*50,i*50), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  }
  for (i in 1:4){
    par(new = TRUE)
    plot(c(i*50,i*50),c(0,ymax), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  }
  #par(xpd=T)
  #legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('実測値','回帰線','信頼区間 \n(95%)'),
  #       lty = 1, col = c('blue','red','darkorange') ,cex = 1.4, bty = 'n')
  dev.off()
  
  garch_result <- garchFit(formula = ~arma(2,2)+garch(1,1), data = diff(df$ping), include.mean = TRUE, trace = FALSE)
  y <- diff(df$ping)
  predict.y <- garch_result@fitted
  predict.y.upper <- predict.y + 1.96*garch_result@sigma.t
  predict.y.lower <- predict.y - 1.96*garch_result@sigma.t
  pdf(paste(str_sub(filename,-20,-14),'-plot-diff.pdf',sep=''), family = 'Japan1GothicBBB', width = 7, height = 4.5)
  par(mar=c(4,5,2,2))
  ymax <- 150
  ymin <- -150
  plot(predict.y.upper, type='l', col='green', ann = FALSE, axes = FALSE,xlim = c(0,length(predict.y.upper)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(predict.y.lower, type='l', col='green', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y.lower)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(y, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',cex = 1.2, cex.lab = 1.5, cex.axis = 1.5, xlim = c(0,length(y)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(predict.y, type='l', col='red', lwd = 2, ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  for (i in 1:(ymax-ymin)%/%50-1){
    par(new = TRUE)
    plot(c(0,length(y)+1),c(ymin+i*50,ymin+i*50), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  }
  for (i in 1:4){
    par(new = TRUE)
    plot(c(i*50,i*50),c(ymin,ymax), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  }
  #par(xpd=T)
  #legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('変動値','回帰線','信頼区間 \n(95%)'),
  #       lty = 1, col = c('blue','red','darkorange'), cex = 1.4, bty = 'n')
  dev.off()
}

histgram <- function(){
  df1 <- read.csv(file = files[45], header = TRUE, sep=',')
  garch_result1 <- garchFit(formula = ~arma(2,2)+garch(1,1), data = diff(df1$ping), include.mean = TRUE, trace = FALSE)
  y1 <- garch_result1@fitted

  pdf('diff-hist-0309_07.pdf',width = 7, height = 5)
  par(mar=c(5,5,1,5))
  
  truehist(diff(df1$ping), xlim = c(-150, 140), h = 1, col="#0000FF7F", border = "#0000FF7F",
           axes = FALSE, xlab = "", ylab = "")
  axis(side = 1)
  axis(side = 2, col.axis = "#0000FF7F")
  mtext("probability of x", side = 2, line = 3, col = 'blue')
  mtext("Delay [ms]", side = 1, line = 3)
  
  par(new = TRUE)
  
  truehist(y1, xlim = c(-150, 150), h = 1, col = "#FF00007F", border = "#FF00007F",
           axes = FALSE, xlab = "", ylab = "")
  axis(side = 4, col.axis = "#FF00007F")
  mtext("probability of y", side = 4, line = 3, col = 'red')  
  dev.off()
}
histgram()
#regression(files,'norm')
#regression(files,'diff')

exsampleRegPlot(12)

normSSE <- 0
normLLH <- 0
diffSSE <- 0
diffLLH <- 0
for (filename in files){
  if (filename != "C:/master/mstudy/data/AWS/log-20200325_030001-ping.csv" && filename != "C:/master/mstudy/data/AWS/log-20200327_030001-ping.csv"){
    df <- read.csv(file = filename, header = TRUE, sep=',')
    garch_result <-garchFit(formula = ~arma(2,2)+garch(1,1), data = df$ping, trace = FALSE)
    normSSE <- normSSE + sum(garch_result@residuals**2)/length(garch_result@data)
    normLLH <- normLLH - garch_result@fit$llh

    garch_result <- garchFit(formula = ~arma(2,2)+garch(1,1), data = diff(df$ping), trace = FALSE)
    diffSSE <- diffSSE + sum(garch_result@residuals**2)/length(garch_result@data)
    diffLLH <- diffLLH - garch_result@fit$llh
  }
}
cat(normSSE/length(files)-2)
cat(normLLH/length(files)-2)
cat(diffSSE/length(files)-2)
cat(diffLLH/length(files)-2)
pdf('a2.pdf', width = 7, height = 4.5)
par(mar=c(3,3,1,1))
plot(density(Dnorm[,'a2']),xlim=c(-4,4),ylim=c(0,2),ann = FALSE, col = 'blue',lwd = 2)
dev.off()
pdf('scale_a2.pdf', width = 7, height = 4.5)
par(mar=c(3,3,1,1))
plot(density(scale(Dnorm[,'a2'])),xlim=c(-4,4),ylim=c(0,2),ann = FALSE, col = 'blue', lwd = 2)
dev.off()

for(i in 120:122){
  exsampleRegPlot(i)
}
exsampleRegPlot(40)
