library(stringr)
library(fGarch)
library(cluster)
library(factoextra)
library(RColorBrewer)

files <- list.files('C:/master/mstudy/data/AWS/', pattern = '-ping.csv', full.names = TRUE)

identifyNumParam <- function(id){
  AIC <- matrix(nrow = 0,ncol = 2)
  colnames(AIC) <- c('(p,q,r,s)','AIC')
  date <- NULL

  for (filename in files){
    df <- read.csv(file = filename, header = TRUE, sep=',')
    if (id == 'norm'){
      data <-  df$ping
    }
    else if (id == 'diff'){
      data <- diff(df$ping)
    }
    
    date <- c(date,str_sub(filename, start = -20, end = -14))
    best.p <- 0
    best.q <- 0
    best.r <- 0
    best.s <- 0
    min.aic <- 10000

    aic <- try(garchFit(formula = ~arma(0,0)+garch(1,0), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 0
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,0)+garch(1,1), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 0
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,1)+garch(1,0), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 1
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,1)+garch(1,1), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 1
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    
    aic <- try(garchFit(formula = ~arma(0,2)+garch(1,0), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 2
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,2)+garch(1,1), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 2
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,0)+garch(1,0), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 0
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,0)+garch(1,1), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 0
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,1)+garch(1,0), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 1
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,1)+garch(1,1), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 1
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,2)+garch(1,0), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 2
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,2)+garch(1,1), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 2
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,0)+garch(1,0), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 0
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,0)+garch(1,1), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 0
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,1)+garch(1,0), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 1
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,1)+garch(1,1), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 1
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,2)+garch(1,0), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 2
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,2)+garch(1,1), data = data, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 2
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    AIC <- rbind(AIC,c(paste('(',best.p,',',best.q,',',best.r,',',best.s,')',sep = ''),min.aic))
    
  }
  rownames(AIC) <- date
  AIC
}

checkParamNum <- function(id){
  if (id == 'norm'){
    D <- Dnorm
  }
  else if(id == 'diff'){
    D <- Ddiff
  }
  
  AIC_num <- matrix(nrow = 0,ncol = 2)
  colnames(AIC) <- c('(p,q,r,s)','Num')
  
  for (p in 0:2){
    for (q in 0:2){
      r = 1
      for (s in 0:2){
        x <- 0
        param <- paste('(',p,',',q,',',r,',',s,')', sep = '')
        for (i in D){
          if (i == param){
            x <- x + 1
          }
        }
        AIC_num <- rbind(AIC_num,c(param,x))
      }
    }
  }
  
  AIC_num
}

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
    
    ymax <- 500
    plot(predict.y.upper, type='l', col='darkorange', ann = FALSE, axes = FALSE,xlim = c(0,length(predict.y.upper)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
    par(new = TRUE)
    plot(predict.y.lower, type='l', col='darkorange', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y.lower)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
    par(new = TRUE)
    plot(predict.y, type='l', col='red', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
    par(new = TRUE)
    plot(y, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',xlim = c(0,length(y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
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

exsampleRegPlot(id){
  filename <- files[id]
  df <- read.csv(file = filename, header = TRUE, sep=',')
  
  garch_result <- garchFit(formula = ~arma(2,2)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)
  y <- df$ping
  predict.y <- garch_result@fitted
  predict.y.upper <- predict.y + 1.96*garch_result@sigma.t
  predict.y.lower <- predict.y - 1.96*garch_result@sigma.t
  pdf(paste(str_sub(filename,-20,-14),'-plot.pdf',sep=''), family = 'Japan1GothicBBB', width = 7, height = 4.5)
  par(mar=c(4,5,2,9))
  ymax <- 200
  plot(predict.y.upper, type='l', col='darkorange', ann = FALSE, axes = FALSE,xlim = c(0,length(predict.y.upper)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(predict.y.lower, type='l', col='darkorange', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y.lower)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(predict.y, type='l', col='red', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(y, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',cex = 1.2, cex.lab = 1.5, cex.axis = 1.5,xlim = c(0,length(y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  for (i in 1:ymax%/%50){
    par(new = TRUE)
    plot(c(0,length(y)+1),c(i*50,i*50), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  }
  for (i in 1:4){
    par(new = TRUE)
    plot(c(i*50,i*50),c(0,ymax), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(0,ymax), xaxs = "i", yaxs = "i")
  }
  par(xpd=T)
  legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('実測値','回帰線','信頼区間 \n(95%)'),
         lty = 1, col = c('blue','red','darkorange') ,cex = 1.4, bty = 'n')
  dev.off()
  
  garch_result <- garchFit(formula = ~arma(2,2)+garch(1,1), data = diff(df$ping), include.mean = TRUE, trace = FALSE)
  y <- diff(df$ping)
  predict.y <- garch_result@fitted
  predict.y.upper <- predict.y + 1.96*garch_result@sigma.t
  predict.y.lower <- predict.y - 1.96*garch_result@sigma.t
  pdf(paste(str_sub(filename,-20,-14),'-plot-diff.pdf',sep=''), family = 'Japan1GothicBBB', width = 7, height = 4.5)
  par(mar=c(4,5,2,9))
  ymax <- 150
  ymin <- -150
  plot(predict.y.upper, type='l', col='darkorange', ann = FALSE, axes = FALSE,xlim = c(0,length(predict.y.upper)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(predict.y.lower, type='l', col='darkorange', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y.lower)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(predict.y, type='l', col='red', ann = FALSE, axes = FALSE, xlim = c(0,length(predict.y)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  par(new = TRUE)
  plot(y, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',cex = 1.2, cex.lab = 1.5, cex.axis = 1.5, xlim = c(0,length(y)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  for (i in 1:(ymax-ymin)%/%50-1){
    par(new = TRUE)
    plot(c(0,length(y)+1),c(ymin+i*50,ymin+i*50), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  }
  for (i in 1:4){
    par(new = TRUE)
    plot(c(i*50,i*50),c(ymin,ymax), type='l', lty=3, ann = FALSE, axes = FALSE, xlim = c(0,length(y)+1),ylim = c(ymin,ymax), xaxs = "i", yaxs = "i")
  }
  par(xpd=T)
  legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('変動値','回帰線','信頼区間 \n(95%)'),
         lty = 1, col = c('blue','red','darkorange'), cex = 1.4, bty = 'n')
  dev.off()
}

params <- function(id){
  D <- matrix(nrow = 0,ncol = 8)
  colnames(D) <- c('c','a1','a2','b1','b2','omega','alpha1','beta1')
  date <- NULL
  
  for (filename in files){
    df <- read.csv(file = filename, header = TRUE, sep=',')
    
    if(id == 'norm'){
      tryCatch(
        {
          garch_param <- garchFit(formula = ~arma(2,2)+garch(1,1), data = df$ping, trace = FALSE)@fit$coef
          date <- c(date,str_sub(filename, start = -20, end = -14))
          D <- rbind(D,c(garch_param[1],garch_param[2],garch_param[3],garch_param[4],garch_param[5],garch_param[6],garch_param[7],garch_param[8]))
        }
        ,error = function(e){
          cat(filename)
          }
        )
    }else if(id == 'diff'){
      tryCatch(
        {
          garch_param <- garchFit(formula = ~arma(2,2)+garch(1,1), data = diff(df$ping), trace = FALSE)@fit$coef
          date <- c(date,str_sub(filename, start = -20, end = -14))
          D <- rbind(D,c(garch_param[1],garch_param[2],garch_param[3],garch_param[4],garch_param[5],garch_param[6],garch_param[7],garch_param[8]))
        }
        ,error = function(e){
          cat(filename)
          }
        )
    }
  }
  
  rownames(D) <- date
  D
}

clustering <- function(dataID,dist.method,clustering.method,k,type){
  if(dataID == 'norm'){
    D <- Dnorm
  }
  else if(dataID == 'diff'){
    D <- Ddiff
  }
  else if(dataID == 'norm_comp'){
    D <- Dnorm_comp
  }
  else if(dataID == 'diff_comp'){
    D <- Ddiff_comp
  }
  
  if (clustering.method == 'kmeans'){
    cluster <- kmeans(D, k)$cluster
    filename <- paste(dataID,'-eucl-kmean-',k,'-',type,'.pdf',sep = '')
    }
  else{
    cluster <- hcut(D, k = k, hc_func = 'hclust', hc_method = clustering.method, hc_metric = dist.method)$cluster
    filename <- paste(dataID,'-',str_sub(dist.method,1,4),'-',str_sub(clustering.method,1,4),'-',k,'-',type,'.pdf',sep = '')
    }
    
  if (type == 'timezone'){
    data <- matrix(0,nrow = 5, ncol = k)
    rownames(data) <- c('03','07','12','17','20')
    colnames(data) <- 1:k
    for (i in 1:length(cluster)){
      data[str_sub(names(cluster[i]),-2),cluster[i]] <- data[str_sub(names(cluster[i]),-2),cluster[i]] + 1
    }
    num <- colSums(data)
    for (i in 1:length(num)){
      data[,i] <- data[,i]/num[i]
    }
    
    pdf(filename, height = 5, width =7)
    par(mar=c(4,4,4,7))
    bar <- barplot(
      data,
      names.arg = 1:k,
      col = c('purple','green','red','darkorange','blue'),
      xlab = 'cluster index',
      ylab = 'proportion',
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('20:00-21:00','17:00-18:00','12:00-13:00','7:00-8:00','3:00-4:00'),
          pch = 15, col = c('blue','darkorange','red','green','purple'),title = 'time zone')
    for (i in 1:length(num)){
      text(num[i],x = bar[i], y = 1.05)
    }
    text('sum', x = (bar[1]+bar[length(bar)])/2, y = 1.13)
    
    dev.off()
  }
  else if(type == 'day'){
    data <- matrix(0,nrow = 7, ncol = k)
    rownames(data) <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
    colnames(data) <- 1:k
    for (i in 1:length(cluster)){
      day <- str_sub(names(cluster[i]),start = 3, end = 4)
      if(day == '29'){
        id <- 'Sat'
      }
      else{
        x <- as.integer(day)%%7
        if(x == 1){
          id <- 'Sun'
        }
        else if(x == 2){
          id <- 'Mon'
        }
        else if(x == 3){
          id <- 'Tue'
        }
        else if(x == 4){
          id <- 'Wed'
        }
        else if(x == 5){
          id <- 'Thu'
        }
        else if(x == 6){
          id <- 'Fri'
        }
        else if(x == 0){
          id <- 'Sat'
        }
      }
      data[id,cluster[i]] <- data[id,cluster[i]] + 1
    }
    num <- colSums(data)
    for (i in 1:length(num)){
      data[,i] <- data[,i]/num[i]
    }
    
    pdf(filename,height = 5, width = 7)
    par(mar=c(4,4,4,7))
    bar <- barplot(
      data,
      names.arg = 1:k,
      col = c('gray','firebrick3','darkturquoise','green','goldenrod3','blue','red'),
      xlab = 'cluster index',
      ylab = 'proportion'
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('Sun','Sat','Fri','Thu','Wed','Tue','Mon'),
          pch = 15, col = c('red','blue','goldenrod3','green','darkturquoise','firebrick3','gray'), title = 'day of week')
    for (i in 1:length(num)){
      text(num[i],x = bar[i], y = 1.05)
    }
    text('sum', x = (bar[1]+bar[length(bar)])/2, y = 1.13)
    
    dev.off()
  }
  else if(type == 'timezone-day'){
    data <- matrix(0,nrow = k, ncol = 35)
    rownames(data) <- 1:k
    colnames(data) <- c('Mon-03','Mon-07','Mon-12','Mon-17','Mon-20',
                        'Tue-03','Tue-07','Tue-12','Tue-17','Tue-20',
                        'Wed-03','Wed-07','Wed-12','Wed-17','Wed-20',
                        'Thu-03','Thu-07','Thu-12','Thu-17','Thu-20',
                        'Fri-03','Fri-07','Fri-12','Fri-17','Fri-20',
                        'Sat-03','Sat-07','Sat-12','Sat-17','Sat-20',
                        'Sun-03','Sun-07','Sun-12','Sun-17','Sun-20')
    
    for (i in 1:length(cluster)){
      day <- str_sub(names(cluster[i]),start = 3, end = 4)
      if(day == '29'){
        id <- 'Sat'
      }
      else{
        x <- as.integer(day)%%7
        if(x == 1){
          id <- 'Sun'
        }
        else if(x == 2){
          id <- 'Mon'
        }
        else if(x == 3){
          id <- 'Tue'
        }
        else if(x == 4){
          id <- 'Wed'
        }
        else if(x == 5){
          id <- 'Thu'
        }
        else if(x == 6){
          id <- 'Fri'
        }
        else if(x == 0){
          id <- 'Sat'
        }
      }
      data[cluster[i],paste(id,'-',str_sub(names(cluster[i]),-2),sep = '')] <- data[cluster[i],paste(id,'-',str_sub(names(cluster[i]),-2),sep = '')] + 1
    }
    num <- colSums(data)
    for (i in 1:length(num)){
      data[,i] <- data[,i]/num[i]
    }
    
    pdf(filename, height = 5, width =7, family = 'Japan1GothicBBB')
    par(mar=c(4,4,4,7))
    color <- brewer.pal(k,"Spectral")
    bar <- barplot(
      data,
      names.arg = 1:35,
      xlab = 'time zone and day of week',
      ylab = 'proportion',
      xaxt="n",
      col = color
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = rev(1:k),
           pch = 15, col = rev(color), title = 'cluster index')
    
    for (i in 1:length(num)){
      text(num[i],x = bar[i], y = 1.05)
    }
    text('sum', x = (bar[1]+bar[length(bar)])/2, y = 1.13)
    barHalfWidth = (bar[2] - bar[1])/2
    day <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
    for(i in 0:6){
      segments(bar[i*5+1]-barHalfWidth,0,bar[i*5+1],-0.04)
      segments(bar[i*5+1],-0.04,bar[i*5+5],-0.04)
      segments(bar[i*5+5],-0.04,bar[i*5+5]+barHalfWidth,0)
      text(day[i+1], x = bar[i*5+3], y = -0.06)
      text('時間帯は左から順に 3:00-4:00,7:00-8:00,12:00-13:00,17:00-18:00,20:00-21:00',x = bar[1] - barHalfWidth*10, y = -0.15, pos = 4)
    }
    
    dev.off()
  }
}

#norm_AIC <- identifyNumParam('norm')
#diff_AIC <- identifyNumParam('diff')

#norm_AIC_num <- AIC_num('norm')
#diff_AIC_num <- AIC_num('diff')

#regression(files,'norm')
#regression(files,'diff')

#exsampleRegPlot(8)

Dnorm <- params('norm')
Ddiff <- params('diff')
Dnorm_comp <- princomp(scale(Dnorm))$scores[,0:3]
Ddiff_comp <- princomp(scale(Ddiff))$scores[,0:3]

#summary(princomp(scale(Dnorm)))
#princomp(scale(Dnorm))$loadings
#summary(princomp(scale(Ddiff)))
#princomp(scale(Ddiff))$loadings

data <- c('norm','diff','norm_comp','diff_comp')
distance <- c('euclidean','manhattan','canberra','maximum','binary')
method <- c('kmeans','single','centroid','ward.D2','complete','average','mcquitty')
k <- c(6,7,9,12,15)
trend <- c('timezone','day','timezone-day')

#clustering(data[1],distance[2],method[2],k[4],trend[1])
clustering('norm','euclidean','kmeans',7,'timezone-day')

for(a in data){
  for(b in distance[1:3]){
    for(c in method[2:4]){
      for(d in k){
        for (e in trend){
          clustering(a,b,c,d,e)
        }
      }
    }
  }
}

for(a in data){
  for(d in k){
    for (e in trend){
      clustering(a,distance[1],method[1],d,e)
    }
  }
}