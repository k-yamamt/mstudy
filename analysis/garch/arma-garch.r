library(stringr)
library(fGarch)
library(cluster)

files <- list.files('C:/master/mstudy/data/AWS/', pattern = '-ping.csv', full.names = TRUE)

identifyNumParam <- function(files){
  AIC <- matrix(nrow = 0,ncol = 2)
  colnames(AIC) <- c('(p,q,r,s)','AIC')
  date <- NULL
  
  for (filename in files[1:10]){
    df <- read.csv(file = filename, header = TRUE, sep=',')
    
    date <- c(date,str_sub(filename, start = -20, end = -14))
    best.p <- 0
    best.q <- 0
    best.r <- 0
    best.s <- 0
    min.aic <- 10000

    aic <- try(garchFit(formula = ~arma(0,0)+garch(1,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 0
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,0)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 0
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,0)+garch(1,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 0
      best.r <- 1
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,0)+garch(2,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 0
      best.r <- 2
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,0)+garch(2,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 0
      best.r <- 2
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,0)+garch(2,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 0
      best.r <- 2
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,1)+garch(1,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 1
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,1)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 1
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,1)+garch(1,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 1
      best.r <- 1
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,1)+garch(2,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 1
      best.r <- 2
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,1)+garch(2,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 1
      best.r <- 2
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,1)+garch(2,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 1
      best.r <- 2
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,2)+garch(1,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 2
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,2)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 2
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,2)+garch(1,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 2
      best.r <- 1
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,2)+garch(2,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 2
      best.r <- 2
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,2)+garch(2,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 2
      best.r <- 2
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(0,2)+garch(2,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 0
      best.q <- 2
      best.r <- 2
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,0)+garch(1,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 0
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,0)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 0
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,0)+garch(1,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 0
      best.r <- 1
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,0)+garch(2,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 0
      best.r <- 2
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,0)+garch(2,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 0
      best.r <- 2
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,0)+garch(2,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 0
      best.r <- 2
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,1)+garch(1,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 1
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,1)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 1
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,1)+garch(1,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 1
      best.r <- 1
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,1)+garch(2,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 1
      best.r <- 2
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,1)+garch(2,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 1
      best.r <- 2
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,1)+garch(2,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 1
      best.r <- 2
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,2)+garch(1,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 2
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,2)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 2
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,2)+garch(1,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 2
      best.r <- 1
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,2)+garch(2,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 2
      best.r <- 2
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,2)+garch(2,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 2
      best.r <- 2
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(1,2)+garch(2,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 1
      best.q <- 2
      best.r <- 2
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,0)+garch(1,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 0
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,0)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 0
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,0)+garch(1,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 0
      best.r <- 1
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,0)+garch(2,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 0
      best.r <- 2
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,0)+garch(2,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 0
      best.r <- 2
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,0)+garch(2,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 0
      best.r <- 2
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,1)+garch(1,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 1
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,1)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 1
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,1)+garch(1,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 1
      best.r <- 1
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,1)+garch(2,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 1
      best.r <- 2
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,1)+garch(2,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 1
      best.r <- 2
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,1)+garch(2,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 1
      best.r <- 2
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,2)+garch(1,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 2
      best.r <- 1
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,2)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 2
      best.r <- 1
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,2)+garch(1,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 2
      best.r <- 1
      best.s <- 2
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,2)+garch(2,0), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 2
      best.r <- 2
      best.s <- 0
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,2)+garch(2,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 2
      best.r <- 2
      best.s <- 1
      min.aic <- aic
    }
    aic <- try(garchFit(formula = ~arma(2,2)+garch(2,2), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$ics["AIC"])
    if (aic < min.aic){
      best.p <- 2
      best.q <- 2
      best.r <- 2
      best.s <- 2
      min.aic <- aic
    }
    AIC <- rbind(AIC,c(paste('(',best.p,',',best.q,',',best.r,',',best.s,')',sep = ''),min.aic))
    
  }
  rownames(AIC) <- date
  AIC
}

regression <- function(files,id){
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

params <- function(files,id){
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
  
  cluster <- cutree(hclust(dist(D,method = dist.method),method = clustering.method),k = k)
  
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
    par(mar=c(4,4,4,7))
    bar <- barplot(
      data,
      names.arg = 1:k,
      col = c('purple','green','red','darkorange','blue'),
      xlab = 'クラスタ番号',
      ylab = '割合',
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('20:00-21:00','17:00-18:00','12:00-13:00','7:00-8:00','3:00-4:00'),
          pch = 15, col = c('blue','darkorange','red','green','purple'),title = '時間帯')
    
    for (i in 1:length(num)){
      text(num[i],x = bar[i], y = 1.05)
    }
    text('総数', x = bar[length(bar)%/%2], y = 1.13)
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
    par(mar=c(4,4,4,7))
    bar <- barplot(
      data,
      names.arg = 1:k,
      col = c('gray','firebrick3','darkturquoise','green','goldenrod3','blue','red'),
      xlab = 'クラスタ番号',
      ylab = '割合'
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('日','土','金','木','水','火','月'),
          pch = 15, col = c('red','blue','goldenrod3','green','darkturquoise','firebrick3','gray'), title = '曜日')
    
    for (i in 1:length(num)){
      text(num[i],x = bar[i], y = 1.05)
    }
    text('総数',x = bar[length(bar)%/%2], y = 1.13)
  }
  
}

#AIC <- identifyNumParam(files)

regression(files,'norm')
regression(files,'diff')

Dnorm <- params(files,'norm')
Ddiff <- params(files,'diff')

#clustering('norm','euclidean','ward.D2',15,'timezone')
