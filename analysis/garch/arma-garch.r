library(stringr)
library(fGarch)
library(cluster)

files <- list.files('C:/master/mstudy/data/AWS/', pattern = '-ping.csv', full.names = TRUE)

identifyNumParam <- function(files){
  AIC <- matrix(nrow = 0,ncol = 2)
  colnames(AIC) <- c('(p,q,r,s)','AIC')
  date <- NULL
  
  for (filename in files){
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

regression <- function(files){
  for (filename in files){
    df <- read.csv(file = filename, header = TRUE, sep=',')
    
    garch_result <- garchFit(formula = ~arma(2,2)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)
    y <- df$ping
    predict.y <- garch_result@fitted
    predict.y.upper <- y + 1.96*garch_result@h.t
    predict.y.lower <- y - 1.96*garch_result@h.t
    ymax <- 500
    
    pdf(paste(str_sub(filename,-20,-14),'-plot.pdf',sep=''))
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

params <- function(files){
  D <- matrix(nrow = 0,ncol = 8)
  colnames(AIC) <- c('c','a1','a2','b1','b2','omega','alpha1','beta1')
  date <- NULL
  
  for (filename in files){
    date <- c(date,str_sub(filename, start = -20, end = -14))
    df <- read.csv(file = filename, header = TRUE, sep=',')
    garch_param <- garchFit(formula = ~arma(2,2)+garch(1,1), data = df$ping, include.mean = TRUE, trace = FALSE)@fit$coef
    D <- rbind(D,c(garch_param[1],garch_param[2],garch_param[3],garch_param[4],garch_param[5],garch_param[6],garch_param[7],garch_param[8]))
  }
  
  rownames(D) <- date
  D
}

clustering <- function(){
  
}

AIC <- identifyNumParam(files)

#regression(files)

#D <- params(files)

#clustering()