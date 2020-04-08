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

#norm_AIC <- identifyNumParam('norm')
#diff_AIC <- identifyNumParam('diff')

#norm_AIC_num <- checkParamNum('norm')
#diff_AIC_num <- checkParamNum('diff')