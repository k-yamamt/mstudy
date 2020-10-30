# RUN first to include library and move current directory #
{
  library('stringr')
  library('fitdistrplus')
  library('mixtools')
  library('hash')
  setwd('C:/master/mstudy/analysis/model/method4')
}

# read Data(df, noise) NOT NECESSARY RUNNING #
{
  alpha <- c()
  xhat <- c()
  L1 <- c()
  L2 <- c()
  noise <- c()
  for (file in list.files('dataset', pattern = '.txt', full.names = TRUE) ){
    data <- read.csv(file, header = FALSE)[,1]
    if (str_detect(file, pattern = 'alpha'))  alpha <- c(alpha,data)
    else if (str_detect(file, pattern = 'xhat'))  xhat <- c(xhat,data)
    else if (str_detect(file, pattern = 'L1'))  L1 <- c(L1,data)
    else if (str_detect(file, pattern = 'L2'))  L2 <- c(L2,data)
    else if (str_detect(file, pattern = 'noise'))  noise <- c(noise,data)
  }
  
  df <- data.frame(alpha = alpha, xhat = xhat, L1 = L1, L2 = L2)
  rm (alpha, xhat, L1, L2, data, file)
}

# experiment of fitting single distribution #
{
  distribution <- c('norm', 'lnorm', 'exp', 'pois', 'cauchy', 'gamma', 'logis', "nbinom", 'geom', 'beta', 'weibull')
  AIC <- matrix(nrow = length(distribution), ncol = length(df))
  rownames(AIC) <- distribution
  colnames(AIC) <- colnames(df)
  
  for (dist in distribution){
    for (col in colnames(df)){
      tryCatch(
        {
          AIC[dist,col] <- fitdist(df[,col], dist)$aic
        },
        error = function(e) {}
      )
    }
  }
  
  addNoise <- matrix(nrow = length(distribution), ncol = 1)
  rownames(addNoise) <- distribution
  colnames(addNoise) <- 'noise'
  for (dist in distribution){
    tryCatch(
      {
        addNoise[dist,'noise'] <- fitdist(noise[!is.na(noise)], dist)$aic
      },
      error = function(e) {}
    )
  }
  
  AIC <- cbind(AIC,addNoise)
  result <- paste('alpha : ', names(which.min(AIC[,'alpha'])), '\n',
                  'xhat : ', names(which.min(AIC[,'xhat'])), '\n',
                  'L1 : ', names(which.min(AIC[,'L1'])), '\n',
                  'L2 : ', names(which.min(AIC[,'L2'])), '\n',
                  'noise : ', names(which.min(AIC[,'noise'])), '\n'
  )
  
  cat (result)
  
  rm(addNoise, AIC, col, dist, distribution)
}

# model fitting, get models, NOT NECESSARY RUNNING #
#results : alpha <- GMM, xhat <- logis, L1 <- cauchy, L2 <- GMM, noise <- norm #
{
  params <- hash()
  alpha_gmm <- normalmixEM(df$alpha, k=3)
  xhat_logis <- fitdist(df$xhat, 'logis')
  L1_cauchy <- fitdist(df$L1, 'cauchy')
  L2_gmm <- normalmixEM(df$L2, k=2)
  noise_norm <- fitdist(noise[!is.na(noise)], 'norm')
  
  models <- hash()
  models['alpha'] <- alpha_gmm
  models['xhat'] <- xhat_logis
  models['L1'] <- L1_cauchy
  models['L2'] <- L2_gmm
  models['noise'] <- noise_norm
  
  rm(alpha_gmm, xhat_logis, L1_cauchy, L2_gmm, noise_norm)
}

# plot fitting distribution #
{
  ## alpha - gaussian mixture model ##
  {
    pdf('plot/fit_alpha.pdf', width = 7, height = 5)
    par(mar=c(3, 3, 1, 1.5))
    par(mgp=c(2, 0.7, 0))
    hist(models$alpha$x,
         probability = TRUE,
         breaks = 150,
         main = '',
         xlab = expression(italic(alpha[i])),
         ylab = 'Density',
         xlim = c(0,1),
         ylim = c(0,10),
         col = 'blue'
         )
    curve(models$alpha$lambda[1]*dnorm(x,models$alpha$mu[1],models$alpha$sigma[1]) + models$alpha$lambda[2]*dnorm(x,models$alpha$mu[2],models$alpha$sigma[2]) + models$alpha$lambda[3]*dnorm(x,models$alpha$mu[3],models$alpha$sigma[3]),
          col = 'red',
          add = TRUE
          )
    dev.off()
  }
  ## xhat - logistic distribution ##
  {
    pdf('plot/fit_xhat.pdf', width = 7, height = 5)
    par(mar=c(3, 3, 1, 1.5))
    par(mgp=c(2, 0.7, 0))
    denscomp(models$xhat,
             main = '',
             xlab = expression(paste(italic(hat(x)[i]), '[ms]')),
             xlim = c(50,90),
             ylim = c(0,0.2),
             breaks = 100,
             datacol = 'blue',
             addlegend = FALSE
    )
    dev.off()
  }
  ## L1 - cauchy distribution ##
  {
    pdf('plot/fit_L1.pdf', width = 7, height = 5)
    par(mar=c(3, 3, 1, 1.5))
    par(mgp=c(2, 0.7, 0))
    denscomp(models$L1,
             main = '',
             xlab = expression(italic(b[i] - a[i])),
             xlim = c(0,800),
             ylim = c(0,0.015),
             breaks = 250,
             datacol = 'blue',
             addlegend = FALSE
    )
    dev.off()
  }
  ## L2 - gaussian mixture model ##
  {
    pdf('plot/fit_L2.pdf', width = 7, height = 5)
    par(mar=c(3, 3, 1, 1.5))
    par(mgp=c(2, 0.7, 0))
    hist(models$L2$x,
         probability = TRUE,
         breaks = 50,
         main = '',
         xlab = expression(italic(a[i+1] - b[i])),
         ylab = 'Density',
         xlim = c(0,50),
         ylim = c(0,0.2),
         col = 'blue'
    )
    curve(models$L2$lambda[1]*dnorm(x,models$L2$mu[1],models$L2$sigma[1]) + models$L2$lambda[2]*dnorm(x,models$L2$mu[2],models$L2$sigma[2]),
          col = 'red',
          add = TRUE
    )
    dev.off()
  }
  ## noise - gaussian distribution ##
  {
    pdf('plot/fit_noise.pdf', width = 7, height = 5)
    par(mar=c(3, 3, 1, 1.5))
    par(mgp=c(2, 0.7, 0))
    denscomp(models$noise,
             main = '',
             xlab = expression(paste(italic(epsilon[i]), '[ms]')),
             xlim = c(-40,40),
             ylim = c(0,0.15),
             breaks = 250,
             datacol = 'blue',
             addlegend = FALSE
    )
    dev.off()
  }
}

# generate elements of sawtooth wave #
{
  ## get xhat from xhat ##
  get_xhat <- function(x){
    cor <- 0.5097466
    loc <- models$xhat$estimate['location']
    scale <- models$xhat$estimate['scale']
    if (is.na(x)){
      x <- rlogis(n = 1, loc, scale)
    }
    x_inv <- plogis(x,loc,scale)
    x_inv_temp <- qnorm(x_inv, 0, 1)
    y_inv_temp <- cor*x_inv_temp + sqrt(1-cor^2)*rnorm(1)
    y_inv <- pnorm(y_inv_temp)
    y <- qlogis(y_inv, loc, scale)
    
    return (y)
    rm(cor, loc, scale, x_inv, x_inv_temp, y_inv_temp, y_inv, y)
  }
  
  ## get alpha from xhat ##
  get_alpha <- function(x){
    cor <- -0.469427
    loc <- models$xhat$estimate['location']
    scale <- models$xhat$estimate['scale']
    x_inv <- plogis(x,loc,scale)
    x_inv_temp <- qnorm(x_inv, 0, 1)
    y_inv_temp <- cor*x_inv_temp + sqrt(1-cor^2)*rnorm(1)
    y_inv <- pnorm(y_inv_temp)
    f <- function(x){
      models$alpha$lambda[1]*dnorm(x,models$alpha$mu[1],models$alpha$sigma[1]) + models$alpha$lambda[2]*dnorm(x,models$alpha$mu[2],models$alpha$sigma[2]) + models$alpha$lambda[3]*dnorm(x,models$alpha$mu[3],models$alpha$sigma[3])
    } 
    y <- 0
    while(TRUE){
      y_inv_prime <- integrate(f, lower = -10, upper = y)$value
      if(y_inv_prime >= y_inv)  return (y)
      else  y <- y + 0.001
    }
    
    rm(cor, loc, scale, x_inv, x_inv_temp, y_inv_temp, y_inv, f, y, y_inv_temp)
  }
  
  ## get L1 from alpha ##
  get_L1 <- function(x){
    cor <- -0.3546743
    f <- function(x){
      models$alpha$lambda[1]*dnorm(x,models$alpha$mu[1],models$alpha$sigma[1]) + models$alpha$lambda[2]*dnorm(x,models$alpha$mu[2],models$alpha$sigma[2]) + models$alpha$lambda[3]*dnorm(x,models$alpha$mu[3],models$alpha$sigma[3])
    }
    x_inv <- integrate(f, lower = -10, upper = x)$value
    x_inv_temp <- qnorm(x_inv, 0, 1)
    y_inv_temp <- cor*x_inv_temp + sqrt(1-cor^2)*rnorm(1)
    y_inv <- pnorm(y_inv_temp)
    y <- qcauchy(y_inv, location = models$L1$estimate['location'], scale = models$L1$estimate['scale'])
    y <- round(y)
    
    if (y >= 20)  return (y)
    else  return (20)
    rm(cor, f, x_inv, x_inv_temp, y_inv_temp, y_inv, y)
  }
  
  ## get L2 ##
  get_L2 <- function(){
    y <- rnormmix(1, lambda = models$L2$lambda, mu = models$L2$mu, sigma = models$L2$sigma)
    y <- round(y)
    if (y >= 1) return (y)
    else  return (get_L2())
  }
  
  ## get noise ##
  get_noise <- function(){
    return (rnorm(1, models$noise$estimate['mean'], models$noise$estimate['sd']))
  }
}

# test generate sawtooth wave model #
{
  predict_xhat <- c(NaN)
  for (i in 1:10000){ predict_xhat[i+1] <- get_xhat(predict_xhat[i]) }
  predict_xhat <- predict_xhat[-1]
  
  predict_alpha <- c()
  for (i in 1:10000){ predict_alpha[i] <- get_alpha(predict_xhat[i]) }
  
  predict_L1 <- c()
  for (i in 1:10000){ predict_L1[i] <- get_L1(predict_alpha[i]) }
  
  predict_L2 <- c()
  for (i in 1:10000){ predict_L2[i] <- get_L2() }
  
  predict_noise <- c()
  for (i in 1:10000){ predict_noise[i] <- get_noise() }
  
  rm (predict_xhat, predict_alpha, predict_L1, predict_L2, get_noise)
}

# generate sawtooth wave #
{
  predict <- c(get_xhat[NaN])
  
}