library(stringr)
library(fGarch)
library(cluster)
library(factoextra)
library(RColorBrewer)

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

files <- list.files('C:/master/mstudy/data/AWS/', pattern = '-ping.csv', full.names = TRUE)

Dnorm <- params('norm')
Ddiff <- params('diff')
Dnorm_comp <- princomp(scale(Dnorm))$scores[,0:3]
Ddiff_comp <- princomp(scale(Ddiff))$scores[,0:3]

#summary(princomp(scale(Dnorm)))
#princomp(scale(Dnorm))$loadings
#summary(princomp(scale(Ddiff)))
#princomp(scale(Ddiff))$loadings


