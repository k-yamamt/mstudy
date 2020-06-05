sinetparams <- function(id){
  D <- matrix(nrow = 0,ncol = 10)
  colnames(D) <- c('c','a1','a2','b1','b2','omega','alpha1','alpha2','beta1','beta2')
  date <- NULL
  
  for (filename in sinetfiles){
    df <- read.csv(file = filename, header = TRUE, sep=',')
    
    if(id == 'norm'){
      tryCatch(
        {
          garch_param <- garchFit(formula = ~arma(2,2)+garch(2,2), data = df$ping, trace = FALSE)@fit$coef
          date <- c(date,str_sub(filename, start = -15, end = -9))
          D <- rbind(D,c(garch_param[1],garch_param[2],garch_param[3],garch_param[4],garch_param[5],garch_param[6],garch_param[7],garch_param[8],garch_param[9],garch_param[10]))
        }
        ,error = function(e){
          cat(filename)
        }
      )
    }else if(id == 'diff'){
      tryCatch(
        {
          garch_param <- garchFit(formula = ~arma(2,2)+garch(2,2), data = diff(df$ping), trace = FALSE)@fit$coef
          date <- c(date,str_sub(filename, start = -15, end = -9))
          D <- rbind(D,c(garch_param[1],garch_param[2],garch_param[3],garch_param[4],garch_param[5],garch_param[6],garch_param[7],garch_param[8],garch_param[9],garch_param[10]))
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

sinetfiles <- list.files('C:/master/mstudy/data/SINET/csv/15s/', full.names = TRUE)

Sinet_Dnorm <- sinetparams('norm')
Sinet_Ddiff <- sinetparams('diff')
#Sinet_Dnorm_comp <- princomp(scale(Sinet_Dnorm))$scores[,0:3]
#Sinet_Ddiff_comp <- princomp(scale(Sinet_Ddiff))$scores[,0:4]

#summary(princomp(scale(Sinet_Dnorm)))
#princomp(scale(Sinet_Dnorm))$loadings
#summary(princomp(scale(Sinet_Ddiff)))
#princomp(scale(Sinet_Ddiff))$loadings