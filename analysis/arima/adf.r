library(tseries)

getADFList <- function(distination, process){
  if(distination == 'AWS'){
    files <- list.files('C:/master/mstudy/data/AWS/', pattern = '-ping.csv', full.names = TRUE)
  }
  else if(distination == 'SINET'){
    files <- list.files('C:/master/mstudy/data/SINET/csv/15s/', full.names = TRUE)
  }
  else{
    print('error distination')
    exit()
  }
  
  ADFList <- c()
  
  for (filename in files){
    y <- read.csv(file = filename, header = TRUE, sep=',')$ping
    if (process == 'diff'){
      y <- diff(y)
    }
    else if (process != 'norm'){
      print('error process')
      exit()
    }

    ADFList <- c(ADFList,adf.test(y)$p.value)
  }

  ADFList
}

AWS_norm <- getADFList('AWS','norm')
AWS_diff <- getADFList('AWS','diff')
SINET_norm <- getADFList('SINET','norm')
SINET_diff <- getADFList('SINET','diff')

x <- 0
for (a in SINET_norm){
  if (a > 0.05)
    x <- x + 1
}
x
