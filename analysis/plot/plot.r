library('stringr')
for (filename in list.files('C:/master/mstudy/data/aws.amazon.com/', pattern = '-ping.csv', full.names = TRUE)){
  df <- read.csv(file = filename, header = TRUE, sep=',')
  
  pdf(paste(str_sub(filename,-24,-9),'plot.pdf',sep=''))
  if (max(df$ping) > 135){
    plot(df$ping, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',ylim = c(0,max(df$ping)+10))
  }else{
    plot(df$ping, type='l', col='blue', xlab = 't', ylab = 'Delay[ms]',ylim = c(0,140))
  }
  dev.off()
}