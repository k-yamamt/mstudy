library('SuperGauss')

addr = 'C:/master/mstudy/data/aws.amazon.com/'

for (filename in list.files(addr, pattern = '-ping.csv', full.names = TRUE)){
  df <- read.csv(file = filename, header = TRUE, sep=',')

}


