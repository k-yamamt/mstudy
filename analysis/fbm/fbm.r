library('bigmemory')

for (filename in list.files('C:/master/mstudy/data/aws.amazon.com/', pattern = '-ping.csv', full.names = TRUE)){
  df <- read.csv(file = filename, header = TRUE, sep=',')
  fbm_result <- as_FBM(df$ping)
  
}
FBM()

