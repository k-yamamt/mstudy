wssplot <- function(dataID,dist.method,clustering.method){
  if(dataID == 'norm'){
    D <- Dnorm
  }
  else if(dataID == 'diff'){
    D <- Ddiff
  }
  else if(dataID == 'norm_comp'){
    D <- Dnorm_comp
  }
  else if(dataID == 'diff_comp'){
    D <- Ddiff_comp
  }
  
  wss <- c()
  
  for (k in 1:16){
    if (clustering.method == 'kmeans'){
      wss <- c(wss,kmeans(D, k)$tot.withinss)
    }
    else{
      cluster <- hcut(D, k = k, hc_func = 'hclust', hc_method = clustering.method, hc_metric = dist.method)$cluster
      wss[k] <- 0
      for(i in 1:k){
        onecluster <- D[names(cluster[cluster==i]),]
        nrow <- nrow(onecluster)
        if(!is.null(nrow)){
          center <- apply(onecluster, 2, mean)
          for(e in 1:nrow){
            wss[k] <- wss[k] + dist(rbind(center,onecluster[e,]), method = dist.method) ** 2
          }
        }
      }
    }
  }
  best.check <- 0
  best.k <- 0
  for ( t in 2:15){
    w <- (wss[t]-wss[t-1])/(wss[t+1]-wss[t])
    if(w > best.check){
      best.check <- w
      best.k <- t
    }
  }
  
  if (clustering.method == 'kmeans'){
    filename <- paste(dataID,'-eucl-kmean-sse.pdf',sep = '')
  }
  else{
    filename <- paste(dataID,'-',str_sub(dist.method,1,4),'-',str_sub(clustering.method,1,4),'-sse.pdf',sep = '')
  }
  pdf(filename,width = 7, height = 5)
  plot(1:16, wss, type = 'o', col = 'blue', xlab = 'Num of cluster', ylab = 'SSE')
  segments(best.k, 0, best.k, wss[best.k], col = 'red', lty="dashed", lwd = 1.4)
  dev.off()
}

compPlot <- function(dataID,dist.method,clustering.method,k){
  if(dataID == 'norm'){
    D <- Dnorm
  }
  else if(dataID == 'diff'){
    D <- Ddiff
  }
  else if(dataID == 'norm_comp'){
    D <- Dnorm_comp
  }
  else if(dataID == 'diff_comp'){
    D <- Ddiff_comp
  }
  
  if (clustering.method == 'kmeans'){
    cluster <- kmeans(D, k)
    filename <- paste(dataID,'-eucl-kmean-compscat.pdf',sep = '')
    fviz_cluster(cluster, D, main = '', labelsize = 0)+ theme_minimal()
  }
  else{
    cluster <- hcut(D, k = k, hc_func = 'hclust', hc_method = clustering.method, hc_metric = dist.method)
    filename <- paste(dataID,'-',str_sub(dist.method,1,4),'-',str_sub(clustering.method,1,4),'-compscat.pdf',sep = '')
    fviz_cluster(cluster, main = '', labelsize = 0)+ theme_minimal()
  }
  
}

clustercompare <- function(){
  C <- matrix(nrow = 122,ncol = 40)
  colnames(C) <- c('norm-eucl-ward','norm-manh-ward','norm-canb-ward','norm-eucl-sing','norm-manh-sing',
                   'norm-canb-sing','norm-eucl-cent','norm-manh-cent','norm-canb-cent','norm-kmean',
                   'diff-eucl-ward','diff-manh-ward','diff-canb-ward','diff-eucl-sing','diff-manh-sing',
                   'diff-canb-sing','diff-eucl-cent','diff-manh-cent','diff-canb-cent','diff-kmean',
                   'norm_comp-eucl-ward','norm_comp-manh-ward','norm_comp-canb-ward','norm_comp-eucl-sing','norm_comp-manh-sing',
                   'norm_comp-canb-sing','norm_comp-eucl-cent','norm_comp-manh-cent','norm_comp-canb-cent','norm_comp-kmean',
                   'diff_comp-eucl-ward','diff_comp-manh-ward','diff_comp-canb-ward','diff_comp-eucl-sing','diff_comp-manh-sing',
                   'diff_comp-canb-sing','diff_comp-eucl-cent','diff_comp-manh-cent','diff_comp-canb-cent','diff_comp-kmean')
  rownames(C) <- c("0229_03","0229_07","0229_12","0229_17","0229_20","0301_03","0301_07","0301_12","0301_17","0301_20",
                   "0302_03","0302_07","0302_12","0302_17","0302_20","0303_03","0303_07","0303_12","0303_17","0303_20",
                   "0304_03","0304_07","0304_12","0304_17","0304_20","0305_03","0305_07","0305_12","0305_17","0305_20",
                   "0306_03","0306_07","0306_12",          "0306_20","0307_03","0307_07","0307_12",          "0307_20",
                   "0308_03","0308_07","0308_12","0308_17","0308_20","0309_03","0309_07","0309_12","0309_17","0309_20",
                   "0310_03","0310_07","0310_12","0310_17","0310_20","0311_03","0311_07","0311_12","0311_17","0311_20",
                   "0312_03","0312_07",                    "0312_20","0313_03","0313_07","0313_12",          "0313_20",
                   "0314_03","0314_07","0314_12",          "0314_20","0315_03","0315_07","0315_12",          "0315_20",
                   "0316_03","0316_07","0316_12",          "0316_20","0317_03","0317_07","0317_12","0317_17",
                   "0318_03","0318_07","0318_12","0318_17","0318_20","0319_03","0319_07","0319_12","0319_17","0319_20",
                   "0320_03","0320_07","0320_12",          "0320_20","0321_03","0321_07","0321_12","0321_17","0321_20",
                   "0322_03","0322_07","0322_12",          "0322_20","0323_03","0323_07",          "0323_17",
                   "0324_03","0324_07","0324_12",          "0325_03","0325_07","0325_12",          "0325_17",
                   "0326_03","0326_07","0326_12","0326_17","0327_03","0327_07","0327_12",          "0327_17"           )
  
  klist <- c(4,4,2,11,6,12,5,5,3,3,
             6,6,14,12,15,7,8,8,14,8,
             2,3,2,12,14,10,9,11,15,2,
             3,3,8,8,14,14,10,10,7,9)
  
  kindex <- 1
  for(dataID in c('norm','diff','norm_comp','diff_comp')){
    if(dataID == 'norm'){
      D <- Dnorm
    }
    else if(dataID == 'diff'){
      D <- Ddiff
    }
    else if(dataID == 'norm_comp'){
      D <- Dnorm_comp
    }
    else if(dataID == 'diff_comp'){
      D <- Ddiff_comp
    }
    for(dist in c('euclidean','manhattan','canberra')){
      for(meth in c('ward.D2','single','centroid')){
        cluster <- hcut(D, k = klist[kindex], hc_func = 'hclust', hc_method = meth, hc_metric = dist)$cluster
        kindex <- kindex + 1
        colname <- paste(dataID,'-',str_sub(dist,1,4),'-',str_sub(meth,1,4),sep = '')
        for(rowname in names(cluster)){
          C[rowname,colname] <- cluster[rowname]
        }
      }
    }
    cluster <- kmeans(D, klist[kindex])$cluster
    kindex <- kindex + 1
    colname <- paste(dataID,'-kmean',sep = '')
    for(rowname in names(cluster)){
      C[rowname,colname] <- cluster[rowname]
    }
  }
  
  C
}

heat <- function(id){
  map1 <- matrix(0,nrow = 40,ncol = 40)
  colnames(map1) <- c('norm-eucl-ward-col','norm-manh-ward-col','norm-canb-ward-col','norm-eucl-sing-col','norm-manh-sing-col',
                      'norm-canb-sing-col','norm-eucl-cent-col','norm-manh-cent-col','norm-canb-cent-col','norm-kmean-col',
                      'diff-eucl-ward-col','diff-manh-ward-col','diff-canb-ward-col','diff-eucl-sing-col','diff-manh-sing-col',
                      'diff-canb-sing-col','diff-eucl-cent-col','diff-manh-cent-col','diff-canb-cent-col','diff-kmean-col',
                      'norm_comp-eucl-ward-col','norm_comp-manh-ward-col','norm_comp-canb-ward-col','norm_comp-eucl-sing-col','norm_comp-manh-sing-col',
                      'norm_comp-canb-sing-col','norm_comp-eucl-cent-col','norm_comp-manh-cent-col','norm_comp-canb-cent-col','norm_comp-kmean-col',
                      'diff_comp-eucl-ward-col','diff_comp-manh-ward-col','diff_comp-canb-ward-col','diff_comp-eucl-sing-col','diff_comp-manh-sing-col',
                      'diff_comp-canb-sing-col','diff_comp-eucl-cent-col','diff_comp-manh-cent-col','diff_comp-canb-cent-col','diff_comp-kmean-col')
  
  rownames(map1) <- c('norm-eucl-ward-row','norm-manh-ward-row','norm-canb-ward-row','norm-eucl-sing-row','norm-manh-sing-row',
                      'norm-canb-sing-row','norm-eucl-cent-row','norm-manh-cent-row','norm-canb-cent-row','norm-kmean-row',
                      'diff-eucl-ward-row','diff-manh-ward-row','diff-canb-ward-row','diff-eucl-sing-row','diff-manh-sing-row',
                      'diff-canb-sing-row','diff-eucl-cent-row','diff-manh-cent-row','diff-canb-cent-row','diff-kmean-row',
                      'norm_comp-eucl-ward-row','norm_comp-manh-ward-row','norm_comp-canb-ward-row','norm_comp-eucl-sing-row','norm_comp-manh-sing-row',
                      'norm_comp-canb-sing-row','norm_comp-eucl-cent-row','norm_comp-manh-cent-row','norm_comp-canb-cent-row','norm_comp-kmean-row',
                      'diff_comp-eucl-ward-row','diff_comp-manh-ward-row','diff_comp-canb-ward-row','diff_comp-eucl-sing-row','diff_comp-manh-sing-row',
                      'diff_comp-canb-sing-row','diff_comp-eucl-cent-row','diff_comp-manh-cent-row','diff_comp-canb-cent-row','diff_comp-kmean-row')
  
  map2 <- matrix(0,nrow = 40,ncol = 40)
  colnames(map2) <- c('norm-eucl-ward-col','norm-manh-ward-col','norm-canb-ward-col','norm-eucl-sing-col','norm-manh-sing-col',
                      'norm-canb-sing-col','norm-eucl-cent-col','norm-manh-cent-col','norm-canb-cent-col','norm-kmean-col',
                      'diff-eucl-ward-col','diff-manh-ward-col','diff-canb-ward-col','diff-eucl-sing-col','diff-manh-sing-col',
                      'diff-canb-sing-col','diff-eucl-cent-col','diff-manh-cent-col','diff-canb-cent-col','diff-kmean-col',
                      'norm_comp-eucl-ward-col','norm_comp-manh-ward-col','norm_comp-canb-ward-col','norm_comp-eucl-sing-col','norm_comp-manh-sing-col',
                      'norm_comp-canb-sing-col','norm_comp-eucl-cent-col','norm_comp-manh-cent-col','norm_comp-canb-cent-col','norm_comp-kmean-col',
                      'diff_comp-eucl-ward-col','diff_comp-manh-ward-col','diff_comp-canb-ward-col','diff_comp-eucl-sing-col','diff_comp-manh-sing-col',
                      'diff_comp-canb-sing-col','diff_comp-eucl-cent-col','diff_comp-manh-cent-col','diff_comp-canb-cent-col','diff_comp-kmean-col')
  
  rownames(map2) <- c('norm-eucl-ward-row','norm-manh-ward-row','norm-canb-ward-row','norm-eucl-sing-row','norm-manh-sing-row',
                      'norm-canb-sing-row','norm-eucl-cent-row','norm-manh-cent-row','norm-canb-cent-row','norm-kmean-row',
                      'diff-eucl-ward-row','diff-manh-ward-row','diff-canb-ward-row','diff-eucl-sing-row','diff-manh-sing-row',
                      'diff-canb-sing-row','diff-eucl-cent-row','diff-manh-cent-row','diff-canb-cent-row','diff-kmean-row',
                      'norm_comp-eucl-ward-row','norm_comp-manh-ward-row','norm_comp-canb-ward-row','norm_comp-eucl-sing-row','norm_comp-manh-sing-row',
                      'norm_comp-canb-sing-row','norm_comp-eucl-cent-row','norm_comp-manh-cent-row','norm_comp-canb-cent-row','norm_comp-kmean-row',
                      'diff_comp-eucl-ward-row','diff_comp-manh-ward-row','diff_comp-canb-ward-row','diff_comp-eucl-sing-row','diff_comp-manh-sing-row',
                      'diff_comp-canb-sing-row','diff_comp-eucl-cent-row','diff_comp-manh-cent-row','diff_comp-canb-cent-row','diff_comp-kmean-row')
  
  
  Clustcol <- colnames(Clust)
  Clustrow <- rownames(Clust)
  
  for (col1 in 1:(length(Clustcol)-1)){
    for (col2 in (col1+1):length(Clustcol)){
      for(row1 in 1:(length(Clustrow)-1)){
        for(row2 in (row1+1):length(Clustrow)){
          if(!is.na(Clust[Clustrow[row1],Clustcol[col1]])
             && !is.na(Clust[Clustrow[row1],Clustcol[col2]])
             && !is.na(Clust[Clustrow[row2],Clustcol[col1]])
             && !is.na(Clust[Clustrow[row2],Clustcol[col2]])){
            if(Clust[Clustrow[row1],Clustcol[col1]]==Clust[Clustrow[row2],Clustcol[col1]]
               && Clust[Clustrow[row1],Clustcol[col2]]==Clust[Clustrow[row2],Clustcol[col2]]){
              map1[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] <-
                map1[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] + 1
            }
            else if(Clust[Clustrow[row1],Clustcol[col1]]==Clust[Clustrow[row2],Clustcol[col1]]
                    || Clust[Clustrow[row1],Clustcol[col2]]==Clust[Clustrow[row2],Clustcol[col2]]){
              map2[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] <-
                map2[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] + 1
            }
          }
        }
      }
      if(str_sub(Clustcol[col1],1,4) == str_sub(Clustcol[col2],1,4)){
        map1[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] <-
          map1[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] / 7260
        map2[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] <-
          map2[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] /7260
      }
      else{
        map1[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] <-
          map1[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] / 7140
        map2[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] <-
          map2[paste(Clustcol[col1],'-row',sep = ''),paste(Clustcol[col2],'-col',sep = '')] /7140
      }
      
    }
  }
  
  if(id == 'map1'){
    map1
  }
  else if(id == 'map2'){
    map2
  }
}

#Clust <- clustercompare()

#map1 <- heat('map1')
#map2 <- heat('map2')