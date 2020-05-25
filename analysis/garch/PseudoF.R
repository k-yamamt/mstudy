getData <- function(dataID){
  if(dataID == 'norm'){
    Dnorm
  }
  else if(dataID == 'diff'){
    Ddiff
  }
  else if(dataID == 'norm_comp'){
    Dnorm_comp
  }
  else if(dataID == 'diff_comp'){
    Ddiff_comp
  }
  else{
    cat('Error getData')
    exit()
  }
}

getMedoid <- function(C){
  n <- nrow(C)
  
  if(is.null(n)){
    C
  }
  else{
    distance.min <- 100000
    medoid <- NULL
    
    for (i in 1:n){
      sum.distance <- 0
      for (j in (1:n)[-i]){
        sum.distance <- sum.distance + dist(rbind(C[i,],C[j,]),method = 'euclidean')
      }
      
      if(sum.distance < distance.min){
        medoid <- C[i,]
        distance.min <- sum.distance
      }
    }
    
    if(is.null(medoid)){
      cat('Error getMedoid')
      exit()
    }
    else{
      medoid
    }
    
  }
}

medoidMatrix <- function(k, D, cluster){
  medoid <- matrix(nrow = k+1, ncol = ncol(D))
  rownames(medoid) <- c(1:k,'all')
  colnames(medoid) <- colnames(D)
  
  for (i in 1:k){
    medoid[i,] <- getMedoid(D[names(cluster[cluster==i]),])
  }
  medoid['all',] <- getMedoid(D)
  
  medoid
}

PseudoF <- function(dataID){
  D <- getData(dataID)
  
  CH <- c()
  for (k in 2:15){
    cluster <- hcut(D, k = k, hc_func = 'hclust', hc_method = 'ward.D2', hc_metric = 'euclidean')$cluster
    
    medoid <- medoidMatrix(k, D, cluster)
    
    within.var <- 0
    for(i in 1:k){
      C <- D[names(cluster[cluster==i]),]
      if(!is.null(nrow(C))){
        for(j in 1:nrow(C)){
          within.var <- within.var + dist(rbind(C[j,],medoid[i,]), method = 'euclidean')**2
        }
      }
    }
    within.var <- within.var / (nrow(D)-k)
  
    without.var <- 0
    for(i in 1:k){
      without.var <- without.var + length(cluster[cluster==i]) * dist(rbind(medoid[i,],medoid['all',]), method = 'euclidean')**2
    }
    without.var <- without.var / (k-1)
    
    CH[k-1] <- without.var / within.var
  }
  
  pdf(file = paste(dataID, '-PseudoF.pdf', sep = ''), width = 7, height = 5)
  plot(2:15, CH, type = 'o', col = 'blue', xlab = 'Num of cluster', ylab = 'PseudoF')
  dev.off()
}

PseudoFwithMean <- function(dataID){
  D <- getData(dataID)
  
  CH <- c()
  for (k in 2:15){
    cluster <- hcut(D, k = k, hc_func = 'hclust', hc_method = 'ward.D2', hc_metric = 'euclidean')$cluster
    
    medoid <- medoidMatrix(k, D, cluster)
    
    within.var <- 0
    for(i in 1:k){
      C <- D[names(cluster[cluster==i]),]
      if(!is.null(nrow(C))){
        for(j in 1:nrow(C)){
          within.var <- within.var + dist(rbind(C[j,],medoid[i,]), method = 'euclidean')**2
        }
      }
    }
    within.var <- 1 + within.var
    
    without.var <- 0
    for(i in 1:k){
      for(j in 1:k){
      without.var <- without.var + length(cluster[cluster==i]) * dist(rbind(medoid[i,],medoid[j,]), method = 'euclidean')**2
      }
    }
    without.var <- without.var / k
    
    CH[k-1] <- without.var / within.var
  }
  
  pdf(file = paste(dataID, '-PseudoFwithMean.pdf', sep = ''), width = 7, height = 5)
  plot(2:15, CH, type = 'o', col = 'blue', xlab = 'Num of cluster', ylab = 'PseudoF with Mean')
  dev.off()
}

PseudoFwithMin <- function(dataID){
  D <- getData(dataID)
  
  CH <- c()
  for (k in 2:15){
    cluster <- hcut(D, k = k, hc_func = 'hclust', hc_method = 'ward.D2', hc_metric = 'euclidean')$cluster
    
    medoid <- medoidMatrix(k, D, cluster)
    
    within.var <- 0
    for(i in 1:k){
      C <- D[names(cluster[cluster==i]),]
      if(!is.null(nrow(C))){
        for(j in 1:nrow(C)){
          within.var <- within.var + dist(rbind(C[j,],medoid[i,]), method = 'euclidean')**2
        }
      }
    }
    within.var <- 1 + within.var
    
    without.var <- 0
    for(i in 1:k){
      distance.min <- 100000
      for(j in (1:k)[-i]){
        distance <- dist(rbind(medoid[i,],medoid[j,]), method = 'euclidean')
        if(distance < distance.min){
          distance.min <- distance
        }
      }
      
      if(distance.min != 100000){
        without.var <- without.var + length(cluster[cluster==i]) * distance.min**2
      }
      else{
        cat('Error PsuedoFwithMin')
        exit()
      }
      
    }
    
    CH[k-1] <- without.var / within.var
  }
  
  pdf(file = paste(dataID, '-PseudoFwithMin.pdf', sep = ''), width = 7, height = 5)
  plot(2:15, CH, type = 'o', col = 'blue', xlab = 'number of clusters', ylab = 'PseudoF with Min')
  dev.off()
}

CompPlot <- function(dataID,k){
  D <- getData(dataID)
  
  cluster <- hcut(D, k = k, hc_func = 'hclust', hc_method = 'ward.D2', hc_metric = 'euclidean')
  fviz_cluster(cluster, main = '', labelsize = 0)+ theme_minimal()
}

for ( data in list('norm', 'diff', 'norm_comp', 'diff_comp')){
  PseudoF(data)
  PseudoFwithMean(data)
  PseudoFwithMin(data)
}

CompPlot('diff_comp',15)
