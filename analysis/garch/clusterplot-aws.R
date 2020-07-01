clustering <- function(dataID,dist.method,clustering.method,k,type){
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
  else if(dataID == 'norm_scale'){
    D <- Dnorm_scale
  }
  else if(dataID == 'diff_scale'){
    D <- Ddiff_scale
  }
  
  if (clustering.method == 'kmeans'){
    cluster <- kmeans(D, k)$cluster
    filename <- paste(dataID,'-eucl-kmean-',k,'-',type,'.pdf',sep = '')
  }
  else{
    cluster <- hcut(D, k = k, hc_func = 'hclust', hc_method = clustering.method, hc_metric = dist.method)$cluster
    filename <- paste(dataID,'-',str_sub(dist.method,1,4),'-',str_sub(clustering.method,1,4),'-',k,'-',type,'.pdf',sep = '')
  }
  
  if (type == 'timezone'){
    data <- matrix(0,nrow = 5, ncol = k)
    rownames(data) <- c('03','07','12','17','20')
    colnames(data) <- 1:k
    for (i in 1:length(cluster)){
      data[str_sub(names(cluster[i]),-2),cluster[i]] <- data[str_sub(names(cluster[i]),-2),cluster[i]] + 1
    }
    
    pdf(paste('num-',filename,sep = ''), height = 5, width =7)
    par(mar=c(4,4,1,7.5))
    par(mgp=c(2, 0.5, 0.5))
    par(xaxs = "i")
    par(yaxs = "i")
    bar <- barplot(
      data,
      names.arg = 1:k,
      col = c('purple','green','red','darkorange','blue'),
      xlab = 'cluster index',
      ylab = 'number',
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('20:00-21:00','17:00-18:00','12:00-13:00','7:00-8:00','3:00-4:00'),
           pch = 15, col = c('blue','darkorange','red','green','purple'),title = 'time of day')
    
    dev.off()
    
    num <- colSums(data)
    for (i in 1:length(num)){
      data[,i] <- data[,i]/num[i]
    }
    
    pdf(filename, height = 5, width =7)
    par(mar=c(4,4,1.5,7.5))
    par(mgp=c(2, 0.5, 0.5))
    par(xaxs = "i")
    par(yaxs = "i")
    bar <- barplot(
      data,
      names.arg = 1:k,
      col = c('purple','green','red','darkorange','blue'),
      xlab = 'cluster index',
      ylab = 'proportion',
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('20:00-21:00','17:00-18:00','12:00-13:00','7:00-8:00','3:00-4:00'),
           pch = 15, col = c('blue','darkorange','red','green','purple'),title = 'time of day')
    for (i in 1:length(num)){
      text(num[i],x = bar[i], y = 1.05)
    }

    dev.off()
    
  }
  else if(type == 'day'){
    data <- matrix(0,nrow = 7, ncol = k)
    rownames(data) <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
    colnames(data) <- 1:k
    for (i in 1:length(cluster)){
      day <- str_sub(names(cluster[i]),start = 3, end = 4)
      if(day == '29'){
        id <- 'Sat'
      }
      else{
        x <- as.integer(day)%%7
        if(x == 1){
          id <- 'Sun'
        }
        else if(x == 2){
          id <- 'Mon'
        }
        else if(x == 3){
          id <- 'Tue'
        }
        else if(x == 4){
          id <- 'Wed'
        }
        else if(x == 5){
          id <- 'Thu'
        }
        else if(x == 6){
          id <- 'Fri'
        }
        else if(x == 0){
          id <- 'Sat'
        }
      }
      data[id,cluster[i]] <- data[id,cluster[i]] + 1
    }
    
    pdf(paste('num-',filename,sep = ''),height = 5, width = 7)
    par(mar=c(4,4,1,7.5))
    par(mgp=c(2, 0.5, 0.5))
    par(xaxs = "i")
    par(yaxs = "i")
    bar <- barplot(
      data,
      names.arg = 1:k,
      col = c('gray','firebrick3','darkturquoise','green','goldenrod3','blue','red'),
      xlab = 'cluster index',
      ylab = 'number'
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('Sun','Sat','Fri','Thu','Wed','Tue','Mon'),
           pch = 15, col = c('red','blue','goldenrod3','green','darkturquoise','firebrick3','gray'), title = 'day of week')
    
    dev.off()
    
    num <- colSums(data)
    for (i in 1:length(num)){
      data[,i] <- data[,i]/num[i]
    }
    
    pdf(filename,height = 5, width = 7)
    par(mar=c(4,4,1.5,7.5))
    par(mgp=c(2, 0.5, 0.5))
    par(xaxs = "i")
    par(yaxs = "i")
    bar <- barplot(
      data,
      names.arg = 1:k,
      col = c('gray','firebrick3','darkturquoise','green','goldenrod3','blue','red'),
      xlab = 'cluster index',
      ylab = 'proportion'
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('Sun','Sat','Fri','Thu','Wed','Tue','Mon'),
           pch = 15, col = c('red','blue','goldenrod3','green','darkturquoise','firebrick3','gray'), title = 'day of week')
    for (i in 1:length(num)){
      text(num[i],x = bar[i], y = 1.05)
    }
    
    dev.off()
  }
  else if(type == 'timezone-day'){
    data <- matrix(0,nrow = k, ncol = 41)
    rownames(data) <- 1:k
    colnames(data) <- c('Mon-03','Mon-07','Mon-12','Mon-17','Mon-20','dammy1',
                        'Tue-03','Tue-07','Tue-12','Tue-17','Tue-20','dammy2',
                        'Wed-03','Wed-07','Wed-12','Wed-17','Wed-20','dammy3',
                        'Thu-03','Thu-07','Thu-12','Thu-17','Thu-20','dammy4',
                        'Fri-03','Fri-07','Fri-12','Fri-17','Fri-20','dammy5',
                        'Sat-03','Sat-07','Sat-12','Sat-17','Sat-20','dammy6',
                        'Sun-03','Sun-07','Sun-12','Sun-17','Sun-20')
    
    for (i in 1:length(cluster)){
      day <- str_sub(names(cluster[i]),start = 3, end = 4)
      if(day == '29'){
        id <- 'Sat'
      }
      else{
        x <- as.integer(day)%%7
        if(x == 1){
          id <- 'Sun'
        }
        else if(x == 2){
          id <- 'Mon'
        }
        else if(x == 3){
          id <- 'Tue'
        }
        else if(x == 4){
          id <- 'Wed'
        }
        else if(x == 5){
          id <- 'Thu'
        }
        else if(x == 6){
          id <- 'Fri'
        }
        else if(x == 0){
          id <- 'Sat'
        }
      }
      data[cluster[i],paste(id,'-',str_sub(names(cluster[i]),-2),sep = '')] <- data[cluster[i],paste(id,'-',str_sub(names(cluster[i]),-2),sep = '')] + 1
    }
    num <- colSums(data)
    for (i in 1:length(num)){
      data[,i] <- data[,i]/num[i]
    }
    
    pdf(filename, height = 5, width =10, family = 'Japan1GothicBBB')
    par(mar=c(4,4,2,6))
    color <- colorRampPalette(brewer.pal(11,"Spectral"))(k)
    bar <- barplot(
      data,
      names.arg = 1:41,
      xlab = 'time of day and day of week',
      ylab = 'proportion',
      xaxt="n",
      col = color
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = rev(1:k),
           pch = 15, col = rev(color), title = 'cluster index')
    
    for (i in 1:length(num)){
      if(num[i] != 0){
        text(num[i],x = bar[i], y = 1.05)
      }
    }
    barHalfWidth = (bar[2] - bar[1])/2
    day <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
    for(i in 0:6){
      segments(bar[i*6+1]-barHalfWidth,0,bar[i*6+1],-0.04)
      segments(bar[i*6+1],-0.04,bar[i*6+5],-0.04)
      segments(bar[i*6+5],-0.04,bar[i*6+5]+barHalfWidth,0)
      text(day[i+1], x = bar[i*6+3], y = -0.06)
      text('time of day 3:00-4:00,7:00-8:00,12:00-13:00,17:00-18:00,20:00-21:00',x = bar[1] , y = -0.15, pos = 4)
    }
    
    dev.off()
  }
  cluster
}

#data ('norm','diff','norm_comp','diff_comp')
#distance  c('euclidean','manhattan','canberra','maximum','binary')
#method  c('kmeans','single','centroid','ward.D2','complete','average','mcquitty')
#k  c(6,7,9,12,15)
trend  <- c('timezone','day','timezone-day')
nc <- clustering('norm_comp','euclidean','ward.D2',8,'timezone-day')
x <- 0
for (i in 1:121){
  if(n[i]==nc[i]){
    x<- x+1
  }
}
for(t in trend){
  clustering('diff_scale','euclidean','ward.D2',3,t)
}

