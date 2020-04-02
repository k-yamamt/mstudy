sinetclustering <- function(dataID,dist.method,clustering.method,k,type){
  if(dataID == 'norm'){
    D <- Sinet_Dnorm
  }
  else if(dataID == 'diff'){
    D <- Sinet_Ddiff
  }
  else if(dataID == 'norm_comp'){
    D <- Sinet_Dnorm_comp
  }
  else if(dataID == 'diff_comp'){
    D <- Sinet_Ddiff_comp
  }
  
  if (clustering.method == 'kmeans'){
    cluster <- kmeans(D, k)$cluster
    filename <- paste(dataID,'-eucl-kmean-',k,'-',type,'-sinet.pdf',sep = '')
  }
  else{
    cluster <- hcut(D, k = k, hc_func = 'hclust', hc_method = clustering.method, hc_metric = dist.method)$cluster
    filename <- paste(dataID,'-',str_sub(dist.method,1,4),'-',str_sub(clustering.method,1,4),'-',k,'-',type,'-sinet.pdf',sep = '')
  }
  
  if (type == 'timezone'){
    data <- matrix(0,nrow = 5, ncol = k)
    rownames(data) <- c('03','07','12','17','20')
    colnames(data) <- 1:k
    for (i in 1:length(cluster)){
      data[str_sub(names(cluster[i]),-2),cluster[i]] <- data[str_sub(names(cluster[i]),-2),cluster[i]] + 1
    }
    num <- colSums(data)
    for (i in 1:length(num)){
      data[,i] <- data[,i]/num[i]
    }
    
    pdf(filename, height = 5, width =7)
    par(mar=c(4,4,4,7))
    bar <- barplot(
      data,
      names.arg = 1:k,
      col = c('purple','green','red','darkorange','blue'),
      xlab = 'cluster index',
      ylab = 'proportion',
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = c('20:00-21:00','17:00-18:00','12:00-13:00','7:00-8:00','3:00-4:00'),
           pch = 15, col = c('blue','darkorange','red','green','purple'),title = 'time zone')
    for (i in 1:length(num)){
      text(num[i],x = bar[i], y = 1.05)
    }
    text('sum', x = (bar[1]+bar[length(bar)])/2, y = 1.13)
    
    dev.off()
  }
  else if(type == 'day'){
    data <- matrix(0,nrow = 7, ncol = k)
    rownames(data) <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
    colnames(data) <- 1:k
    for (i in 1:length(cluster)){
      day <- str_sub(names(cluster[i]),start = 3, end = 4)
      x <- as.integer(day)%%7
      if(str_sub(names(cluster[i]),start = 1, end = 2) == '12'){
        if(x == 1){
          id <- 'Sun'
        }
        else if(x == 2){
          id <- 'Mon'
        }
        else if(x == 3){
          id <- 'Tue'
        }
      }
      else{
        if(x == 1){
          id <- 'Wed'
        }
        else if(x == 2){
          id <- 'Thu'
        }
        else if(x == 3){
          id <- 'Fri'
        }
        else if(x == 4){
          id <- 'Sat'
        }
        else if(x == 5){
          id <- 'Sun'
        }
        else if(x == 6){
          id <- 'Mon'
        }
        else if(x == 0){
          id <- 'Tue'
        }
      }
      data[id,cluster[i]] <- data[id,cluster[i]] + 1
    }
    num <- colSums(data)
    for (i in 1:length(num)){
      data[,i] <- data[,i]/num[i]
    }
    
    pdf(filename,height = 5, width = 7)
    par(mar=c(4,4,4,7))
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
    text('sum', x = (bar[1]+bar[length(bar)])/2, y = 1.13)
    
    dev.off()
  }
  else if(type == 'timezone-day'){
    data <- matrix(0,nrow = k, ncol = 35)
    rownames(data) <- 1:k
    colnames(data) <- c('Mon-03','Mon-07','Mon-12','Mon-17','Mon-20',
                        'Tue-03','Tue-07','Tue-12','Tue-17','Tue-20',
                        'Wed-03','Wed-07','Wed-12','Wed-17','Wed-20',
                        'Thu-03','Thu-07','Thu-12','Thu-17','Thu-20',
                        'Fri-03','Fri-07','Fri-12','Fri-17','Fri-20',
                        'Sat-03','Sat-07','Sat-12','Sat-17','Sat-20',
                        'Sun-03','Sun-07','Sun-12','Sun-17','Sun-20')
    
    for (i in 1:length(cluster)){
      day <- str_sub(names(cluster[i]),start = 3, end = 4)
      x <- as.integer(day)%%7
      if(str_sub(names(cluster[i]),start = 1, end = 2) == '12'){
        if(x == 1){
          id <- 'Sun'
        }
        else if(x == 2){
          id <- 'Mon'
        }
        else if(x == 3){
          id <- 'Tue'
        }
      }
      else{
        if(x == 1){
          id <- 'Wed'
        }
        else if(x == 2){
          id <- 'Thu'
        }
        else if(x == 3){
          id <- 'Fri'
        }
        else if(x == 4){
          id <- 'Sat'
        }
        else if(x == 5){
          id <- 'Sun'
        }
        else if(x == 6){
          id <- 'Mon'
        }
        else if(x == 0){
          id <- 'Tue'
        }
      }
      data[cluster[i],paste(id,'-',str_sub(names(cluster[i]),-2),sep = '')] <- data[cluster[i],paste(id,'-',str_sub(names(cluster[i]),-2),sep = '')] + 1
    }
    num <- colSums(data)
    for (i in 1:length(num)){
      data[,i] <- data[,i]/num[i]
    }
    
    pdf(filename, height = 5, width =7, family = 'Japan1GothicBBB')
    par(mar=c(4,4,4,7))
    color <- colorRampPalette(brewer.pal(11,"Spectral"))(k)
    bar <- barplot(
      data,
      names.arg = 1:35,
      xlab = 'time zone and day of week',
      ylab = 'proportion',
      xaxt="n",
      col = color
    )
    par(xpd=T)
    legend(par()$usr[2] + 0.1, par()$usr[4], legend = rev(1:k),
           pch = 15, col = rev(color), title = 'cluster index')
    
    for (i in 1:length(num)){
      text(num[i],x = bar[i], y = 1.05)
    }
    text('sum', x = (bar[1]+bar[length(bar)])/2, y = 1.13)
    barHalfWidth = (bar[2] - bar[1])/2
    day <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
    for(i in 0:6){
      segments(bar[i*5+1]-barHalfWidth,0,bar[i*5+1],-0.04)
      segments(bar[i*5+1],-0.04,bar[i*5+5],-0.04)
      segments(bar[i*5+5],-0.04,bar[i*5+5]+barHalfWidth,0)
      text(day[i+1], x = bar[i*5+3], y = -0.06)
      text('譎る俣蟶ｯ縺ｯ蟾ｦ縺九ｉ鬆?縺ｫ 3:00-4:00,7:00-8:00,12:00-13:00,17:00-18:00,20:00-21:00',x = bar[1] - barHalfWidth*10, y = -0.15, pos = 4)
    }
    
    dev.off()
  }
}