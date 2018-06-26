library(lubridate)
Sys.setenv(TZ="Asia/Shanghai")
source('conf.R')
time_inter = nmins

return_fun = function(price, time = 5){
  if (time == 1){
    return_list = log(price[-1] / price[-length(price)])
    return(return_list)
  }else if(time > 1){
    return_list = numeric()
    interval = ceiling(length(price)/time)
    #初始化k
    k = time
    n = 1
    for (i in 1:length(price)){
      #终止条件
      if(n == interval){return(return_list)}
      
      if(i == k){
        return = log(price[k]/price[k-1])
        return_list = append(return_list,return)
        k = k + time
        n = n + 1
      }
    }
  }
}



Cormatrix = function(rootpath,date,exchange,curr,nmins,ndays){
  if (curr =='btc'){
    pairs = c('btc_eth','btc_ltc','btc_etc','btc_bch','btc_eos','btc_dash','btc_xmr','btc_zec')
  }else if(curr =='usd'){
    pairs = c('usd_btc','usd_eth','usd_ltc','usd_etc','usd_bch','usd_eos','usd_dash','usd_xmr','usd_zec')
  }
  
  if (ndays == 1){
    
    len = ceiling(1440/as.numeric(time_inter))-1
    logreturn_matrix = matrix(NA,nrow = len,ncol =1)
    path_file = paste(path,filename,sep='/')
    
    trades = read.csv(file = path_file, header = T, sep = ',')
    for (i in 1:length(pairs)){
      indx = (trades$pair == pairs[i])
      if (sum(indx) == 1440){
        pair_data = trades[indx,]
        mid_price = (pair_data$bid+pair_data$ask)/2
        log_return = return_fun(mid_price, time = time_inter)
        #outlier
        lindex = log_return < quantile(log_return,c(0.01,0.99))[1]
        hindex = log_return > quantile(log_return,c(0.01,0.99))[2]
        log_return[lindex] = quantile(log_return,c(0.01,0.99))[1]
        log_return[hindex] = quantile(log_return,c(0.01,0.99))[2]
        
        logreturn_matrix = cbind(logreturn_matrix,log_return)
      } else{
        log_return = rep(NA,len)
        logreturn_matrix = cbind(logreturn_matrix,log_return)
      }
    }
    
    logreturn_matrix = logreturn_matrix[,-1]
    colnames(logreturn_matrix) = pairs
    cor_matrix = cor(logreturn_matrix)
    cor_matrix
  }else{
    path_file = paste(path,filename,sep='/')
    len = ceiling(1440*ndays/as.numeric(time_inter))-1
    logreturn_matrix = matrix(NA,nrow = len,ncol =1)
    trades = read.csv(file = path_file, header = T, sep = ',')
    for(i in 1:(ndays-1)){
  
      ddate = ymd(date)+1
      ddate = gsub('-','',ddate)
      newpath = paste(rootpath,ddate,exchange,sep = '')
      
      newfilename = paste(curr,ddate,sep='_')
      newfilename = paste(newfilename,'.csv',sep ='')
      new_path_file = paste(newpath,newfilename,sep='/')
      new_trades = read.csv(file = new_path_file, header = T, sep = ',')
      trades = rbind(trades,new_trades)
    }
    for (i in 1:length(pairs)){
      indx = (trades$pair == pairs[i])
      if (sum(indx) == 1440*ndays){
        pair_data = trades[indx,]
        mid_price = (pair_data$bid+pair_data$ask)/2
        log_return = return_fun(mid_price, time = time_inter)
        #outlier
        lindex = log_return < quantile(log_return,c(0.01,0.99))[1]
        hindex = log_return > quantile(log_return,c(0.01,0.99))[2]
        log_return[lindex] = quantile(log_return,c(0.01,0.99))[1]
        log_return[hindex] = quantile(log_return,c(0.01,0.99))[2]
        
        logreturn_matrix = cbind(logreturn_matrix,log_return)
      } else{
        log_return = rep(NA,len)
        logreturn_matrix = cbind(logreturn_matrix,log_return)
        
      }
    }
    logreturn_matrix = logreturn_matrix[,-1]
    colnames(logreturn_matrix) = pairs
    cor_matrix = cor(logreturn_matrix)
    cor_matrix
  }
}



Cormatrix(rootpath,date,exchange,curr,nmins,ndays)
