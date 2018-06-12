#setwd('/Users/yangjichen/Desktop/6.7/20180605')
data_name = commandArgs(T)  

fifteen_return = function(close){
  k = ceiling(length(close)/15)
  fif_return = numeric()
  if(k <= 1) {
    omg =(close[length(close)]-close[1])/close[1]
    return(omg)
  } else {
    for(numm in 1:(k-1)){
      fif_return = append(fif_return, (close[15*numm]-close[15*(numm-1)+1])/close[15*(numm-1)+1])
    }
    fif_return = append(fif_return, (close[length(close)]-close[15*(k-1)+1])/close[15*(k-1)+1])
    return(fif_return)
  }
}






if (data_name[3]=='ba'){
  #trades = read.csv(file = 'ba_kline1min_20180605.csv', header = T, sep = ',')
  #depth = read.csv(file = 'ba_depth_20180605.csv', header = T, sep = ',')
  trades = read.csv(file = data_name[1], header = T, sep = ',')
  depth = read.csv(file = data_name[2], header = T, sep = ',')
  depth$pair = gsub('depth','kline1min',depth$pair)
  
  pair = names(summary(trades$pair))
  
  if (pair[length(pair)] == "(Other)"){
    pair = pair[-length(pair)]
  } 
  
  output_matrix = matrix(ncol = 6)     #need to change ncol 
  for (i in pair){
    Exchange = trades[trades$pair == i, ]
    timeline = dim(Exchange)[1]
    
    volume = numeric()              # 1 min volume array
    high = numeric()
    low = numeric()
    close = numeric()
    
    #This for loop can be modified
    for (j in 1:timeline){
      if (Exchange[j,]$isClose == 'True'){
        volume = append(volume, Exchange[j,]$volume )
        high = append(high, Exchange[j,]$high )
        low = append(low, Exchange[j,]$low)
        close = append(close, Exchange[j,]$close)
      }
    }
    
    one_min_return = (close[-1]-close[-length(close)]) / close[-length(close)]
    fif_min_return = fifteen_return(close)
    
    if(length(high) > 0){
      total_volume = sum(volume)
      total_high = max(high)
      total_low = min(low)
    }else{
      total_volume = sum(volume)
      total_high = NA
      total_low = NA
    }
    # Use Standard Deviation*10000 as Volatility
    volatility_one_min = round(sd(one_min_return)*10000,2)
    volatility_fif_min = round(sd(fif_min_return)*10000,2)
    
    
    #Some value in table2
    DExchange = depth[depth$pair == i, ]
    spread = DExchange$askp_1 - DExchange$bidp_1
    mid = (DExchange$askp_1 + DExchange$bidp_1)/2
    eff_spread = spread/mid
    eff_spread_mean = round(mean(eff_spread)*10000,2)
    
   #output_matrix 
    
    output_matrix = rbind(output_matrix,c(total_volume, total_high, total_low,
                                          volatility_one_min,volatility_fif_min,eff_spread_mean))
  }
  # Row name: 'exch' 'curr' 'coin'
  exch = c()
  curr = c()
  coin = c()
  splittt = strsplit(pair,split = "_")
  for (i in 1:length(pair)){
    exch = append(exch, splittt[[i]][2])
    curr = append(curr, splittt[[i]][3])
    coin = append(coin, splittt[[i]][4])
  }
  
  output_matrix = cbind(exch,curr,coin,output_matrix[-1, ])
  
  colnames(output_matrix) = c('Exchange','Currency','Coin' ,'Total_volume','Total_high','Total_low',
                              'Volatility_1min','Volatility_15min','Effctive_spread')
  
}else {
  
  
  
  
  
  
  #trades = read.csv(file = 'okexf_kline1min_20180605.csv', header = T, sep = ',')
  trades = read.csv(file = data_name[1], header = T, sep = ',')
  depth = read.csv(file = data_name[2], header = T, sep = ',')
  depth$pair = gsub('depth','kline1min',depth$pair)
  pair = names(summary(trades$pair))
  
  if (pair[length(pair)] == "(Other)"){
    pair = pair[-length(pair)]
  } 
  
  output_matrix = matrix(ncol = 6) 
  for (i in pair){
    Exchange = trades[trades$pair == i, ]
    timeline = dim(Exchange)[1]
    
    #Add True_False index to the data frame
    true_index = (Exchange$volume[-1]-Exchange$volume[-timeline]) < 0
    true_index = append(true_index, FALSE)
    Exchange$isClose = true_index
    
    volume = numeric()              # 1 min volume array
    high = numeric()
    low = numeric()
    close = numeric()
    
    for (j in 1:timeline){
      if (Exchange[j,]$isClose == TRUE){
        volume = append(volume, Exchange[j,]$volume )
        high = append(high, Exchange[j,]$high )
        low = append(low, Exchange[j,]$low)
        close = append(close, Exchange[j,]$close)
      }
    }
    
    one_min_return = (close[-1]-close[-length(close)]) / close[-length(close)]
    fif_min_return = fifteen_return(close)
    
    if(length(high) > 0){
      total_volume = sum(volume)
      total_high = max(high)
      total_low = min(low)
    }else{
      total_volume = sum(volume)
      total_high = NA
      total_low = NA
    }
    # Use Standard Deviation*10000 as Volatility
    volatility_one_min = round(sd(one_min_return)*10000,2)
    volatility_fif_min = round(sd(fif_min_return)*10000,2)
    
    
    
    #Some value in table2
    DExchange = depth[depth$pair == i ,]
    spread = DExchange$askp_1 - DExchange$bidp_1
    mid = (DExchange$askp_1 + DExchange$bidp_1)/2
    eff_spread = spread/mid
    eff_spread_mean = round(mean(eff_spread)*10000,2)
    
    output_matrix = rbind(output_matrix,c(total_volume, total_high, total_low, 
                                          volatility_one_min,volatility_fif_min,eff_spread_mean))

  }
  
  # Row name: 'exch' 'curr' 'coin'
  exch = c()
  curr = c()
  coin = c()
  splittt = strsplit(pair,split = "_")
  for (i in 1:length(pair)){
    exch = append(exch, splittt[[i]][2])
    curr = append(curr, splittt[[i]][3])
    coin = append(coin, splittt[[i]][4])
  }
  
  output_matrix = cbind(exch,curr,coin,output_matrix[-1, ])
  colnames(output_matrix) = c('Exchange','Currency','Coin' ,'Total_volume','Total_high','Total_low',
                              'Volatility_1min','Volatility_15min','Effctive_spread')
}
print(output_matrix)
#write.csv(output_matrix,file = 'ba_summary.csv')
