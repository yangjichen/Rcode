library(stringr)
datapath = commandArgs(T)  
#setwd('/Users/yangjichen/Desktop/6.7/kline')

Cor_kline = function(time_inter=900){
  pair = names(summary(trades$pair))
  #避免这种情况出现 
  if (pair[length(pair)] == "(Other)"){
    pair = pair[-length(pair)]
  } 
  
  #最后生成矩阵的大小
  size = ceiling((trades$timestamp[length(trades$timestamp)] - trades$timestamp[1])/900)
  return_matrix = matrix(data = NA,nrow = size, ncol = 1)
  #print(size)
  
  for (i in pair){
    Exchange = trades[trades$pair == i, ]
    #所有时刻从0开始 
    Exchange$timestamp = Exchange$timestamp-Exchange$timestamp[1]
    #暂取第一个时间点为900秒
    time_indicator = time_inter
    end_time = Exchange$timestamp[length(Exchange$timestamp)]
    #暂时取时间间隔为900秒
    time_interval = time_inter
    #close列表第一个元素
    close = c(Exchange$close[1])
    #可以分成多少个时间段
    num_interval = ceiling(end_time/time_interval)
    
    for (j in 2:length(Exchange$timestamp)){
      true_index = ((Exchange$timestamp[j] > time_indicator) & (Exchange$timestamp[j-1] <= time_indicator))
      if (true_index){
        close = append(close, Exchange$close[j-1])
        #剩余需要统计的时间段个数
        num_interval = num_interval - 1
        if (num_interval>1) {time_indicator = time_indicator+time_interval} else{
          close  = append (close, Exchange$close[length(Exchange$close)])
          #return(close)
        }
      }
    } 
    interval_return = (close[-1]-close[-length(close)])/close[-length(close)]
    #如果长度比正常少1个，那么重复最后一个元素
    if(length(interval_return) != size){interval_return = append(interval_return,interval_return[length(interval_return)])}
    return_matrix = cbind(return_matrix,interval_return)
  }
  return_matrix = return_matrix[,-1]
  colnames(return_matrix) = pair
  #pairs(return_matrix)
  cor_matrix = cor(return_matrix)
  cor_matrix
  fileeeename = paste(str_extract_all(file,"[0-9]+[0-9]"),'.csv',sep = '')
  write.csv(cor_matrix,file = fileeeename)
}





path = datapath[1]
files <- list.files(path=path, pattern="*.csv")
for (file in files){
  trades = read.csv(file = file, header = T, sep = ',')
  try(Cor_kline(time_inter=900))
}





