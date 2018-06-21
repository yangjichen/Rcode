#实现功能：读取一个文件夹中的全部csv文件，按照pair与exchange将其拆为划分更细的CSV文件
library(stringr)
#首先保证时区正确
Sys.setenv(TZ="Asia/Shanghai")
#setwd('/Users/yangjichen/Desktop/6.7/20180605')
#file = 'hbp_depth_20180605.csv'

#命令行中需要输入的参数
today_time = commandArgs(T) 
#today_time = 20180605



depth_split = function(time_inter = 60){
  pair = names(summary(trades$pair))
  #防止Other这种情况产生
  if (pair[length(pair)] == "(Other)"){
    pair = pair[-length(pair)]
  } 
  #对应的cst时区时间从0点开始
  cst_time = as.numeric(as.POSIXct(as.character(today_time), format="%Y %m %d"))
  
  
  foldnamesplit = strsplit(file,split = "_")
  fold_name = paste(foldnamesplit[[1]][1],today_time,sep = '_')
  dir.create(fold_name)
  
  for (i in pair){
    #每次循环生成这样一个矩阵，然后保存下来
    split_matrix = matrix(data = NA,nrow = 24*60*60/time_inter, ncol = 1)
    splittt = strsplit(i,split = "_")
    pairrr = paste(splittt[[1]][3],splittt[[1]][4] ,sep = '_' )
    pairrrs = rep(pairrr,24*60*60/time_inter)
    exchangeees = rep(splittt[[1]][2],24*60*60/time_inter)
    time_lines = seq(cst_time+60, cst_time+86400,by = 60)
    minutes_index = seq(1,24*60*60/time_inter)
    
    
    
    Exchange = trades[trades$pair == i, ]
    #所有时刻从0开始 
    starttime = Exchange$timestamp[1]
    Exchange$timestamp = Exchange$timestamp-starttime
    end_time = Exchange$timestamp[length(Exchange$timestamp)]
    #暂取第一个时间点为60秒
    time_indicator = time_inter
    #取时间间隔为60秒
    time_interval = time_inter
    #新的表格中，需要对一下几个变量进行构建
    bid = numeric()
    ask = numeric()
    lasttime = numeric()
    #可以分成多少个时间段
    num_interval = 24*60*60/time_inter
    
    for (j in 2:length(Exchange$timestamp)){
      true_index = ((Exchange$timestamp[j] > time_indicator) & (Exchange$timestamp[j-1] <= time_indicator))
      
      if (true_index){
        bid = append(bid, Exchange$bidp_1[j-1])
        ask = append(ask, Exchange$askp_1[j-1])
        lasttime = append(lasttime, Exchange$timestamp[j-1]+starttime)
        #剩余需要统计的时间段个数
        num_interval = num_interval - 1
  
        if (num_interval>1) {
          time_indicator = time_indicator+time_inter
          #避免indicator更新后还是比当前时间点时间小的Bug，也就是这里验证会不会跳过一分钟
          if (time_indicator < Exchange$timestamp[j]){
            bid = append(bid, Exchange$bidp_1[j-1])
            ask = append(ask, Exchange$askp_1[j-1])
            lasttime = append(lasttime, Exchange$timestamp[j-1]+starttime)
            num_interval = num_interval - 1
            
            if (num_interval>1) {
              time_indicator = time_indicator+time_inter
              next} 

          
          
          } }else{
           bid  = append (bid, Exchange$bidp_1[length(Exchange$bidp_1)])
           ask  = append (ask, Exchange$askp_1[length(Exchange$askp_1)])
           lasttime = append(lasttime, Exchange$timestamp[length(Exchange$timestamp)]+starttime)
        }
    
      }
    }
    
    if (length(bid)!=1440){
      print(paste('please check',i))
      next
    }
    split_matrix = cbind(split_matrix,lasttime,bid,ask)
    
    split_matrix = split_matrix[,-1]
   
    split_matrix = cbind(exchangeees,split_matrix)
    split_matrix = cbind(pairrrs,split_matrix)
    split_matrix = cbind(time_lines,split_matrix)
    #split_matrix = cbind(minutes_index,split_matrix)
    colnames(split_matrix) = c('timestamp','pairs','exchange','lasttime','bid1price','ask1price')
    filename = paste(splittt[[1]][2],splittt[[1]][3],splittt[[1]][4],today_time,sep = '_')
    filename = paste(filename,'.csv',sep = '')
  
    write.csv(split_matrix,file =paste('/Users/yangjichen/Desktop/6.7/20180605',fold_name,filename,sep = '/'))
  }

}



csv = c('ba_depth','hbp_depth','okexf_depth','okexs_depth')
csvv = paste(csv,today_time,sep='_')
files = paste(csvv,'.csv',sep='' )

for (file in files){
  trades = read.csv(file = file, header = T, sep = ',')
  try(depth_split(time_inter=60))
}

