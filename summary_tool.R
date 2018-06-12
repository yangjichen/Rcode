require(gdata)

setwd('/Users/yangjichen/Desktop/6.7/trades')
data_name = commandArgs(T)  

#print('请输入文件名及类型：')
#data_name = scan(what ='character')
#data_name = readline()

# For btx
if (data_name[2] == 'btx'){
  trades = read.csv(file = data_name[1], header = T, sep = ',',fileEncoding="UCS-2LE")
  pair = names(summary(trades$Exchange))
  output_matrix = matrix(ncol=3)
  
  for (i in 1:length(pair)){
    Exchange = trades[trades$Exchange ==pair[i],]
    for(j in c('LIMIT_BUY','LIMIT_SELL')){
      Exchange_buy = Exchange[Exchange$Type ==j,]
      Total_quantity = sum(Exchange_buy$Quantity)
      Total_price = sum(Exchange_buy$Price)
      if(Total_quantity==0){
        output_matrix = rbind(output_matrix,c(NA,0,0))
      }
      else{
        Avg_price = Total_price/Total_quantity
        output_matrix = rbind(output_matrix,c(Avg_price,Total_price,Total_quantity))
      }
    }
  }
  row_name = rep(NA,2*length(pair))
  num_iter = 2*length(pair)
  for (i in 1:num_iter){
    if (i%%2==0){
      row_name[i] = paste(pair[ceiling(i/2)],'Sell')
    }
    else{
      row_name[i] = paste(pair[ceiling(i/2)],'Buy')
    }
  }
  output_matrix= cbind(row_name,output_matrix[-1,])
  colnames(output_matrix) = c('row_name','Avg_price','Total_price','Total_quantity')

}  
  
  # For ba
  
if(data_name[2] == 'ba'){
  trades = read.csv(file = data_name[1],header = T, sep = ',',fileEncoding="GBK")
  pair = names(summary(trades$市场))
  output_matrix = matrix(ncol=3)
  
  for (i in 1:length(pair)){
    Exchange = trades[trades$市场 ==pair[i],]
    for(j in c('买','卖')){
      Exchange_buy = Exchange[Exchange$类型 == j,]
      Total_quantity = sum(Exchange_buy$数量 )
      Total_price = sum(Exchange_buy$成交额 )
      if(Total_quantity==0){
        output_matrix = rbind(output_matrix,c(NA,0,0))
      }
      else{
        Avg_price = Total_price/Total_quantity
        output_matrix = rbind(output_matrix,c(Avg_price,Total_price,Total_quantity))
      }
    }
  }
  
  row_name = rep(NA,2*length(pair))
  num_iter = 2*length(pair)
  for (i in 1:num_iter){
    if (i%%2==0){
      row_name[i] = paste(pair[ceiling(i/2)],'Sell')
    }
    else{
      row_name[i] = paste(pair[ceiling(i/2)],'Buy')
    }
  }
  output_matrix= cbind(row_name,output_matrix[-1,])
  colnames(output_matrix) = c('row_name','Avg_price','Total_price','Total_quantity')

  
  #write.csv(output_matrix,file = 'summary_xaba_20180607')
}

# For kk
if(data_name[2] == 'kk'){
  trades = read.csv(data_name[1], header = T, sep = ',')
  pair = names(summary(trades$Coins))
  output_matrix = matrix(ncol=3)
  for (i in 1:length(pair)){
    Exchange = trades[trades$Coins ==pair[i],]
    for(j in c('BUY','SELL')){
      Exchange_buy = Exchange[Exchange$Sell.Buy ==j, ]
      Total_Volume = sum(Exchange_buy$Volume)
      Total_Amount = sum(Exchange_buy$Amount)
      if(Total_Amount==0){
        output_matrix = rbind(output_matrix,c(NA,0,0))
      }
      else{
        Avg_price = Total_Volume/Total_Amount
        output_matrix = rbind(output_matrix,c(Avg_price, Total_Volume, Total_Amount))
      }
    }
  }
  
  row_name = rep(NA,2*length(pair))
  num_iter = 2*length(pair)
  for (i in 1:num_iter){
    if (i%%2==0){
      row_name[i] = paste(pair[ceiling(i/2)],'Sell')
    }
    else{
      row_name[i] = paste(pair[ceiling(i/2)],'Buy')
    }
  }
  
  output_matrix= cbind(row_name,output_matrix[-1,])
  colnames(output_matrix) = c('row_name','Avg_price','Total_Volume','Total_Amount')
}
print(output_matrix)
#write.csv(output_matrix,file = 'summary_tmykk_20180607.csv')
