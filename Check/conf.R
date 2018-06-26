rootpath = '/Users/yangjichen/Desktop/6.7/btcdataA/'
date = '20180615'  #20xx
exchange = c('ba','hbp','okexf')  #ba
curr = c('usd','btc')     #btc

path = paste(rootpath,date,sep = '')
path = paste(path,exchange,sep='/')

filename = paste(curr,date,sep ='_')
filename = paste(filename,'.csv',sep ='')
