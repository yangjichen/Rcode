source('conf.R')

deltaTcheck = function(newdata,pair,exch){
  deltaT = newdata$timestamp-newdata$tlastMsg
  num1 = sum(deltaT < 0)
  num2 = sum(deltaT > 60)
  if (num1==0 & num2==0){
    return(TRUE)
  }else{
    print(paste(exch,pair, '#(deltaT)<0 =',num1))
    print(paste(exch,pair, '#(deltaT)>60 =',num2))
    return(FALSE)
  }
}


NAcheck = function(newdata,pair){
  if(sum(is.na(newdata$bid))>0){
    print(paste('NA:',exch, pair,'have NA'))
    return(FALSE)
  }else{return(TRUE)}
  
}



Datacheck = function(path,filename){
  for (i in path){
    files = list.files(path=i, pattern="*.csv")
    exch = strsplit(i,'/')[[1]][8]
    for (j in filename){
      if (j %in% files){
        trades = read.csv(paste(i, j, sep = '/'))
        
        currency = strsplit(j,'_')[[1]][1]
        if (currency =='btc'){
          pairs = c('btc_eth','btc_ltc','btc_etc','btc_bch','btc_eos','btc_dash','btc_xmr','btc_zec')
        }else if(currency =='usd'){
          pairs = c('usd_btc','usd_eth','usd_ltc','usd_etc','usd_bch','usd_eos','usd_dash','usd_xmr','usd_zec')
        }
        
        for (k in pairs){
          if(k %in% names(summary(trades$pair))){
            indx = (trades$pair == k)
            newdata = trades[indx, ]
            rest1 = deltaTcheck(newdata,k,exch)
            rest2 = NAcheck(newdata,k)
            
            if(rest1&rest2){
              print(paste('Pair:',exch,k,'is normal'))}
          }      else{
            print(paste('Pair:',exch,k,'is missing'))
        }
       }
  
      }else{print(paste('csvData:',exch,j,'is missing'))}
      
    }
  }
}

Datacheck(path,filename)