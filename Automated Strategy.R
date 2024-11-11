
#EMA Strategy
ema_trading_function <- function(symbol,n1,n2){
  library(quantmod)
  library(PerformanceAnalytics)
  library(writexl)
  library(dplyr)
  
  stock <- getSymbols(symbol, from = "2020-01-01", auto.assign = FALSE)
  head(stock)
  stock <- na.locf(stock)
  
  ema_n1 <- EMA(Ad(stock), n1)
  ema_n2 <- EMA(Ad(stock), n2)
  
  ema_trading_signal <-  lag(ifelse(ema_n1>ema_n2 & lag(ema_n1) < lag(ema_n2),1,ifelse(ema_n1<ema_n2 & lag(ema_n1)>lag(ema_n2), -1, 0)))
  ema_trading_signal[is.na(ema_trading_signal)] <- 0
  
  nrow(stock)

  ema_trading_position <- 0L
  changeover <- 0L
  i=1
  for (i in 1:nrow(stock)){
    if(ema_trading_signal[i]==1){
      ema_trading_position[i] =  1
      changeover = 1
    }else if(ema_trading_signal[i]==-1){
      ema_trading_position[i] = 0
      changeover = 0
    }else{
      ema_trading_position[i] = changeover
    }
  }
  
  
  daily_return <- Return.calculate(Ad(stock), method = "discrete")
  strategy_return <- ema_trading_position * daily_return
  daily_return[is.na(daily_return)] <- 0
  strategy_return[is.na(strategy_return)] <- 0
  cumulative_strategy_return <- cumprod(1+strategy_return) - 1
  cumulative_stock_return <- cumprod(1+daily_return) - 1
  

  cat("Cumulative Daily Return:", tail(cumulative_stock_return,1), "\n")
  cat("EMI Cumulative STrategy Return:", tail(cumulative_strategy_return,1), "\n")
}

ema_trading_function("ASIANPAINT.NS",5,21)



# SMI Return

smi_trading_function <- function(symbol){
  library(quantmod)
  library(PerformanceAnalytics)
  library(writexl)
  library(dplyr)
  
  stock <- getSymbols(symbol, from = "2020-01-01", auto.assign = FALSE)
  stock <- na.locf(stock)
  hlc <- HLC(stock)
  smi_func <- SMI(hlc,13,2,25,9)
  smi <- smi_func[,1]
  signal <- smi_func[,2]
  
  smi_trading_signal <- lag(ifelse(smi>signal & lag(smi)<lag(signal),1,ifelse(smi<signal & lag(smi)>lag(signal),-1,0)))
  smi_trading_signal[is.na(smi_trading_signal)] <- 0
  smi_trading_position <- 0L
  changeover <- 0L
  for (i in 1:nrow(stock)) {
    if (smi_trading_signal[i]==1){
      smi_trading_position[i] <- 1
      changeover=1
    }else if(smi_trading_signal[i]==-1){
      smi_trading_position[i] <- 0
      changeover=0
    }else{
      smi_trading_position[i]=changeover}
    
  }
  
  
  daily_return <- Return.calculate(Ad(stock), method = "discrete")
  strategy_return <- smi_trading_position * daily_return
  daily_return[is.na(daily_return)] <- 0
  strategy_return[is.na(strategy_return)] <- 0
  cumulative_strategy_return <- cumprod(1+strategy_return) - 1
  cumulative_stock_return <- cumprod(1+daily_return) - 1
  
  cat("Cumulative Daily Return:", tail(cumulative_stock_return,1), "\n")
  cat("SMI Cumulative STrategy Return:", tail(cumulative_strategy_return,1), "\n")
  
}

smi_trading_function("ASIANPAINT.NS")



# RSI Return

rsi_trading_function <- function(symbol){
  library(quantmod)
  library(PerformanceAnalytics)
  library(writexl)
  library(dplyr)
  
  stock <- getSymbols(symbol, from = "2020-01-01", auto.assign = FALSE)
  stock <- na.locf(stock)
  rsi <- RSI(Ad(stock),14)
  rsi[is.na(rsi)] <- 0
  
  rsi_trading_signal <- lag(ifelse(rsi<30,1,ifelse((rsi>70),-1,0)))
  rsi_trading_signal[1:15] <- 0
  rsi_trading_signal[is.na(rsi_trading_signal)] <- 0
  rsi_trading_position <- 0L
  changeover <- 0L
  for (i in 1:nrow(stock)) {
    if (rsi_trading_signal[i]==1){
      rsi_trading_position[i] <- 1
      changeover=1
    }else if(rsi_trading_signal[i]==-1){
      rsi_trading_position[i] <- 0
      changeover=0
    }else{rsi_trading_position[i]=changeover}
    
  }
  
  
  daily_return <- Return.calculate(Ad(stock), method = "discrete")
  strategy_return <- rsi_trading_position * daily_return
  daily_return[is.na(daily_return)] <- 0
  strategy_return[is.na(strategy_return)] <- 0
  cumulative_strategy_return <- cumprod(1+strategy_return) - 1
  cumulative_stock_return <- cumprod(1+daily_return) - 1
  
  cat("Cumulative Daily Return:", tail(cumulative_stock_return,1), "\n")
  cat("RSI Cumulative Strategy Return:", tail(cumulative_strategy_return,1), "\n")
  
}

rsi_trading_function("ASIANPAINT.NS")



# MACD Return

macd_trading_function <- function(symbol){
  library(quantmod)
  library(PerformanceAnalytics)
  library(writexl)
  library(dplyr)
  
  stock <- getSymbols(symbol, from = "2020-01-01", auto.assign = FALSE)
  stock <- na.locf(stock)
  x <- MACD(Ad(stock),nFast = 12,nSlow = 26,nSig = 9,percent = FALSE )
  macd <- x[,1]
  signal <- x[,2]
  
  macd_trading_signal <- lag(ifelse(macd>signal & lag(macd)<lag(signal),1,ifelse(macd<signal & lag(macd)>lag(signal),-1,0)))
  macd_trading_signal[is.na(macd_trading_signal)] <- 0
  macd_trading_position <- 0L
  changeover <- 0L
  for (i in 1:nrow(stock)) {
    if (macd_trading_signal[i]==1){
      macd_trading_position[i] <- 1
      changeover=1
    }else if(macd_trading_signal[i]==-1){
      macd_trading_position[i] <- 0
      changeover=0
    }else{macd_trading_position[i]=changeover}
    
  }
  
  
  daily_return <- Return.calculate(Ad(stock), method = "discrete")
  strategy_return <- macd_trading_position * daily_return
  daily_return[is.na(daily_return)] <- 0
  strategy_return[is.na(strategy_return)] <- 0
  cumulative_strategy_return <- cumprod(1+strategy_return) - 1
  cumulative_stock_return <- cumprod(1+daily_return) - 1
  
  cat("Cumulative Daily Return:", tail(cumulative_stock_return,1), "\n")
  cat("MACD Cumulative Strategy Return:", tail(cumulative_strategy_return,1), "\n")
  
}

macd_trading_function("ASIANPAINT.NS")



#Switch Functionality

ans <- function(x){
  result <- switch (as.character(x),
               "1"={ema_trading_function(symbol,5,21)},
               "2"={smi_trading_function(symbol)},
               "3"={rsi_trading_function(symbol)},
               "4"={macd_trading_function(symbol)},
               stop("Invalid Input"))
  return(result)
}


for (i in 1) {
  print("Please enter the Ticker")
  symbol <- scan(what ="" )
  print("Enter indicator choice: ")
  print("1 = EMA strategy") 
  print("2 = SMI strategy")
  print("3 = RSI strategy")
  print("4 = MACD strategy")
  strategy = scan(what = double())
  ans(strategy)
  
}

