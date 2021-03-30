

require(quantmod)
require(Quandl)
require(dplyr)
require(lubridate)
require(alphavantager)
require(patchwork)
require(tidyverse)
require(formattable)
require(plotly)
require(highcharter)

require(corrplot)
##AlphaVantage API Key
av_api_key("XPFWRLDOVN8GSZOB")
##Quandl API key
Quandl.api_key("q3xnVFJWH55yhwepLne7")


##holding period return
hpr<-function(x, n=1, reverse =F){
  if(reverse==F){
    rtn<-x[nrow(x),]/x[nrow(x)-n,]-1
  }else{
    rtn<-x[1,]/x[n+1,]-1
    
  }
  
  return(rtn)
}

## holding period difference for yields
hpdiff<-function(x, n=1, reverse =F){
  if(reverse==F){
    rtn<-x[nrow(x),]-x[nrow(x)-n,]
  }else{
    rtn<-x[1,]-x[n+1,]
    
  }
  
  return(rtn)
}


##performance indexed 

indexer<-function(x, reverse = F){
  if(reverse==F){
    indexed_perf <-  x / x[1,] * 100
    
    return(indexed_perf)
  }else{
    indexed_perf <-  x / x[nrow(x),] * 100
  }
}


##plotting function for fx prices

plot_fx<-function(x, fx, cut_off = 2019){
  x<-x%>%filter(FX==fx) %>% filter(year(Date)>cut_off)
  p <-ggplot(data = x, aes(x = Date, y = Price))+ xlab("")+ylab("")+ggtitle(fx)+
    geom_line()+theme_bw() 
  return(p)
}


## %change
pcp<-function(x, n=1, reverse =F){
  if(reverse){
    rtn <- x / lag(x) - 1
  } else {
    rtn <- lag(x) / x - 1
  }
  
  return(rtn)
}

pcpdiff<-function(x, n=1, reverse =F){
  if(reverse){
    
    rtn <- x - lag(x)
  } else {
    rtn <- lag(x) - x 
  }
  
  return(rtn)
}

framify<- function(x){
  
  x<-as.data.frame(x)
  x$Date<-ymd(rownames(x))
  x<- x[,c(ncol(x), ncol(x)-1)]
  
  colnames(x)[1]<-c('Date')
  return(x)
}


AUDUSD<- av_get("AUD/USD", av_fun = "FX_DAILY", outputsize="full") %>% select(timestamp, close)
AUDGBP<- av_get("AUD/GBP", av_fun = "FX_DAILY", outputsize="full") %>% select(timestamp, close)
USDJPY<- av_get("USD/JPY", av_fun = "FX_DAILY", outputsize="full") %>% select(timestamp, close)
EURUSD<- av_get("EUR/USD", av_fun = "FX_DAILY", outputsize="full") %>% select(timestamp, close)

Sys.sleep(65)

AUDEUR<- av_get("AUD/EUR", av_fun = "FX_DAILY", outputsize="full") %>% select(timestamp, close)
AUDJPY<- av_get("AUD/JPY", av_fun = "FX_DAILY", outputsize="full") %>% select(timestamp, close)
AUDCNY<- av_get("AUD/CNY", av_fun = "FX_DAILY", outputsize="full") %>% select(timestamp, close)
#
# 
fx.list<-c( 'AUDUSD', 'AUDGBP', 'USDJPY',
            'EURUSD', 'AUDEUR', 'AUDJPY',
            'AUDCNY')
# 

SP500 <- getSymbols("^GSPC", src = 'yahoo', auto.assign = F)
VIX <- getSymbols("^VIX", src = 'yahoo', auto.assign = F)
NASDAQ <- getSymbols("^IXIC", src = 'yahoo', auto.assign = F)

SP500<- framify(SP500)
VIX<- framify(VIX)
NASDAQ<- framify(NASDAQ)

Stks<-list(SP500, VIX, NASDAQ) %>% reduce(left_join, by ="Date")

stk.retn<- Stks %>% mutate_at(vars(-Date),pcp) %>% drop_na()





# 



fx.close <- list( AUDUSD, AUDGBP, USDJPY,
                  EURUSD, AUDEUR, AUDJPY,
                  AUDCNY) %>% reduce(left_join, by = "timestamp")

fx.retn <- fx.close %>% mutate_at(vars(-timestamp), pcp)

colnames(fx.retn)[1] <-"Date"



comb.rtn <- left_join(stk.retn, fx.retn, by = "Date" ) %>% drop_na()


colnames(comb.rtn)[2:4]<- c("SP500", "VIX", "NASDAQ")



colnames(comb.rtn)[5:length(comb.rtn)] <- fx.list

comb.corr<-cor(comb.rtn[,-1],method = 'spearman')


corr.dist<-2*(sqrt(1-comb.corr))

corr.dist <- dist(corr.dist)
hc <- hclust(corr.dist, method = "complete")
plot(hc)

as.matrix(corr.dist)
corrplot(comb.corr, method = 'shade',hclust.method = "complete", order="hclust", 
         addCoef.col = "black")

