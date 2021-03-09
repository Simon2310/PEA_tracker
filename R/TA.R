library(shiny);
library(quantmod);
library(dplyr);
library(highcharter);
library(plotly);
library(DT);
#library(tsm);
library(vars);
#library(mFilter);
library(rlist);
library(tidyr);
library(purrr);
library(tidyquant)



Analysis<-function(ticklist) {
  tq_get(ticklist,get="stock.prices",complete_cases=FALSE) %>% 
    group_by(symbol) %>% 
    
    #day return
    tq_mutate(select = close, mutate_fun = periodReturn,period = "daily", type = "log",col_rename = "day_return") %>%
    #Short SMA
    tq_mutate(select = close, mutate_fun = SMA,n = 10,col_rename = "short_SMA") %>%
    #long SMA
    tq_mutate(select = close, mutate_fun = SMA,n = 50,col_rename = "mid_SMA") %>%
    #Long SMA
    tq_mutate(select = close, mutate_fun = SMA,n = 200,col_rename = "long_SMA") %>%
    #RSI, Wilder EMA par défaut
    tq_mutate(select = close, mutate_fun = RSI,col_rename = "RSI") %>%
    #MACD: valeur (MACD=(Short SMA-Long SMA), et signal=EMA de MACD. Delta= MACD-Signal. Buy de - à +. Sell de + à -)
    tq_mutate(select=close,mutate_fun=MACD, nFast=12, nSlow=26,nSig=9, percent=FALSE) %>%
    mutate("delta_MACD"=macd-signal) %>%
    #Bollinger Bands (n period MA, standard deviation pour up and down, et ratio ((MA price-down)/(up-down)))
    tq_mutate(select = close, mutate_fun = BBands,n=20,sd=2,col_rename=c("BB_down","BB_MA","BB_up","BB_ratio")) %>%
    #ROC:((Pt-Pt-K)/Pt : fixer K, et discrete or continuous buy quand passe positif, sell négatif)
    tq_mutate(select = close, mutate_fun = ROC,type='discrete',n=6) %>%
    
    nest()
}
