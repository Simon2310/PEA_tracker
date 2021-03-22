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

source("~/R/projets/PEA_tracker/R/fonctions_communes.R")
load("~/R/projets/PEA_tracker/base/ticktable.rData")

Analysis<-function(ticklist) {
  base<-tq_get(ticklist,get="stock.prices",complete_cases=TRUE)
  nadeal<-base %>% group_by(across(1))  %>%
    mutate_at(vars(c(open,high,low,close,volume,adjusted)),~filling(.)) %>%
    #~na.locf((na.locf((na.spline(.,na.rm=FALSE)),na.rm=FALSE)),fromLast = TRUE))  %>%
    
    
    #day return
    tq_mutate(select = close, mutate_fun = periodReturn,period = "daily", type = "log",col_rename = "day_return") %>%
    #Short SMA
    tq_mutate(select = close, mutate_fun = SMA,n = 10,col_rename = "short_SMA") %>%
    #long SMA
    tq_mutate(select = close, mutate_fun = SMA,n = min(50,group_size(.)-15),col_rename = "mid_SMA") %>%
    #Long SMA
    tq_mutate(select = close, mutate_fun = SMA,n = min(200,group_size(.)-15),col_rename = "long_SMA") %>%
    #RSI, Wilder EMA par défaut
    tq_mutate(select = close, mutate_fun = RSI,col_rename = "RSI") %>%
    #MACD: valeur (MACD=(Short SMA-Long SMA), et signal=EMA de MACD. Delta= MACD-Signal. Buy de - à +. Sell de + à -)
    tq_mutate(select=close,mutate_fun=MACD, nFast=12, nSlow=26,nSig=9, percent=FALSE) %>%
    mutate("delta_MACD"=macd-signal) %>%
    #Bollinger Bands (n period MA, standard deviation pour up and down, et ratio ((MA price-down)/(up-down)))
    tq_mutate(select = close, mutate_fun = BBands,n=20,sd=2,col_rename=c("BB_down","BB_MA","BB_up","BB_ratio")) %>%
    #ROC:((Pt-Pt-K)/Pt : fixer K, et discrete or continuous buy quand passe positif, sell négatif)
    tq_mutate(select = close, mutate_fun = ROC,type='discrete',n=6) %>%
    #Volume indicator OBV
    tq_mutate_xy(x=close,y=volume, mutate_fun = OBV) %>%
    #CMF en 2 étapes
    tq_mutate(select = c(high, low, close), mutate_fun = CLV) %>%
    mutate(cmf = tq_cmf(clv, volume, 20)) %>%
    
    
    nest(data=-c(Ticker,Name,Exchange,Type,Category, Country))
  
  
  brut <- base %>% group_by(across(1)) %>% nest(data=-c(Ticker,Name,Exchange,Type,Category, Country))
  
  TAout=list(brut,nadeal)
}
