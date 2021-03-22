library(shiny);
library(quantmod);
library(dplyr);
library(highcharter);
library(plotly);
library(DT);
library(vars);
library(rlist);
library(tidyr);
library(purrr);
library(tidyquant)
test bidon git
source("~/R/projets/PEA_tracker/R/fonctions_communes.R")
load("~/R/projets/PEA_tracker/base/ticktable.rData")

#téléchargement brut des données et mise en forme dans un nested df
download<-function(ticklist) {
  tq_get(ticklist,get="stock.prices",complete_cases=TRUE)%>% group_by(across(1)) %>% nest(data=-c(Ticker,Name,Exchange,Type,Category, Country))
}

#computation des nb de miss NA sur tous les horizons listés dans input_list
  # ex pour c(10,200): stats des NA sur les 10 et 200 valeurs les plus récentes
#on travaille à partir des données brutes nested (fonction download) et on enrichit le dataframe
stats_na<-function(df,input_list){
  df %>% cbind(map_df(.$data,~agregate_NA(.x$close,input_list)))
}

#compute les indicateurs dans le nested dataframe filtré (tickers avec trop de NA éliminés)
techana<-function(df){
  #t<-df$data %>% map(function(.x){
    df<-df %>% mutate(data=map(data,function(.x){
      
    .x%>%
    mutate_at(vars(c(open,high,low,close,volume,adjusted)),~filling(.)) %>%
    
    
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
    mutate(cmf = tq_cmf(clv, volume, 20))
    
    
    
  })

    )
  df
}
