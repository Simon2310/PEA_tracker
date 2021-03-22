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
library(tidyr)


load("~/R/projets/PEA_tracker/base/transactions_test.rData")

load("~/R/projets/PEA_tracker/base/tickers.rData")

source("~/R/projets/PEA_tracker/R/fonctions_communes.R")

#"génération de histo_port à la volée à partir du registre: somme cumulative des quantités d'actifs possédés (positif à l'achat, négatif à la vente)
histo_port<-registre %>% dplyr::select(date,quantité,Ticker,operation) %>% mutate(abs=case_when(operation=="achat" ~ quantité,operation=="vente" ~ -quantité )) %>% 
  spread(key=Ticker,value=abs) %>%
  group_by(date) %>%
  summarise_at(vars(-operation,-quantité),funs(sum(., na.rm = TRUE))) %>%
 mutate_if(is.numeric,cumsum)
#génération de inv_port à la volée à partir du registre: par actif, somme cumulative des montants investis (positif à l'achat, négatif à la vente)
inv_port<-registre %>% dplyr::select(date,montant_final,Ticker,operation) %>% mutate(abs=case_when(operation=="achat" ~ montant_final,operation=="vente" ~ -montant_final )) %>% 
  spread(key=Ticker,value=abs) %>%
  group_by(date) %>%
  summarise_at(vars(-operation,-montant_final),funs(sum(., na.rm = TRUE))) %>%
  mutate_if(is.numeric,cumsum)

#passage en xts des df
histo_port<-xts(histo_port%>% dplyr::select(-c("date")),order.by=as.Date(histo_port$date)) 
inv_port<-xts(inv_port%>% dplyr::select(-c("date")),order.by=as.Date(inv_port$date)) 



#on prend la liste des actifs correspondant au filtre utilisateur (OPC, ETF, action ..)
list_label<-(registre %>% distinct(Ticker))
#on va chercher les valeurs
valeurs<-(listing(list_label$Ticker))
#on renomme les colonnes pour avoir le ticker sans ".close"
colnames(valeurs)<-list_label$Ticker
#on vire les NA par interpolation
valeurs<-filling(valeurs)

#creation d'un xts avec les quantités des actifs à chaque pas de temps, depuis histo_port
operations<-merge(histo_port,index(valeurs)) %>% na.locf()
colnames(operations)<-colnames(histo_port)

#idem depuis inv_port
investissement<-merge(inv_port,index(valeurs)) %>% na.locf()
colnames(investissement)<-colnames(inv_port)

#préparation d'un xts vide, qui contiendra la valeur finale à représenter (total du portefeuille sur filtre sélectionné)
valeur_agr<-xts(order.by=index(valeurs))
valeur_net_inv<-xts(order.by=index(valeurs))

#multiplication colonne par colonne des prix*quantités
for (symb in list_label$Ticker){
        valeur_agr<-merge(valeur_agr,xts(operations[,symb]*valeurs[,symb]))
        valeur_net_inv<-merge(valeur_net_inv,xts(operations[,symb]*valeurs[,symb]-investissement[,symb]))
}
  

colnames(valeur_agr)<-list_label$Ticker
colnames(valeur_net_inv)<-list_label$Ticker

#agrégation par catégorie, pour rendu utilisateur

categories<-registre %>% distinct(type) %>% pull(type)


for (cat in categories){
        names<-colnames(valeur_agr)
        elt<-registre %>% filter(type==cat) %>% distinct(Ticker) %>% pull(Ticker)
        new<-valeur_agr[,elt] %>% rowSums() %>% xts(order.by = index(valeur_agr))
        new_inv<-valeur_net_inv[,elt] %>% rowSums() %>% xts(order.by = index(valeur_net_inv))
        valeur_agr<-merge(valeur_agr,new)
        valeur_net_inv<-merge(valeur_net_inv,new_inv)
        colnames(valeur_agr)<-c(names,cat)
        colnames(valeur_net_inv)<-c(names,cat)
        
}






#fonction pour aller chercher les prix des actifs du portefeuille, depuis début 2020, et retourner un unique xts avec les valeurs



# process:
#ouverture session 
        #1.aller chercher les valeurs
        #2.ensuite fill na
        #3.puis calculer les valeurs au jour le jour: prendre histo_port et caler sur les index du résultat de listing: merge(histo_port,index(listing)) %>% na.locf(). 
#sur demande utilisateur                       
        #Multiplier ligne par ligne, en sélectionnant les lignes appropriées (boucle for sur liste des actifs répondant à un critère (ETF, action ...) à partir du registre)
                #registre %>% filter(type=="OPC") %>% distinct(Ticker) : donne la liste des tickers du portefeuille selon un critère (à mettre en réactive)
                
        
#si l'utilisateur constate une bizzarerie: repartir des valeurs de step1, et proposer de donner la valeur du jour, avant de relancer les na.fill (step 2)


