library(VIM)

mise_en_forme_NA<- function() {
  #créé une matrice avec une colonne date + une colonne par ticker
    screen_brut%>%unnest() %>%
    dplyr::select(-c(Country,open,volume,high,low,adjusted,Type)) %>% 
    dplyr::select(-c(Exchange,Category,Name)) %>%
    spread(key=Ticker,value=close)
}

visu_NA<- function(NAmat,sample_size) {
  #créé une visualisation des emplacements NA pour un nb sample_size au hasard dans une matrice avec une colonne par ticker (sortie de mise_en_forme_NA par exemple)
  matrixplot(NAmat[,sample(ncol(NAmat),sample_size)])
  #le passé est en bas
}


agregate_NA<-function(list_in,input_list){
  #déroule et synthétise consecutive_NA sur plusieurs fenêtres temporelles. En entrée il faut prendre df$close pour chaque ticker
  sortie=list()
  for (i in input_list){
    r<-rle(is.na(tail(list_in,i)))
    sortie[[paste0("ncon_",i)]]<-max(r$lengths[r$values])/i
    sortie[[paste0("ntot_",i)]]<-sum(r$lengths[r$values])/i
  }
  sortie%>%as.data.frame()
}