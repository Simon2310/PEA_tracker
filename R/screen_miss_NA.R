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

consecutive_NA<- function(df_in,size) {
  #dans le data.frame avec les valeurs, regarde le nb max de consecutive NA et le total sur les nb=size valeurs les plus récentes (en bas du df donc tail)
  
    df_in%>%tail(size) %>% map_df(function(.x) {
      r<-rle(is.na(.x))
      data.frame(n_con=max(r$lengths[r$values]),n_tot=sum(r$lengths[r$values]))
    })
}

agregate_NA<-function(df_in,...){
  #déroule et synthétise consecutive_NA sur plusieurs fenêtres temporelles (autant que voulu), en sortie un data.frame
  df<-data.frame(name=colnames(df_in))
  
  for (i in list(...)){
    names<-colnames(df)
    df<-cbind(df,consecutive_NA(df_in,i))
    colnames(df)<-c(names,paste0("ncon_",i),paste0("ntot_",i))
  }
  df
}