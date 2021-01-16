#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
library(shiny)

portefeuilleUI <- function(id, label = "portefeuille_net") {
    ns <- NS(id)
        tagList(
            fluidPage(
    
           
              # Sidebar
              fluidRow(
                  column(3,
                      selectInput(ns("type"),
                                  "type d'actif :", choices=c("all",registre %>% distinct(type) %>% pull(type)),multiple=TRUE,selected=NULL)
                  ),
                  column(9, highchartOutput(ns("values")))
                  
              ),
              
              fluidRow(
                column(3,
                       selectInput(ns("actif"),
                                   "type d'actif :", choices=registre %>% distinct(libelle) %>% pull(libelle),multiple=TRUE,selected=NULL)
                ),
                column(9, highchartOutput(ns("parti")))
   
              ),
              fluidRow(
                column(3,
                      )
                ,
                column(9, DT::dataTableOutput(ns("bilan"))),
            
                
              )
            )
        )
}

portefeuille_netServer <- function(id) {
    moduleServer(
        id,
        function(input, output,session) {
          
          last_value<-as.data.frame(t(valeur_agr[nrow(valeur_agr),]))
          last_value$Ticker<-rownames(last_value)
          bilan<-registre%>%group_by(operation,libelle,Ticker)%>%summarise(Montant=sum(montant_final))%>%spread(key=operation,value=Montant) %>%
             left_join(last_value,by="Ticker")
          colnames(bilan)<-c("Actif","Ticker","Montant investi","Montant retiré","Montant restant")
          
        
          valuesInput <- reactive({
            req(input$type)
            if ("all" %in% input$type){registre %>% distinct(type) %>% pull(type)}
            else {registre %>% filter(type %in% input$type) %>% distinct(Ticker) %>% pull(Ticker)}
          })
          
          actifTicker<-reactive({
            req(input$actif)
            tickers %>%  filter(Nom %in% input$actif) %>% distinct(Ticker) %>% pull(Ticker)
          })
          
          reactiveHighchart1 <- reactive({
            req(input$type)
             hc<-highchart(type="stock") %>%
                #hc_title(text = "Répartition du portefeuille") %>%
  
                hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
               <b>{point.percentage:.1f}%</b> ({point.y:,.0f} E)<br/>",
                           shared = TRUE) 
             if ("OPC" %in% valuesInput()){
                for (symb in valuesInput()){
                  hc <- hc %>%
                  hc_add_series(name = symb, data = valeur_net_inv[,symb])
                }
             }
             else {
                for (symb in valuesInput()){
                  hc <- hc %>%
                  hc_add_series(name = tickers[[which(tickers$Ticker==symb)[1],'Nom']], data = valeur_net_inv[,symb],lineWidth=1)
                }
               for (cat in input$type){
                 hc <- hc %>%
                   hc_add_series(name = cat, data = valeur_net_inv[,cat],lineWidth=3)
               }
              
             }
              
             hc
              
            })
          
          reactiveHighchart2 <- reactive({
            req(input$actif)
            bilan_filter<-bilan%>%filter(Ticker %in% actifTicker())%>% dplyr::select(-c("Ticker"))

            hcc<-highchart(type = "stock") %>%
              hc_yAxis_multiples(create_yaxis(2, height = c(3, 1), turnopposite = TRUE))
            
            for (symb in actifTicker()){
              data_flags<-registre %>% filter(Ticker==symb) %>% dplyr::select(date,operation,quantité)
              
              colnames(data_flags)<-c("date","title","text")
              hcc <- hcc %>%
                hc_add_series(name = tickers[[which(tickers$Ticker==symb)[1],'Nom']],id=symb, data = valeur_net_inv[,symb],yAxis=0) %>%
                hc_add_series(
                  data_flags, 
                  hcaes(x = date),
                  type = "flags", 
                  onSeries = symb
                ) %>%
                hc_add_series(name = tickers[[which(tickers$Ticker==symb)[1],'Nom']], data = operations[,symb],yAxis=1,type="column") 
              
              #pb: si pas de vente, ou d'achat, alors la ligne dessus plante
            }
            
            combo <- list(chart = hcc, bilan_table=bilan_filter)
            
          })
          
          output$values <- renderHighchart({
            reactiveHighchart1()
          })
          
          output$parti <- renderHighchart({
            reactiveHighchart2()$chart
          })
          
          output$bilan <-DT::renderDataTable({
            reactiveHighchart2()$bilan_table
          })
          
      }
    )
}