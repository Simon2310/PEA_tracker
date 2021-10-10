#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
library(shiny)

portefeuille_netUI <- function(id, label = "portefeuille_net") {
    ns <- NS(id)
        tagList(
            fluidPage(
            
              fluidRow(
                column(4,
                       DT::dataTableOutput(ns("out"))
                ),
                column(8, highchartOutput(ns("parti")))
                
              ),
           
              # Sidebar
              fluidRow(
                  column(4,
                      selectInput(ns("type"),
                                  "type d'actif :", choices=NULL,multiple=TRUE,selected=NULL)
                  ),
                  column(8, highchartOutput(ns("values")))
                  
              ),
              

              fluidRow(
                column(4,
                      )
                ,
                column(8, DT::dataTableOutput(ns("bilan"))),
              )
            )
        )
}

portefeuille_netServer <- function(id) {
    moduleServer(
        id,
        function(input, output,session) {

          observe( { req(STORED$ope)
            updateSelectInput(session,"actif",choices=STORED$reg %>% distinct(libelle) %>% pull(libelle))
            updateSelectInput(session,"type",choices=STORED$reg %>% distinct(type) %>% pull(type))
          })
          
          
          bilan<-reactive ({
            bilan<-STORED$reg%>%group_by(operation,libelle,Ticker)%>%summarise(Montant=sum(montant_final))%>%spread(key=operation,value=Montant) %>%
             left_join(STORED$last_val,by="Ticker")
          colnames(bilan)<-c("Actif","Ticker","Montant investi","Montant retiré","Montant restant")
          bilan
          })
          
        
          valuesInput <- reactive({
            req(input$type)
            if ("all" %in% input$type){STORED$reg %>% distinct(type) %>% pull(type)}
            else {STORED$reg %>% filter(type %in% input$type) %>% distinct(Ticker) %>% pull(Ticker)}
          })
          
          
          reactiveHighchart1 <- reactive({
            req(STORED$ope)
            
            req(input$type)
             hc<-highchart(type="stock") %>%
                #hc_title(text = "Répartition du portefeuille") %>%
  
                hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
               <b>{point.percentage:.1f}%</b> ({point.y:,.0f} E)<br/>",
                           shared = TRUE) 
             if ("OPC" %in% valuesInput()){
                for (symb in valuesInput()){
                  hc <- hc %>%
                  hc_add_series(name = symb, data = STORED$val_net_inv[,symb])
                }
             }
             else {
                for (symb in valuesInput()){
                  hc <- hc %>%
                  hc_add_series(name = STORED$tick[[which(STORED$tick$Ticker==symb)[1],'libelle']], data = STORED$val_net_inv[,symb],lineWidth=1)
                }
               for (cat in input$type){
                 hc <- hc %>%
                   hc_add_series(name = cat, data = STORED$val_net_inv[,cat],lineWidth=3)
               }
              
             }
              
             hc
              
            })
          
          reactiveHighchart2 <- reactive({
            req(STORED$ope)

            bilan_filter<-bilan()[input$out_rows_selected,]%>% dplyr::select(-c("Ticker"))

            hcc<-highchart(type = "stock") %>%
              hc_yAxis_multiples(create_yaxis(2, height = c(3, 1), turnopposite = TRUE))
            
            for (symb in STORED$tick[input$out_rows_selected,"Ticker"]){
              data_flags<-STORED$reg %>% filter(Ticker==symb) %>% dplyr::select(date,operation,quantité)
              
              colnames(data_flags)<-c("date","title","text")
              hcc <- hcc %>%
                hc_add_series(name = STORED$tick[[which(STORED$tick$Ticker==symb)[1],'libelle']],id=symb, data = STORED$val_net_inv[,symb],yAxis=0) %>%
                hc_add_series(
                  data_flags, 
                  hcaes(x = date),
                  type = "flags", 
                  onSeries = symb
                ) %>%                hc_add_series(name = STORED$tick[[which(STORED$tick$Ticker==symb)[1],'libelle']], data = STORED$ope[,symb],yAxis=1,type="line") 

              
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
          
          output$out <- DT::renderDataTable({
            STORED$tick%>% arrange(desc(quantité))
          })
          
      }
    )
}