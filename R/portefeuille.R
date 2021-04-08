#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
library(shiny)

portefeuilleUI <- function(id, label = "portefeuille") {
    ns <- NS(id)
        tagList(
            fluidPage(
    
           
              # Sidebar
              fluidRow(
                  column(3,
                      selectInput(ns("type"),
                                  "type d'actif :", choices=NULL,multiple=TRUE,selected=NULL)
                  ),
                  column(9, highchartOutput(ns("values")))
                  
              ),
              
              fluidRow(
                column(3,
                       selectInput(ns("actif"),
                                   "type d'actif :", choices=registre %>% distinct(libelle) %>% pull(libelle),multiple=TRUE,selected=NULL)
                ),
                column(9, highchartOutput(ns("parti")))
   
              )
            )
        )
}

portefeuilleServer <- function(id,registreReac) {
    moduleServer(
        id,
        function(input, output,session) {
        

          
          valuesInput <- reactive({
            req(input$type)
            if ("all" %in% input$type){registreReac()$reg %>% distinct(type) %>% pull(type)}
            else {registreReac()$reg %>% filter(type %in% input$type) %>% distinct(Ticker) %>% pull(Ticker)}
          })
          
          actifTicker<-reactive({
            req(input$actif)
            registreReac()$tick %>%  filter(Nom %in% input$actif) %>% distinct(Ticker) %>% pull(Ticker)
          })
          
          
           reactiveHighchart1<-reactive ({ 
             req(registreReac()$ope)
             hc<-highchart(type="stock") %>%
                hc_chart(type = "area") %>%
                #hc_title(text = "Répartition du portefeuille") %>%
  
                hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
               <b>{point.percentage:.1f}%</b> ({point.y:,.0f} E)<br/>",
                           shared = TRUE) %>%
                hc_plotOptions(area = list(
                  stacking = "normal",
                  lineColor = "#ffffff",
                  lineWidth = 1#,
                  #marker = list(
                   # lineWidth = 1,
                   # lineColor = "#ffffff")
                  )
                )
             if ("OPC" %in% valuesInput()){
                for (symb in valuesInput()){
                  hc <- hc %>%
                  hc_add_series(name = symb, data = registreReac()$val_agr[,symb])
                }
             }
             else {
                for (symb in valuesInput()){
                  hc <- hc %>%
                  hc_add_series(name = registreReac()$tick[[which(registreReac()$tick$Ticker==symb)[1],'Nom']], data = registreReac()$val_agr[,symb])
                }
              
             }
              
             hc
              
            })
          

            reactiveHighchart2<-reactive ({ 
              req(registreReac()$ope)
              
              hcc<-highchart(type = "stock")
              
              for (symb in actifTicker()){
                data_flags<-registreReac()$reg %>% filter(Ticker==symb) %>% dplyr::select(date,operation,quantité)
  
                colnames(data_flags)<-c("date","title","text")
                hcc <- hcc %>%
                 hc_add_series(name = registreReac()$tick[[which(registreReac()$tick$Ticker==symb)[1],'Nom']],id=symb, data = registreReac()$val[,symb]) %>%
                  hc_add_series(
                    data_flags, 
                    hcaes(x = date),
                    type = "flags", 
                    onSeries = symb
                  )
                
              }
              hcc
            })

          output$values <- renderHighchart({
            reactiveHighchart1()
          })
          
          output$parti <- renderHighchart({
            reactiveHighchart2()
          })
      }
    )
}