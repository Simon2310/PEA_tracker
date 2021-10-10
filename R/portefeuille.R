#test
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
library(shiny)

portefeuilleUI <- function(id, label = "portefeuille") {
    ns <- NS(id)
        tagList(
            fluidPage(
    
              fluidRow(
                column(4,
                       DT::dataTableOutput(ns("out"))
                ),
                column(8, highchartOutput(ns("parti"),height = "600px"))
                
              ),           

              fluidRow(
                  column(4,
                      selectInput(ns("type"),
                                  "type d'actif :", choices=NULL,multiple=TRUE,selected=NULL)
                  ),
                  column(8, highchartOutput(ns("values")))
                  
              )
              
            )
        )
}

portefeuilleServer <- function(id) {
    moduleServer(
        id,
        function(input, output,session) {
        
          observe( { req(STORED$ope)
            #updateSelectInput(session,"actif",choices=STORED$reg %>% distinct(libelle) %>% pull(libelle))
            updateSelectInput(session,"type",choices=STORED$reg %>% distinct(type) %>% pull(type))
          })
          
          valuesInput <- reactive({
            req(input$type)
            if ("all" %in% input$type){STORED$reg %>% distinct(type) %>% pull(type)}
            else {STORED$reg %>% filter(type %in% input$type) %>% distinct(Ticker) %>% pull(Ticker)}
          })
          
           reactiveHighchart1<-reactive ({ 
             req(STORED$ope)
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
                  hc_add_series(name = symb, data = STORED$val_agr[,symb])
                }
             }
             else {
                for (symb in valuesInput()){
                  hc <- hc %>%
                  hc_add_series(name = STORED$tick[[which(STORED$tick$Ticker==symb)[1],'libelle']], data = STORED$val_agr[,symb])
                }
              
             }
              
             hc
              
            })
          

            reactiveHighchart2<-reactive ({ 
              req(STORED$ope)
              
              hcc<-highchart(type = "stock") %>%
                hc_yAxis_multiples(create_yaxis(2, height = c(3, 1), turnopposite = TRUE))
              
              for (symb in STORED$tick[input$out_rows_selected,"Ticker"]){
                data_flags<-STORED$reg %>% filter(Ticker==symb) %>% dplyr::select(date,operation,quantité)
  
                colnames(data_flags)<-c("date","title","text")
                hcc <- hcc %>%
                 hc_add_series(name = STORED$tick[[which(STORED$tick$Ticker==symb)[1],'libelle']],id=symb, data = STORED$val[,symb],yAxis=0) %>%
                  hc_add_series(
                    data_flags, 
                    hcaes(x = date),
                    type = "flags", 
                    onSeries = symb
                  )
                
                hcc<-hcc %>% hc_add_series(name = STORED$tick[[which(STORED$tick$Ticker==symb)[1],'libelle']], data = STORED$ope[,symb],yAxis=1,type="line") 
                
                
              }
              
              hcc
            })

          output$values <- renderHighchart({
            reactiveHighchart1()
          })
          
          output$parti <- renderHighchart({
            reactiveHighchart2()
          })
          
          output$out <- DT::renderDataTable({
            STORED$tick %>% arrange(desc(quantité))
          })
      }
    )
}