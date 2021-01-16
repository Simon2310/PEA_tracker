#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

registerFormUI <- function(id, label = "Form") {
    ns <- NS(id)
    tagList(
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(width=3,
                dateInput(ns("date"),
                            "Date de la transaction:",value=Sys.Date()),
                selectInput(ns("ordre"),"Nature de l'ordre",choices=c("achat","vente"),selected=NULL),
                selectizeInput(ns("libelle"),"Nom de l'actif:",choices=registre %>% distinct(libelle) %>% pull(libelle),options = list(
                  placeholder = 'Choisir un actif existant ou ajouter un nouvel actif:',
                  onInitialize = I('function() { this.setValue(""); }'),
                  create = TRUE),selected=""),
                textInput(ns("ticker"),"Ticker de l'actif:",value=""),
                selectizeInput(ns("type"),"Type de l'actif",choices=c("OPC","Tracker","action"),options = list(
                  placeholder = 'Catégorie de l\'actif',
                  onInitialize = I('function() { this.setValue(""); }'),
                  create = TRUE)),
                numericInput(ns("quantité"),"quantité:",value=0),
                numericInput(ns("cours"),"cours:",value=0),
                numericInput(ns("net_hors_TTF"),"Montant net hors TTF:",value=0),
                numericInput(ns("TTF"),"TTF:",value=0),
                actionButton(ns("save"),"enregistrer")
                         
            ),
    
            # Show a plot of the generated distribution
            mainPanel(

              DT::dataTableOutput(ns("out")),
              actionButton(ns("delete"),"supprimer la ligne selectionnée"),
              actionButton(ns("deleteconfirm"),"confirmer la suppression")
            )
        )
    )

}

registerServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
      
              
          registreReac<-reactiveValues(reg=registre,tick=tickers)
          
            observeEvent(
              input$libelle,
              { if (input$libelle %in% tickers$Nom) {
                updateSelectizeInput(session,"type",selected=registre[[which(registre$libelle==input$libelle)[1],'type']])
                updateTextInput(session,"ticker",value=tickers[[which(tickers$Nom==input$libelle)[1],'Ticker']])
              }
              }
            )
            
            observeEvent(
              input$cours,
              {
                updateNumericInput(session,"net_hors_TTF",value=input$cours*input$quantité)
              }
            )
            
            observeEvent(
              input$save, {
              
                registreReac$reg<-registreReac$reg %>% add_row(date=input$date,
                                    operation=input$ordre,
                                    libelle=input$libelle,
                                    Ticker=input$ticker,
                                    type=input$type,
                                    quantité=input$quantité,
                                    cours=input$cours,
                                    montant_hors_frais=input$quantité*input$cours,
                                    montant_net_hors_TTF=input$net_hors_TTF,
                                    TTF=input$TTF,
                                    montant_final=input$net_hors_TTF+input$TTF
                                    )
                registre<-registreReac$reg
                save(registre,file="base/transactions_test.rData")
                
                if (!(input$libelle %in% tickers$Nom)){
                  registreReac$tick<-registreReac$tick %>% add_row(Nom=input$libelle,Ticker=input$ticker)
                  tickers<-registreReac$tick
                  save(tickers,file="base/tickers.rData")
                }
                
                        }
                   )
            
            observeEvent(input$delete,{
              
              if (!is.null(input$out_rows_selected)) {
                
                registreReac$reg <- registreReac$reg[-as.numeric(input$out_rows_selected),]
              }
            })
            
            observeEvent(input$deleteconfirm,{
              registre<-registreReac$reg
              save(registre,file="base/transactions_test.rData")
            })
            
            output$out <- DT::renderDataTable({
              registreReac$reg
            })
            
        }
    )
}

