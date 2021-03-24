#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


load("~/R/projets/PEA_tracker/base/transactions_test.rData")

load("~/R/projets/PEA_tracker/base/tickers.rData")

registerFormUI <- function(id, label = "form") {
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
              actionButton(ns("load_prices"),"Charger les cours"),
              textOutput(ns("txt")),
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
      
              
          registreReac<-reactiveValues(val_agr=NULL,
                                       val_net = NULL,
                                       val = NULL,
                                       ope = NULL,
                                       reg=registre,tick=tickers)
          
          suivi<-reactiveValues(texte="...")
          
            observeEvent(
              input$libelle,
              { if (input$libelle %in% tickers$Nom) {
                updateSelectizeInput(session,"type",selected=registreReac$reg[[which(registreReac$reg$libelle==input$libelle)[1],'type']])
                updateTextInput(session,"ticker",value=registreReac$tick[[which(registreReac$tick$Nom==input$libelle)[1],'Ticker']])
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
            
            output$txt <- renderText({
             suivi$texte
            })
            

            
            observeEvent (input$load_prices,{
              
              histo_port<-registreReac$reg %>% dplyr::select(date,quantité,Ticker,operation) %>% mutate(abs=case_when(operation=="achat" ~ quantité,operation=="vente" ~ -quantité )) %>% 
                spread(key=Ticker,value=abs) %>%
                group_by(date) %>%
                summarise_at(vars(-operation,-quantité),funs(sum(., na.rm = TRUE))) %>%
                mutate_if(is.numeric,cumsum)
              #génération de inv_port à la volée à partir du registre: par actif, somme cumulative des montants investis (positif à l'achat, négatif à la vente)
              inv_port<-registreReac$reg %>% dplyr::select(date,montant_final,Ticker,operation) %>% mutate(abs=case_when(operation=="achat" ~ montant_final,operation=="vente" ~ -montant_final )) %>% 
                spread(key=Ticker,value=abs) %>%
                group_by(date) %>%
                summarise_at(vars(-operation,-montant_final),funs(sum(., na.rm = TRUE))) %>%
                mutate_if(is.numeric,cumsum)
              
              #passage en xts des df
              histo_port<-xts(histo_port%>% dplyr::select(-c("date")),order.by=as.Date(histo_port$date)) 
              inv_port<-xts(inv_port%>% dplyr::select(-c("date")),order.by=as.Date(inv_port$date)) 
              
              
              
              #on prend la liste des actifs correspondant au filtre utilisateur (OPC, ETF, action ..)
              list_label<-(registreReac$reg %>% distinct(Ticker))
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
              
              categories<-registreReac$reg %>% distinct(type) %>% pull(type)
              
              
              for (cat in categories){
                names<-colnames(valeur_agr)
                elt<-registreReac$reg %>% filter(type==cat) %>% distinct(Ticker) %>% pull(Ticker)
                new<-valeur_agr[,elt] %>% rowSums() %>% xts(order.by = index(valeur_agr))
                new_inv<-valeur_net_inv[,elt] %>% rowSums() %>% xts(order.by = index(valeur_net_inv))
                valeur_agr<-merge(valeur_agr,new)
                valeur_net_inv<-merge(valeur_net_inv,new_inv)
                colnames(valeur_agr)<-c(names,cat)
                colnames(valeur_net_inv)<-c(names,cat)
                
              }
              
              registreReac$val_agr=valeur_agr
              registreReac$val_net_inv=valeur_net_inv
              registreReac$val=valeurs
              registreReac$ope=operations
              
              suivi$texte="done"
            })
            
            
            return(regReac=registreReac)
              
            
        }
       
    )
}

