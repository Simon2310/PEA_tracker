#lanceur principal

ui<-navbarPage("PEA",
    tabPanel("Ajout mouvement",

            registerFormUI("form", "Form #1")       
           
            ),
    
    tabPanel("Suivi portefeuille",
             
            portefeuilleUI("portefeuille", "Portefeuille #1")       
             
            ),
    
    tabPanel("Net investissement",
             
             portefeuilleUI("portefeuille_net", "Portefeuille_net #1")       
             
    ),
    
    tabPanel("Suivi actif HC",
             
             suiviCourshcUI("courshc", "Courshc #1")       
             
            )
    
  )

server <- function(input, output, session) {
  registerServer("form")
  portefeuilleServer("portefeuille")
  portefeuille_netServer("portefeuille_net")
  courshcServer("courshc")
}

shinyApp(ui, server)