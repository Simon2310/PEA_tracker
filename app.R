#lanceur principal
library(quantmod);
library(dplyr);
library(highcharter);
library(plotly);
library(DT);
library(vars);
library(rlist);
library(tidyr);
library(purrr);
library(tidyquant)


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
  a<-registerServer("form")
 # portefeuilleServer("portefeuille",reg_price)
  #portefeuille_netServer("portefeuille_net",reg_price)
  #courshcServer("courshc")
}

shinyApp(ui, server)