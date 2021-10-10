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
               
         navbarMenu("Suivi PEA",
            tabPanel("Registre",
                    
        
                    registerFormUI("form", "Form #1")       
                   
                    ),
            
            tabPanel("test", testUI("test", "Test #1")),
            
            tabPanel("Suivi des cours",
                     
                    portefeuilleUI("portefeuille", "Portefeuille #1")       
                     
                    ),
            
            tabPanel("Suivi des plus-values",
                     
                     portefeuille_netUI("portefeuille_net", "Portefeuille_net #1")       
                     
            ),
            
            tabPanel("Analyse actif",
                     
                     suiviCourshcUI("courshc", "Courshc #1")       
                     
                    )
         )
    
  )

server <- function(input, output, session) {
  registerServer("form")
  portefeuilleServer("portefeuille")
  portefeuille_netServer("portefeuille_net")
  testServer("test")
  courshcServer("courshc")
}

shinyApp(ui, server)