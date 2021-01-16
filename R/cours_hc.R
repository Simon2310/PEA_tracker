#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
library(shiny)

suiviCourshcUI <- function(id, label = "Courshc") {
    ns <- NS(id)
        tagList(
            sidebarLayout(
    
           
              # Sidebar
              sidebarPanel(width = 2,
                      selectizeInput(ns("code"),
                                  "code :", choices=c("SPY","XOM"),options = list(create = TRUE), selected=NULL)
              ),
                      
                     # sliderInput(ns("periode"),"période",min=as.Date("1901-01-01"),max=Sys.Date(),value=c(as.Date("1901-01-01"),Sys.Date()))
                  
          
                  # Show a plot of the generated distribution
             mainPanel(
                  highchartOutput(ns("cours"),height = "900px"))
             )
            
           )
        
  
}

courshcServer <- function(id) {
    moduleServer(
        id,
        function(input, output,session) {
    
          dataInput <- reactive({
            req(input$code)
            data<-getSymbols(input$code, src = "yahoo",
                             auto.assign = FALSE) %>% na.approx()
            #data <- adjustOHLC(data)
            colnames(data)<- c("Open", "High", "Low", "Close", 
                               "Volume", "Adjusted")
            data$SMA.10 <- SMA(Cl(data), n = 5)
            data$SMA.200 <- SMA(Cl(data), n = 100)
            data$RSI.variation <- RSI(Cl(data))
            data$RSI.SellLevel <- xts(rep(70, NROW(data)), index(data))
            data$RSI.BuyLevel <- xts(rep(30, NROW(data)), index(data))
            
            
            return(data)
            
            
          })
          
          
          
          output$cours <- renderHighchart({
            
            highchart(type = "stock") %>% 
              # create axis :)
              hc_yAxis_multiples(create_yaxis(3, height = c(3, 1, 1), turnopposite = TRUE)) %>% 
              # series :D
              hc_add_series(dataInput(), yAxis = 0, name = input$code) %>% 
              hc_add_series(dataInput()$SMA.10, yAxis = 0, name = "Fast MA") %>% 
              hc_add_series(dataInput()$SMA.200, yAxis = 0, name = "Slow MA") %>% 
              hc_add_series(dataInput()$Volume, color = "gray", yAxis = 1, name = "Volume", type = "column") %>% 
              hc_add_series(dataInput()$RSI.variation, yAxis = 2, name = "Oscillator", color = hex_to_rgba("green", 0.7)) %>%
              hc_add_series(dataInput()$RSI.SellLevel, color = hex_to_rgba("red", 0.7), yAxis = 2, name = "Sell level") %>% 
              hc_add_series(dataInput()$RSI.BuyLevel, color = hex_to_rgba("blue", 0.7), yAxis = 2, name = "Buy level") %>% 
              hc_tooltip(valueDecimals = 2) #%>% 
              #shc_size(height = 800)
            
          })
        
        
      }
    )
}