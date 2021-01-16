#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
library(shiny)

suiviCoursUI <- function(id, label = "Cours") {
    ns <- NS(id)
        tagList(
            fluidPage(
    
           
            # Sidebar
            sidebarLayout(
                sidebarPanel(
                    selectInput(ns("code"),
                                "code :", choices=c("SPY","XOM"),selected=NULL),
                    
                   # sliderInput(ns("periode"),"période",min=as.Date("1901-01-01"),max=Sys.Date(),value=c(as.Date("1901-01-01"),Sys.Date()))
                ),
        
                # Show a plot of the generated distribution
                mainPanel(
                    plotlyOutput(ns("cours"))
                )
            )
        )
    )
}

coursServer <- function(id) {
    moduleServer(
        id,
        function(input, output,session) {
    
        dataInput <- reactive({
            data<-getSymbols(input$code, src = "yahoo",
                             auto.assign = FALSE)
            #updateSliderInput(session,"periode",min=start(data),max=Sys.Date(),value=c(Sys.Date()-365,Sys.Date()))
            datafr<-data.frame(Date=index(data),coredata(data))
            colnames(datafr)<- c("Date","Open", "High", "Low", "Close", 
                                 "Volume", "Adjusted")
            
            # create Bollinger Bands
            bbands <- BBands(data[,c(paste(input$code,".High",sep=""),paste(input$code,".Low",sep=""),paste(input$code,".Close",sep=""))])
            
            df <- data.frame(datafr, data.frame(bbands[,1:3])) %>%
                mutate(direction=ifelse(Close-Open>=0,"Increasing","Decreasing"))
            
            # plot candlestick chart
            i <- list(line = list(color = '#17BECF'))
            d <- list(line = list(color = '#7F7F7F'))
            fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                                  open = ~Open, close = ~Close,
                                  high = ~High, low = ~Low, name = input$code,
                                  increasing = i, decreasing = d) 
            fig <- fig %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                                     line = list(color = '#ccc', width = 0.5),
                                     legendgroup = "Bollinger Bands",
                                     hoverinfo = "none", inherit = F) 
            fig <- fig %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                                     line = list(color = '#ccc', width = 0.5),
                                     legendgroup = "Bollinger Bands", inherit = F,
                                     showlegend = FALSE, hoverinfo = "none") 
            fig <- fig %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                                     line = list(color = '#E377C2', width = 0.5),
                                     hoverinfo = "none", inherit = F) 
            fig <- fig %>% layout(yaxis = list(title = "Price",autorange=TRUE,fixedrange=FALSE))
            
            # plot volume bar chart
            fig2 <- df 
            fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = "Volume",
                                     color = ~direction, colors = c('#17BECF','#7F7F7F')) 
            fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))
            
            # create rangeselector buttons
            rs <- list(visible = TRUE, x = 0.5, y = -0.055,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 9),
                       buttons = list(
                           list(count=1,
                                label='RESET',
                                step='all'),
                           list(count=1,
                                label='1 YR',
                                step='year',
                                stepmode='backward'),
                           list(count=1,
                                label='1 MO',
                                step='month',
                                stepmode='backward'),
                           list(count=7,
                                label='1 week',
                                step='day',
                                stepmode='backward')
                       ))
            
            # subplot with shared x axis
            fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
                           shareX = TRUE, titleY = TRUE)
            fig <- fig %>% layout(title = paste(input$code),
                                  xaxis = list(rangeselector = rs),
                                  legend = list(orientation = 'h', x = 0.5, y = 1,
                                                xanchor = 'center', yref = 'paper',
                                                font = list(size = 10),
                                                bgcolor = 'transparent'))
            
            return(fig)
            
        })
        
        
        
        output$cours <- renderPlotly({
            
            #candleChart(dataInput(),theme=chartTheme('white'),subset=paste(input$periode[1],input$periode[2],sep="::"))
            
            dataInput()
        })
        
        
      }
    )
}