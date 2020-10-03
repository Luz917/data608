library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)

q1 <- read.csv('https://raw.githubusercontent.com/Luz917/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')
q12010 <- subset(q1,Year == 2010)

ui <- fluidPage(
    headerPanel('Mortality Rate'),
    sidebarPanel(
        selectInput('cause', 'Cause of Death', unique(q12010$ICD.Chapter), choices = q12010$ICD.Chapter)
    ),
    mainPanel(
        plotlyOutput('plot1'),
        verbatimTextOutput('stats')
    )
)

server <- function(input, output, session) {
    
    
    
    output$plot1 <- renderPlotly({
        
      q1slice <- q1 %>%
            filter(ICD.Chapter== input$cause, Year== 2010)
        
      q1slice$State <- factor(q1slice$State,levels=q1slice$State[order(q1slice$Crude.Rate,decreasing=TRUE)])
      
      ggplot(data = q1slice) + 
      aes( x= State,y= Crude.Rate) +
      ylab("Rate")+  
      geom_bar (stat="identity")+
        theme_minimal()+
        theme(axis.text.x = element_text(size=8, angle=90))
      
     
    })
}

shinyApp(ui = ui, server = server)



