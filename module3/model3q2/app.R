#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(tidyverse)

q2 <- read.csv('https://raw.githubusercontent.com/Luz917/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')
q2state <-q2 %>%
          group_by(ICD.Chapter, Year)%>%
          mutate (Nat.Avg = sum(Deaths)* 100000/ sum(Population))


ui <- fluidPage(
    headerPanel('Mortality Rates By State Versus National Average'),
    sidebarPanel(
      selectInput('cause', 'Cause of Death', unique(q2state$ICD.Chapter), choices = q2state$ICD.Chapter),
      selectInput('state', "Choose State", unique(q2state$State), choices = q2state$State)  
    ),
    mainPanel(
        plotlyOutput('plot'),
        verbatimTextOutput('stats')
    )
)

server <- function(input, output, session) {
    
    
    
    output$plot <- renderPlotly({
        
        q2statesslice <- q2state %>%
            filter(ICD.Chapter== input$cause, State == input$state)
        
        
        ggplot(data = q2statesslice) + 
            aes( x= Year,y= Crude.Rate) +
            ylab("Rate")+  
            geom_bar (stat="identity")+
            theme_minimal()+
            theme(axis.text.x = element_text(size=8, angle=90))+
            geom_point(aes(x= Year, y=Nat.Avg), color="green")
          
     
        
    })
}

shinyApp(ui = ui, server = server)



