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

antibias<- read.csv("https://raw.githubusercontent.com/Luz917/data608/master/MCPD_Bias_Incidents.csv")
yearcount <- anitbias %>%
    group_by(Year, Bias.Code)%>%
    count()
yearcount <- yearcount %>%
    group_by(Bias.Code) %>%
    mutate(Bias_Percent = n/sum(n))



ui <- fluidPage(
    headerPanel('Year Versus Bias Code'),
    sidebarPanel(
        selectInput('Year', 'Year', unique(yearcount$Year), choices = yearcount$Year),
     
    ),
    mainPanel(
        plotlyOutput('plot'),
        verbatimTextOutput('stats')
    )
)

server <- function(input, output, session) {
    
    
    
    output$plot <- renderPlotly({
        
        q2statesslice <- yearcount %>%
            filter(Year == input$Year)
        
        ggplot(q2statesslice, aes(fill=Bias.Code, y=Bias_Percent , x=Year)) + 
            geom_bar(position="dodge", stat="identity") +
            ggtitle("Year vs Bias.Code") +
            theme_minimal()+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
        
        
        
        
    })
}

shinyApp(ui = ui, server = server)
