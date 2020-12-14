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

antibias<- read.csv("https://raw.githubusercontent.com/Luz917/data608/master/data608Final/bias_incidents.csv")

yearcount <- antibias %>%
    group_by(Year, Bias.Code)%>%
    count()
yearcount <- yearcount %>%
    group_by(Year) %>%
    mutate(Bias_Percent = n/sum(n))


districtcount <- antibias %>%
    group_by(District, Bias.Code, Year)%>%
    count()
districtcount <- districtcount %>%
    group_by(District) %>%
    mutate(District_Percent = n/sum(n))
districtcount

typebiascount <- antibias %>%
  group_by(Bias, Bias.Code,Year)%>%
  count()
typebiascount <- typebiascount %>%
  group_by(Bias) %>%
  mutate(Bias_Type_Percent = n/sum(n))


typecount <- antibias %>%
  group_by(Victim.Type, Bias.Code,Year)%>%
  count()
typecount <- typecount %>%
  group_by(Victim.Type) %>%
  mutate(Type_Percent = n/sum(n))



ui<-( fluidPage(
  headerPanel('Bias Incidents'),
  
  tabPanel("year",
                             
      sidebarPanel(
          selectInput('Year','Select a Year', unique(yearcount$Year), choices = yearcount$Year)
                                      
                      )),
          mainPanel(plotlyOutput("plot1")),
          
     tabPanel("district",
                              
          sidebarPanel( 
                                        
          selectInput('District', 'Select a District', unique(districtcount$District), choices = districtcount$District)
                             )),
          mainPanel(plotlyOutput("plot2")),
                
      tabPanel("bias",
                   
          sidebarPanel(
          selectInput('Bias','Select an Act of Violence', unique(typebiascount$Bias), choices = typebiascount$Bias)
                     
                   )),
                   mainPanel(plotlyOutput("plot3")), 
          
          
                   tabPanel("typevictim",
                            
                            sidebarPanel(
                              selectInput('Victim.Type','Select a Type of Victim', unique(typecount$Victim.Type), choices = typecount$Victim.Type)
                              
                            )),
                            mainPanel(plotlyOutput("plot4")),
          
 
             
    
    
         
    )
)
    
    server<-(function(input, output) {
        output$plot1 <- renderPlotly({
            q2statesslice <- yearcount %>%
                filter( Year == input$Year)
            
            ggplot(q2statesslice, aes(fill=Bias.Code, y=Bias_Percent , x=Year)) + 
                geom_bar(position="dodge", stat="identity") +
                ggtitle("Year vs Bias.Code") +
                theme_void()+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
            
            
        })
        
        output$plot2 <- renderPlotly({
            q2statesslice1 <- districtcount %>%
                filter(District == input$District, Year == input$Year)
            
            ggplot(q2statesslice1, aes(fill=Bias.Code, y=District_Percent , x=District)) + 
                geom_bar(position="dodge", stat="identity") +
                ggtitle("District Vs. Bias.Code") +
                theme_void()+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
            
        })
        
        output$plot3 <- renderPlotly({
          q2statesslice2 <- typebiascount %>%
            filter(Bias == input$Bias, Year == input$Year)
          
          ggplot(q2statesslice2, aes(fill=Bias.Code, y=Bias_Type_Percent , x=Bias)) + 
            geom_bar(position="dodge", stat="identity") +
            ggtitle("Act of Violence vs Bias.Code") +
            theme_void()+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
        })
        
        output$plot4 <- renderPlotly({
          q2statesslice3 <- typecount %>%
            filter(Victim.Type == input$Victim.Type, Year == input$Year)
          
          ggplot(q2statesslice3, aes(fill=Bias.Code, y=Type_Percent , x=Victim.Type)) + 
            geom_bar(position="dodge", stat="identity") +
            ggtitle("Type of Victim vs Bias.Code") +
            theme_void()+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
        })
        
        output$plot5 <- renderPlotly({
          ggplot(yearcount, aes(fill=Bias.Code, y=Bias_Percent , x=Year)) + 
            geom_point(aes(col=Bias.Code, size=Bias_Percent)) +
            ggtitle("Year vs Bias.Code") +
            theme_minimal()
        })
        
    })
shinyApp(ui = ui, server = server)


                 

