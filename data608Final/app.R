
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(tidyverse)
library(kableExtra)
library(shinythemes)

antibias<- read.csv("https://raw.githubusercontent.com/Luz917/data608/master/data608Final/bias_incidents.csv")

yearcount <- antibias %>%
    group_by(Year, Bias.Code, Bias,Victim.Type)%>%
    count()

## Bar Chart
yearcount11 <- antibias %>%
    group_by(Year, Bias.Code)%>%
    count()
yearcount11 <- yearcount11 %>%
    group_by(Year) %>%
    mutate(Bias_Percent =((n/sum(n))*100))

districtcount <- antibias %>%
    group_by(District, Bias.Code,Year)%>%
    count()
districtcount <- districtcount %>%
    group_by(District) %>%
    mutate(District_Percent = ((n/sum(n))*100))
districtcount

typebiascount <- antibias %>%
    group_by(Bias, Bias.Code,Year)%>%
    count()
typebiascount <- typebiascount %>%
    group_by(Bias) %>%
    mutate(Bias_Type_Percent = ((n/sum(n))*100))


typecount <- antibias %>%
    group_by(Victim.Type, Bias.Code,Year)%>%
    count()
typecount <- typecount %>%
    group_by(Victim.Type) %>%
    mutate(Type_Percent = ((n/sum(n))*100))



## Geom Point and Facet Wrap

districtcount12 <- antibias %>%
    group_by(District, Bias.Code)%>%
    count()
districtcount12 <- districtcount12 %>%
    group_by(District) %>%
    mutate(District_Percent = ((n/sum(n))*100))
districtcount12

typebiascount12 <- antibias %>%
    group_by(Bias, Bias.Code)%>%
    count()
typebiascount12 <- typebiascount12 %>%
    group_by(Bias) %>%
    mutate(Bias_Type_Percent = ((n/sum(n))*100))


typecount12 <- antibias %>%
    group_by(Victim.Type, Bias.Code)%>%
    count()
typecount12 <- typecount12 %>%
    group_by(Victim.Type) %>%
    mutate(Type_Percent = ((n/sum(n))*100))


## Table

yearcount45 <- antibias %>%
    group_by(Bias.Code, Year, Bias,Victim.Type)%>%
    count()



    ui = tagList(
        
        navbarPage(
            theme = shinythemes::shinytheme("darkly"),
            "shinythemes",
            tabPanel("Bias Incidents",
                     sidebarPanel(
                         h1("Bias Incidents Bar Chart Selector"),
                         h4("Each outout is reflected by the year chossen."),
                         selectInput('Year','Select a Year', unique(yearcount$Year), choices = yearcount$Year),
                         selectInput('District', 'Select a District', unique(districtcount$District), choices = districtcount$District),
                         selectInput('Bias','Select an Act of Violence', unique(typebiascount$Bias), choices = typebiascount$Bias),
                         selectInput('Victim.Type','Select a Type of Victim', unique(typecount$Victim.Type), choices = typecount$Victim.Type)
                         ),
                         
                         
                     mainPanel(
                         tabsetPanel(
                             
                             tabPanel("Bar Chart",
                                      h1("Bias Incidents"),
                                      h3("By Year"),
                                      plotlyOutput("plot1"),
                                      h3("By District"),
                                      plotlyOutput("plot2"),
                                      h3("By Acts of Violence"),
                                      plotlyOutput("plot3"),
                                      h3("By Type of Victim"),
                                      plotlyOutput("plot4")),
                                      
                             
                             tabPanel("Bias Incidents Table By Year",
                                      h2("Bias Incidents Table By Year"),
                                      htmlOutput("table")),
                                      
                                      
                             tabPanel("Bias Incidents Table By Bias.Code", 
                                      h2("Bias Incidents Table By Bias.Code"),
                                      htmlOutput("table1"))
                                      
                         )
                     )
            ),
            tabPanel("Geom Point",
                                 h2("Year vs Bias.Code"),
                                 plotlyOutput("plot5"),
                                 h2("District vs Bias.Code"),
                                 plotlyOutput("plot6"),
                                 h2("Act of Violence vs Bias.Code"),
                                 plotlyOutput("plot7"),
                                 h2("Victim.Type vs Bias.Code"),
                                 plotlyOutput("plot8") ),
            
            tabPanel("Bar Plot Facet Wrap",
                     h2("Year vs Bias.Code"),
                     plotlyOutput("plot9"),
                     h2("District vs Bias.Code"),
                     plotlyOutput("plot10"),
                     h2("Act of Violence vs Bias.Code"),
                     plotlyOutput("plot11"),
                     h2("Victim.Type vs Bias.Code"),
                     plotlyOutput("plot12") 
        )
    )
)   
    server<-(function(input, output) {
        output$plot1 <- renderPlotly({
            q2statesslice <- yearcount11 %>%
                filter( Year == input$Year)
            
            ggplot(q2statesslice, aes(fill=Bias.Code, y=Bias_Percent , x=Year)) + 
                geom_bar(position="dodge", stat="identity") +
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
                theme_void()+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
        })
        output$plot5 <- renderPlotly({
            ggplot(yearcount11, aes(fill=Bias.Code, y=Bias_Percent , x=Year)) + 
                geom_point(aes(col=Bias.Code, size=Bias_Percent)) +
                
                theme_minimal()
        })
        output$plot6 <- renderPlotly({
            ggplot(districtcount12, aes( y=District_Percent , x=District)) + 
                geom_point(aes(col=Bias.Code, size=District_Percent)) + 
                theme_minimal()
        })
        output$table <- renderText({
            kable(yearcount) %>%
                kable_styling(
                    font_size = 20,
                    bootstrap_options = c("striped", "hover", "condensed")
                ) 
       
        }) 
        output$plot7 <- renderPlotly({
            ggplot(typebiascount12, aes(fill=Bias.Code, y=Bias_Type_Percent , x=Bias)) + 
                geom_point(aes(col=Bias.Code, size=Bias_Type_Percent)) + 
                theme_minimal()+
                theme(axis.text.x = element_text(angle = 50, hjust=1))
        })
        output$plot8 <- renderPlotly({
            ggplot(typecount12, aes(fill=Bias.Code, y=Type_Percent  , x=Victim.Type)) + 
                geom_point(aes(col=Bias.Code, size=Type_Percent )) +
                theme_minimal()+
                theme(axis.text.x = element_text(angle = 50, hjust=1))
        })
        output$table1 <- renderText({
            kable(yearcount45) %>%
                kable_styling(
                    font_size = 20,
                    bootstrap_options = c("striped", "hover"))
                    
                
        })
        output$plot9 <- renderPlotly({
            ggplot(yearcount11, aes(fill=Bias.Code, y=Bias_Percent , x=Year)) + 
                geom_bar(position="dodge", stat="identity") +
                coord_flip()+
                theme_minimal()+
                facet_wrap(~Bias.Code)+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())
    })
        output$plot10 <- renderPlotly({
            ggplot(districtcount12, aes(fill=Bias.Code, y=District_Percent , x=District)) + 
                geom_bar(position="dodge", stat="identity") +
                ggtitle("District vs Bias.Code") +
                theme_minimal()+
                coord_flip()+
                facet_wrap(~Bias.Code)+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())
        })
        output$plot11 <- renderPlotly({
            ggplot(typebiascount12, aes(fill=Bias.Code, y=Bias_Type_Percent , x=Bias)) + 
                geom_bar(position="dodge", stat="identity") +
                coord_flip()+
                theme_minimal()+
                facet_wrap(~Bias.Code)+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())
        })
        output$plot12 <- renderPlotly({
            ggplot(typecount12, aes(fill=Bias.Code, y=Type_Percent , x=Victim.Type)) + 
                geom_bar(position="dodge", stat="identity") +
                coord_flip()+
                theme_minimal()+
                facet_wrap(~Bias.Code)+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(),
                      axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())
        }) 
})      
    shinyApp(ui = ui, server = server)
    
    
    
    
    
