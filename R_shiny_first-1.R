library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)
df <- read_csv("Desktop/World Energy Consumption.csv")
Energy <- df[,c('country', 
                'year',
                'biofuel_electricity', 
                'hydro_electricity',
                'nuclear_electricity',
                'solar_electricity',
                'wind_electricity', 
                'other_renewable_electricity',
                'coal_electricity',
                'gas_electricity',
                'oil_electricity')]
View(Energy)
Energy$total <- rowSums(Energy[,3:11])
Energy <- Energy[-which(is.na(Energy$total)),]
#Energy1<-Energy%>%select(c(1,2,3,4,5,6,7,8,9,10,11,12,13))
View(Energy)
Energy1<-melt(Energy,id = c("country",'year'))
View(Energy1)
ui<-fluidPage(
  titlePanel(title = "Energy Consumption"),
  sidebarLayout(sidebarPanel(h3("Energy"),
                             selectInput("Country","Enter name of Country",choices = c(unique(Energy1$country)))),
                mainPanel(h4("Energy Consumption over timeperiod"),
                          plotOutput("line")
                          )
                )
)
server<-function(input,output){
  output$line<-renderPlot({
    
    Energy1%>%filter(Energy1$country==input$Country)%>%ggplot(aes(year,value,color=variable))+geom_line()
    
    
  })
}

shinyApp(ui,server)
