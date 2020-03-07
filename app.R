# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)
library(shiny)
library(ggplot2)
library(data.table)
library(plotly)
library(shinythemes)
# Use a fluid Bootstrap layout
ui <- fluidPage(    
  theme = shinytheme("darkly"),
  # Give the page a title
  titlePanel("Data exploration"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      
      selectInput("num1", "Num1:", 
                  choices=num_feature),
      
      selectInput("num2", "Num2:", 
                  choices=num_feature),
      
      selectInput("factor", "Factor:", 
                  choices=colnames(diam[,sapply(diam,is.factor)])),
      
      
      
      sliderInput("span",
                  "Span:",
                  min = 0.3,
                  max = 0.9,
                  value =0.4),
      sliderInput("cor",
                  "cor",
                  min = 0.0001,
                  max = 0.9999,
                  value =0.7)
    ),
    mainPanel(
    plotlyOutput("phonePlot2") ,plotOutput("phonePlot"),plotOutput("phonePlot3"), plotOutput("phonePlot1")
    )
    
  ) 
  
  
)



#Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)

# Define a server for the Shiny app
server<- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$phonePlot <- renderPlot({
    library(ggcorrplot)
    library(caret)
    diam<-read.csv("E:/data/credit_risk_train.csv",header = TRUE)
    names(diam)[1] <-c('BILL_1') 
    num_feature <- colnames(diam[,sapply(diam,is.numeric)])
    corr1 <- cor(diam[,sapply(diam,is.numeric)])
    fC = findCorrelation(corr1, cutoff = input$cor)
    corr2 <- cor(diam[,sapply(diam,is.numeric)][,fC])
    ggcorrplot(corr2, hc.order = TRUE, outline.color = "white",lab = TRUE)
    
  })
  output$phonePlot1 <- renderPlot({
    require(ggridges)
    p <- ggplot(diam) + 
      aes(x =diam[,input$num1], y = diam[,input$factor], color = diam[,input$factor], fill = diam[,input$factor]) + 
      geom_density_ridges(alpha = 0.75) + 
      theme_ridges()
    p
    
  })
  output$phonePlot2 <- renderPlotly({
    p1 <- ggplot(diam,aes(x =diam[,input$num1], y = diam[,input$num2], color = diam[,input$factor])) + 
      geom_point()+ geom_smooth(span=input$span)
    ggplotly(p1)
    
  })
  output$phonePlot3 <- renderPlot({
    library(GGally)
    corr1 <- cor(diam[,sapply(diam,is.numeric)])
    fC = findCorrelation(corr1, cutoff = input$cor)
    dd <- diam[,sapply(diam,is.numeric)][,fC]
    dd$label <- diam$RISK
    ggscatmat(dd, columns = 1:(dim(dd)[2]-1),color='label')
  })

  
}


shinyApp(ui = ui, server = server)

