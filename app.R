library(shiny)
library(tidyverse)
library(plotly)

options(scipen = 100)

create.table.monthly <- function(x1,x2,x3, x4,x5, algne2, panus2){
  
  test <- data.frame(aasta = 1:x3)
  test$intress <- rnorm(x3, mean = x2, sd = x5)
  test$panus <- x1 + 12*x4*(test$aasta-1)
  test$scenario <- "A"
  
  
  test2 <- data.frame(aasta = 1:x3)
  test2$intress <- test$intress
  test2$panus <- algne2 +12*panus2*(test$aasta-1)
  test2$scenario <- "B"
  
  for (i in 1:x3)
    
    if (i == 1){test$vaartus[i] <- test$panus[1]*(1+test$intress[i]/100)}
  else{test$vaartus[i] <- test$vaartus[i-1]*(1+test$intress[i]/100)+12*x4}
  
  for (i in 1:x3)
    
    if (i == 1){test2$vaartus[i] <- test2$panus[1]*(1+test2$intress[i]/100)}
  else{test2$vaartus[i] <- test2$vaartus[i-1]*(1+test$intress[i]/100)+12*panus2}
  
  
  test <- rbind(test, test2) %>% group_by(scenario)
  
  g <- ggplot(data = test, aes(x = aasta, y = vaartus, color = scenario, fill = scenario)) +
    geom_line(alpha = 0.7) +
    geom_ribbon(data = test[,-2], aes(ymin = 0, ymax = panus), alpha = 0.2) + # -2 is "Vaartus"
    
    
    xlab("Aastad") +
    ylab("Vaartus EUR")
  
  
  
  g <- ggplotly(g, tooltip = c("aasta", "vaartus", "panus"))
  
  
  
  
  
  return(g)
  
}

create.table.monthly(30000, 5, 40, 100, 2, 20000, 100)

ui <- fluidPage(


  
sidebarPanel(sliderInput("slider1", "Mitmeks aastaks ma investeerin", min = 3, max = 50, step = 1, value = 30), 
             sliderInput("slider2", "Keskmine aastane tootlus", min = 2, max = 15, step = 0.1, value = 7),
             sliderInput("slider4", "Turu volatiivus %", min = 0, max = 10, step = 0.1, value = 2),
),


sidebarPanel("Scenario A",
  numericInput("input1", "Algne investeering", value = 5000),
             
             sliderInput("slider3", "Igakuine panus", min = 0, max = 400, step = 10, value = 100)
             ),

sidebarPanel("Scenario B",
  numericInput("algne2", "Algne investeering", value = 5000),
             sliderInput("panus2", "Igakuine panus", min = 0, max = 400, step = 10, value = 100)
  
),

mainPanel(plotlyOutput("raha"))

)


server <- function(input, output, session){
    output$raha <- renderPlotly({create.table.monthly(input$input1, input$slider2, input$slider1, input$slider3, input$slider4, input$algne2, input$panus2)})
}

shinyApp(ui, server)










