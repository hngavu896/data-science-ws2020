library(tidyverse)
library(e1071)
library(shiny)

#Load your model
model_svm <- model_svm <- readRDS('titanic.svm.rds')


ui <- fluidPage(
  
  #App Title
  titlePanel("Wie wahrscheinlich würden Sie überleben?"),
  
  #Siebar with Slider Input
  sidebarLayout(
    
    #Slider Panel input
    sidebarPanel(
      
      #Slider
      sliderInput(inputId = "pclass",
                  label = "Which class did you stay?",
                  min = 1,
                  max = 3,
                  value = 1),
      
      selectInput(inputId = "parch",
                  label = "Did you have children/parent with you?",
                  c("yes" = 1, "no" = 0)),
      
      numericInput(inputId = "age",
                   label = "How old were you?",
                   value = ""),
      
      actionButton("action", label = "Find out!")
      
    ),
    
    
    # Main panel for displaying out put
    mainPanel(
      
      #Output: Answer:
      tableOutput("table"),
      h4(textOutput("text"))
      
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$action, {
    pclass <- as.numeric(input$pclass)
    parch <- as.numeric(input$parch)
    age <- as.numeric(input$age)
    data <- data.frame(pclass,age,parch)
    result <- predict(model_svm, data, probability = TRUE)
    my_result <-data.frame(attr(result, "probabilities"))
    text_form <- paste("Your chance of living is", my_result$X1)
    
    output$table <-renderTable(my_result)
    output$text <- renderText(text_form)
  })
}

# Create ShinyApp 
shinyApp(ui,server)