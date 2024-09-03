####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)

# Read data
weather <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv"))

# Convert the target variable 'play' to a factor for classification
weather$play <- as.factor(weather$play)

# Convert predictors to the correct types
weather$outlook <- factor(weather$outlook, levels = c("overcast", "rainy", "sunny"))
weather$windy <- as.logical(weather$windy)

# Build the random forest model for classification
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('Play Golf?'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
                  selectInput("outlook", label = "Outlook:", 
                              choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                              selected = "Rainy"),
                  sliderInput("temperature", "Temperature:",
                              min = 64, max = 86,
                              value = 70),
                  sliderInput("humidity", "Humidity:",
                              min = 65, max = 96,
                              value = 90),
                  selectInput("windy", label = "Windy:", 
                              choices = list("Yes" = TRUE, "No" = FALSE), 
                              selected = TRUE),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # Collect input data
    test <- data.frame(
      outlook = factor(input$outlook, levels = levels(weather$outlook)),  # Set factor levels to match training data
      temperature = as.numeric(input$temperature),
      humidity = as.numeric(input$humidity),
      windy = as.logical(input$windy),
      stringsAsFactors = FALSE
    )
    
    # Make predictions
    Output <- data.frame(Prediction = predict(model, test), 
                         Probability = round(predict(model, test, type = "prob"), 3))
    return(Output)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton > 0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
