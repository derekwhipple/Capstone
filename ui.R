#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Text Prediction Application"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("inputText", NULL, placeholder = "Enter text to predict the next ..."),
            submitButton("Predict")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("predictedLabel"),
            textOutput("predictedWord"),
            tags$head(tags$style("#predictedWord{color: black;
                                                 font-size: 28px;
                                                 font-style: bold;
                                                }"
                                 )
                      )
        )
    )
))
