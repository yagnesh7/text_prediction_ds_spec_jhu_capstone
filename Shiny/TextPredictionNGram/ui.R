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
  titlePanel("Text Prediction App"),
  
          p("Please insert your text below in the input field and wait for predictions!"),
          textAreaInput("predictionText", "Type Here", placeholder = "Insert text here"),
          br(),
          textOutput("predictedWords")

))

