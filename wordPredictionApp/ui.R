
library(shiny)
shinyUI(fluidPage(
  
  h1("Next Word Prediction App - Seshadri K"),
  
  h2(""),
  
  # Application title
  h3("Start typing in the box below and wait for the predictions:"),
  
  textInput("inputText", "Input Text:",""),
  
  h3("Next Word suggestions:"),
  verbatimTextOutput("value")
))