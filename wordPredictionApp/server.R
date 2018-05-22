library(shiny)

freq_table_1grams <- readRDS("freq_table_1grams.RDS")
freq_table_2grams <- readRDS("freq_table_2grams.RDS")
freq_table_3grams <- readRDS("freq_table_3grams.RDS")
freq_table_4grams <- readRDS("freq_table_4grams.RDS")

source("functions.R",local=TRUE)

server <- function(input, output, session) {
  
  output$value <- renderText({paste(getPredictions(input$inputText))})
  
}