#UI for shiny App

suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringr))
suppressMessages(library(tidytext))
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))

source("predictor.R")

ui <- fluidPage(
  
  # Application title
  dashboardHeader(title = span("Coursera Final Project: Text Predictor", style = "color: purple; font-size: 36px")),
  h3("This app takes an user entered phrase and then predicts the next word."),
  
  # Sidebar with notes
  sidebarLayout(
    sidebarPanel(
      h3("Enter a word or phrase into the textbox on the right"),
      h3("The algorithm will predict the most likely following word in the phrase"),
      br()
    ),
  # Input and output for text prediction
    mainPanel(
          h2("Word Predictor"),
          textInput("inputString", h3("Please enter a phrase:")),
          h3("Predicted Next Word based on above phrase:"),
          h3(em(span(textOutput("prediction"), style="color:red"))
      )   
    )
  )
)
