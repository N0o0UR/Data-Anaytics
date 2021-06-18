
# load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
library(dplyr)
library(janitor)
library(shinyWidgets)


# creating user interface UI
ui<- fluidPage(
  tags$div(class = "submit",
           tags$a(href = "file:///C:/Users/noura/Documents/Data-Anaytics/content/post/2021-06-16-a05-exploratory-data-analysis/report.html", 
                  "Link to the exploratory data analysis report", 
                  target="_blank")
  ))





server <- function(input, output,session) {}


# Shiny App
shinyApp(ui = ui, server = server)

