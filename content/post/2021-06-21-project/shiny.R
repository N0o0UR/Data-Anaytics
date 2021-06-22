# Load needed libraries:

library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
library(janitor)



# Read the data  and perform data Transformation:

main_dataset <- read_csv("Fish.csv") %>% 
  clean_names()%>%
  rename(vertical_length = length1, diagonal_length = length2, cross_length = length3)%>%
  select(species,weight,vertical_length,diagonal_length,cross_length,height,width)

# Removing any fish with 0 gram weight

main_dataset<-main_dataset[apply(main_dataset!=0, 1, all),]


# Defining the user interface UI 

ui <- fluidPage(
  titlePanel("Database of seven common fish species from fish market"),
  tags$div(class = "submit",
           fluidRow(column(12,wellPanel(p(" In this part of the exploratory data analysis we can inspect each measurment among the seven fish species avilable in our data set, the used data set was obtained from Kaggle website and it can be found in the this link")))),
           
           tags$a(href = "https://www.kaggle.com/aungpyaeap/fish-market",
                  "Link to the data set",
                  target="_blank")),
  fluidRow(column(12,wellPanel(p("Please select one of the measurments to explore it among all the seven speices ")))),

  sidebarLayout(
    sidebarPanel(
        selectInput(
        inputId = "measurment",
        label = "Select Measurment ",
        choices = c("weight", "vertical_length", "diagonal_length", "cross_length", "height", "width")
        )
   ),
    mainPanel(
      plotlyOutput(outputId = "plot")


    )
  )
)


# Defining the server

server <- function(input, output,session) {
  output$plot <- renderPlotly({
    p <- main_dataset %>%
      ggplot(aes_string(x="species",input$measurment,fill="species"))+
      geom_boxplot()+
      labs(title=glue("The {input$measurment} distribution for all the seven species \n "),subtitle = "", x="Species",y=glue("{input$measurment}"))+
      theme_bw()+
      theme(axis.text.x=element_text(color = "red", size=9, vjust=.5, hjust=0.7)) +
      theme(plot.background = element_rect(fill = "#BFD5E3"))+
      theme(plot.caption  = element_text(hjust = 0.5),plot.title = element_text(hjust = 0.5))

    ggplotly(p)

  })

}




# Shiny App


shinyApp(ui = ui, server = server)



