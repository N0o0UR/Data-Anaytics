
# load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
library(dplyr)
library(janitor)
library(shinyWidgets)

# Read the data
main_data <- read_csv("Provisional_COVID-19_Death_Counts_in_the_United_States_by_County.csv")%>%
clean_names() # Taming the data

# tidy and select the needed variables
selected_var <- main_data%>%
  select("state","county_name","deaths_involving_covid_19","deaths_from_all_causes")
selected_var[is.na(selected_var)] <- 0  ## replace any NA with 0

  
#grouping data by state and gather the data 
  group_by_state <- selected_var%>% 
    group_by(state)%>% 
    summarise(all_causes=sum(deaths_from_all_causes),covid=sum(deaths_involving_covid_19))%>% 
    gather(key="caused_by",value="deaths_counts",all_causes:covid)
  
  # creating user interface UI
  ui<- fluidPage(
    titlePanel(h1("Deaths counts in the United States from 01/01/2020 to 06/09/2021")),
    tags$div(class = "submit",
             tags$a(href = "https://catalog.data.gov/dataset/provisional-covid-19-death-counts-in-the-united-states-by-county", 
                    "Link to the data set", 
                    target="_blank")
    ),
    
    # tags$a(href="https://catalog.data.gov/dataset/provisional-covid-19-death-counts-in-the-united-states-by-county", 
    #        "The link to the data set is avialable here, https://catalog.data.gov/dataset/provisional-covid-19-death-counts-in-the-united-states-by-county"),
    # fluidRow(shinydashboard::box( " To access the dataset source press on the Data button  ", 
    #                              shiny::actionButton(inputId='ab1', label="Data", 
    #                                                  icon = icon("th"), 
    #                                                  onclick ="window.open('https://catalog.data.gov/dataset/provisional-covid-19-death-counts-in-the-united-states-by-county')")
    # )),

    fluidRow(column(12,wellPanel(p("The figure below shows the deaths counts in each state of the united states  where  the orange represents the deaths from all causes and the blue represents the deaths from Covid 19 ")), mainPanel(plotlyOutput(outputId = "plot_1"))),
    fluidRow(column(12,wellPanel(p("To show the deaths counts per each county in a specific state please select the state name ")),
           sidebarLayout(sidebarPanel(pickerInput("state","State",choices=group_by_state$state, options = list(`actions-box` = TRUE),multiple = F)),         
           fluidRow(column(12, mainPanel(plotlyOutput(outputId = "plot")))))
           
      
    )
  )
          ))

  
  
  
  
  server <- function(input, output,session) {
    
    output$plot_1 <- renderPlotly({
      p<- group_by_state%>%
        ggplot(aes(state,deaths_counts))+
        geom_col(aes(fill = caused_by), position = "stack")+
        theme(axis.text.x=element_text(color = "black", size=9, angle=90))+
        ylim(0, 500000)+
        labs(title="                            Death Counts vs. state\n ", x="State",y="Number of deaths", fill="Reason of death") +
        scale_fill_discrete(labels = c("Not Covid", "Covid"))
      ggplotly(p)
      
    })
    
    
      output$plot <- renderPlotly({
      p<- selected_var%>%
        filter(state==input$state)%>%
        gather(key="caused_by",value="deaths_counts",deaths_involving_covid_19:deaths_from_all_causes)%>%
        ggplot(aes(county_name,deaths_counts,fill = caused_by))+
        geom_col()+
        theme(axis.text.x=element_text(color = "black", size=9, angle=90))+
        labs(title=glue("Deaths Counts in {input$state} state per county \n "), x="County",y="Number of deaths", fill="Reason of death") +
        scale_fill_discrete(labels = c("Not Covid", "Covid"))
      ggplotly(p)
      
    })
  }
  
  
  # Shiny App
  shinyApp(ui = ui, server = server)
  
  
  
