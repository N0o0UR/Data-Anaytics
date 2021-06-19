# Load needed libraries:

library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
library(janitor)



# Read the data
main_dataset <- read_csv("Health.csv") %>% 
  clean_names()%>%
  drop_na()

# choices for the country
countries<- main_dataset%>%
  distinct(country_name)

# choices for the indicators
indicators<- main_dataset%>%
  distinct(indicator_name)


years=c(1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,
        1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,
        1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,
        1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,
        2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,
        2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
       


ui <- fluidPage(
  titlePanel("The World Bank Data for the Health as Indicator Among differrent Countries from 1960 to 2019"),
  tags$div(class = "submit",
           tags$a(href = "https://data.worldbank.org/topic/health?view=chart", 
                  "Link to the data set", 
                  target="_blank")),
  fluidRow(column(12,wellPanel(p("The data is composed of some indicators for the health for some countries.\n Please select the country and the indicator you are interested in to show a plot that represent the time series from 1960 to 2019 for the selected indicator in the selected country ")))),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country_name",
        label = "Select Country",
        choices = countries
      ),
      selectInput(
        inputId = "indicator_name",
        label = "Select Indicator",
        choices = indicators
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "plot")
      
      
    )
  )
# ,      fluidRow(column(12,wellPanel(p("QuestionS 1, 2, 3, and 4 can be answered through the interactive visualization shown below so that by picking the indicator of interest and the year, we can check the answers of those 4 questions and more:")))),
# sidebarLayout(
#   sidebarPanel(
#     selectInput(
#       inputId = "year",
#       label = "Select Year",
#        choices = years
# 
# 
#     ),
#     selectInput(
#       inputId = "indicator2_name",
#       label = "Select Indicator",
#       choices = indicators
#     )
#   ),
#   mainPanel(
#     plotlyOutput(outputId = "plot_2")
# 
# 
#   )
# )
)



server <- function(input, output) {
  output$plot <- renderPlotly({
    p <- main_dataset %>%
      filter(country_name == input$country_name) %>%
      filter(indicator_name == input$indicator_name) %>%
      select(-country_code,- indicator_code,-country_name, -indicator_name)%>%
      gather(key="year", value="value",x1960:x2019)%>%
      mutate(year=as.integer(str_replace(year, "x", "")))%>%
      ggplot(aes(x=year,y=value))+
      geom_line()+
      labs(title=glue("{input$country_name}'s {input$indicator_name} from 1960 to 2019 \n "),subtitle = "", x="Year",y="Population",caption = "Figure (1)")+
      theme_bw()+
      theme(axis.text.x=element_text(color = "red", size=9, vjust=.5, hjust=0.7)) +
      theme(plot.background = element_rect(fill = "#BFD5E3"))+
      theme(plot.caption  = element_text(hjust = 0.5),plot.title = element_text(hjust = 0.5))

    ggplotly(p)
    
  })
  # output$plot_2 <- renderPlotly({
  #   
  # 
  #   
  #   p2 <- main_dataset %>%
  #     select(-country_code,- indicator_code)%>%
  #     gather(key="year", value="value",x1960:x2019)%>%
  #     mutate(year=as.integer(str_replace(year, "x", "")))%>%
  #     filter(indicator_name == input$indicator2_name) %>%
  #     filter(year == input$year) %>%
  #     ggplot(aes(x=country_name,y=value))+
  #     geom_col()+
  #     labs(title=glue("{input$indicator2_name} from 1960 to 2019 \n "),subtitle = "", x="Year",y="Population",caption = "Figure (2)")+
  #     theme_bw()+
  #     theme(axis.text.x=element_text(color = "red", size=3, angle=90,vjust=.5, hjust=0.7)) +
  #     theme(plot.background = element_rect(fill = "#BFD5E3"))+
  #     theme(plot.caption  = element_text(hjust = 0.5),plot.title = element_text(hjust = 0.5))
  # 
  #   ggplotly(p2)
  # })
}




# Shiny App
shinyApp(ui = ui, server = server)



