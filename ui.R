
library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("lumen"),

  # Application title
  navbarPage("Public Library Peer Groups",
  # Sidebar with a slider input for number of bins
  tabPanel("Library Info",
  fluidPage(      
    fluidRow(  
      column(3, style = "background-color:#e3e6e8;",br(),
      selectInput("StateSelect", label = "Select a State: ", choices = LibNames %>% select(State) %>% unique() %>% arrange(State)),br(),
      uiOutput("inputLibList"),br(),br(),br(),br(),br(),br()
      ),
      column(9,
             p("map location")
      )
    ),
    fluidRow(
      br(),
      dataTableOutput('table')
    )
  )
  ),
  tabPanel("About the Project",
           
           p("Text to come")
           )
  
  )
))
