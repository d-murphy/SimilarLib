library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)


shinyUI(fluidPage(theme = shinytheme("lumen"),
                  
                  # Application title
                  navbarPage("Public Library Peer Groups",
                             # Sidebar with a slider input for number of bins
                             tabPanel("Library Info",
                                      fluidPage(      
                                        fluidRow(  
                                          column(3, style = "background-color:#e3e6e8;",br(),
                                                 p("Use the controls to select a library.  The library and 25 of its statistical peers appear on the map and in the table below."),
                                                 br(),br(),
                                                 selectInput("StateSelect", label = "Select a State: ", choices = LibNames %>% select(State) %>% unique() %>% arrange(State)),br(),
                                                 uiOutput("inputLibList"),br(),br(),br()
                                          ),
                                          column(9,
                                                 leafletOutput("map")
                                          )
                                        ),
                                        fluidRow(
                                          br(),
                                          dataTableOutput('table')
                                        )
                                      )
                             ),
                             tabPanel("About the Project",
                                      
                                      tags$div(
                                        
                                        HTML("<p>This web app utilizes data from the 
                                             <a href = 'https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey/explore-pls-data/pls-data'>
                                             2016 Public Libraries Survey</a>
                                             completed by the Institute of Museum and Library Services.  
                                             </p>")
                                        ),
                                      br(), p("The app was created by Dan Murphy.  If you have any comments on the project or 
                                              have ideas for similar work, contact Dan at:  murphy.dan.s@gmail.com.")
                                      )
                             
                                        )
                  ))