## Add text to homepage describing process
## fix color on box
## update table util


library(dplyr)
library(shiny)
library(leaflet)

Distances <- readRDS("Distances.rds")
LibDataDisplay <- readRDS("LibDataDisplay.rds")
LibNames <- readRDS("LibNames.rds")


shinyServer(function(input, output) {
  
  ## input controls
  
  LibsInState <- reactive({
    
    
    
    LibNames %>% filter(State == input$StateSelect) %>%
      select(`Library Name`)
    
  })
  
  output$inputLibList <- renderUI({
    
    selectInput("LibSelect", label = "Select a library: ",
                choices = LibsInState()$`Library Name`)
    
  })
  
  ## output controls
  
  LibSelected <- reactive({
    
    LibNames %>% filter(`Library Name` == input$LibSelect & 
                          State == input$StateSelect) %>% select(rowname)
    
  })
  
  PeerGroup <- reactive({
    
    validate(
      need(input$LibSelect != "", "")
    )
    
    LibDataDisplay %>% filter(rowname == LibSelected()$rowname) %>%
      bind_rows(
        Distances %>% filter(rowname == LibSelected()$rowname) %>% arrange(rank) %>% select(rank, rownameOf2ndLib) %>%
          left_join(LibDataDisplay, by = c("rownameOf2ndLib" = "rowname"))
      ) %>% 
      mutate(Ranking = ifelse(is.na(rank),0,rank)) %>% 
      select(Ranking, `Library Name`, State, `Belongs to a Cooperative`, `Population of Legal Service Area`, `# of Branch Libraries`,
             `# of Bookmobiles`, `Total Staff Count`, `Total Income`, `Hours Open`, `Total Circulation`, `Total Programs`,
             `Local Gvt Funding Percentage`, `Children's Circulation Share of Total`, `Children's Programming Share of Total`)
    
    
    
  })
  
  output$table <- renderDataTable(PeerGroup(), options = list(paging = FALSE, searching = FALSE, pageLength = 26))
  
  PeerGroupMap <- reactive({
    
    
    validate(
      need(input$LibSelect != "", "")
    )
    
    
    LibDataDisplay %>% filter(rowname == LibSelected()$rowname) %>%
      bind_rows(
        Distances %>% filter(rowname == LibSelected()$rowname) %>% select(rank, rownameOf2ndLib) %>%
          left_join(LibDataDisplay, by = c("rownameOf2ndLib" = "rowname"))
      )
    
  })
  
  output$map <- renderLeaflet({
    
    
    leaflet() %>% addTiles() %>%
      addMarkers(data = PeerGroupMap(), popup = ~as.character(PopupText))
  })
  
})