


# Things to do:  
## add map
## control selection with row number instead of name - see Denver example
## Add text to homepage describing process
## test other grading scripts


library(dplyr)
library(shiny)

shinyServer(function(input, output) {

  
  # temp <- Distances %>% filter(rowname == 1) %>% select(rownameOf2ndLib)
  # 
  # View(LibData %>% filter(rowname %in% temp$rownameOf2ndLib | rowname == "1"))


    
## input controls
    
  LibsInState <- reactive({
    
    LibNames %>% filter(State == input$StateSelect) %>%
      select(`Library Name`)
    
  })
  
  output$inputLibList <- renderUI({
    
    selectInput("LibSelect", label = "Select a library: ",
                choices = LibsInState())
    
  })

## output controls

  LibSelected <- reactive({
    
    LibNames %>% filter(`Library Name` == input$LibSelect) %>% select(rowname)
    
  })
      
  PeerGroup <- reactive({
    
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
  
  # LibDataTable <- reactive({
  #   
  #   LibDataDisplay %>% 
  #   
  #   
  # })
  
  
})
