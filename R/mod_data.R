#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
        shinydashboard::valueBoxOutput(ns("no.users")),
        shinydashboard::valueBoxOutput(ns("avg.opti")),
        shinydashboard::valueBoxOutput(ns("avg.overall")),
      
        shinydashboard::box(width = 6,
            HTML("<h3>How it works?</h3>
            <p>The algorithm matches people into pairs to maximize the score of overall population.
            Score is measured by the number of questions you and other person answered the same.</p>
            <p>For example, let's image that the population consists of four people: Alice, Ben, Charlie and Dona 
            with the following scores.</p>
            <img src='www/diagram.png', style='max-width:100%; max-height:100%;'>
            The algorithm matches Alice with Ben (score of 7) and Charlie with Donna (score of 6). In that way overall score is 13.
            Even though Alice and Charlie have the highest score of 8, they are not optimates, 
            because Ben would be paired with Donna (score of 4). 
            The overall score would be then 12 which is worse than 13.</p>
            ")
        ),
        
        shinydashboard::box(width = 6, 
          DT::DTOutput(ns("table"))
        )
      )
    )
}
    
#' data Server Function
#'
#' @noRd 
mod_data_server <- function(input, output, session){
  ns <- session$ns
  
  answers <- LoadData(table = "answers")
  matches <- LoadData(table = "matches")
  
  output$no.users <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = nrow(answers),    
      subtitle = "Users",
      icon = icon("user"),
      color = "olive"
    )
  })
  
  output$avg.opti <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = matches %>% dplyr::filter(match_indicator == 1) %>% dplyr::select(score) %>% dplyr::pull() %>% mean() %>% round(2),
      subtitle = "Optimates average score",
      icon = icon("trophy"),
      color = "orange"
    )
  })
  
  output$avg.overall <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = matches %>% dplyr::filter(username1 != username2) %>% dplyr::select(score) %>% dplyr::pull() %>% mean() %>% round(2),
      subtitle = "Overall average score",
      icon = icon("star"),
      color = "light-blue"
    )
  })
  
  output$table <- DT::renderDataTable({
    matches
  })
  
}
    
## To be copied in the UI
# mod_data_ui("data_ui_1")
    
## To be copied in the server
# callModule(mod_data_server, "data_ui_1")
 
