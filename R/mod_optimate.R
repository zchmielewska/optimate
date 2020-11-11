#' optimate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_optimate_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      shinydashboard::box(width = 4, 
        HTML("<h3 style='text-align: center;'>What was your <span style='color: #552583;'>nickname</span> again...?</h3>"),
        
        HTML("<div style='display: flex; justify-content: center;'>"),
        textInput(ns("username"), label = NULL, width = '250px', placeholder = "Your nickname"),
        HTML("</div>"),
        
        HTML("<div style='text-align: center;'>"),
        actionButton(ns("button"), label = "Check", width = "200px", class = "btn btn-success", 
                     style = "color: #fff; margin-top: 10px; margin-bottom: 15px;"),
        HTML("</div>")
      ),
      
      shinydashboard::box(width = 8, 
        HTML("<p><i>Please, provide your nickname to check your optimate.</i></p>"),
        
        htmlOutput(ns("result")),
        HTML("<div style='display: flex; justify-content: center;'>"),
        tableOutput(ns("compatibility.table")),
        HTML("</div>")
      )
    )
  )
}
    
#' optimate Server Function
#'
#' @noRd 
mod_optimate_server <- function(input, output, session){
  ns <- session$ns
 
  observeEvent(input$button, {
    
    ### in development
    
    db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = options()$mysql$dbname, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
    
    query <- "SELECT * FROM answers"
    
    # Submit the fetch query and disconnect
    data <- DBI::dbGetQuery(db, query)
    
    
    matches <- LoadData(table = "matches")
    answers <- LoadData(table = "answers")
    matches <- tibble::as_tibble(matches)
    max.when_added <- matches %>% dplyr::select(when_added) %>% dplyr::pull() %>% max()
    latest.matches <- dplyr::filter(matches, when_added == max.when_added)
    optimate.result <- dplyr::filter(latest.matches, username1 == input$username  & match_indicator == 1)
    
    # User can fill in a wrong username
    if (nrow(optimate.result) == 0) {
      showModal(modalDialog(
        title = "Incorrect nickname",
        "Can't find the result for this nickname. 
        You need to complete the survey before checking the result. 
        If you've already completed the survey, please check the nickname and try again.", 
        footer = modalButton("OK"), fade = FALSE
      ))
    } else {
      output$result <- renderUI(HTML(paste0("
        <hr/>
        <h3 style='text-align: center;'>Your optimate is <span style='color: #552583;'>", optimate.result$username2, "</span>!
        Your score is <span style='color: #552583;'>", paste0(optimate.result$score, '0%'), "</span>.</h3>
        <p style='text-align: center;'>Your answers:</p>")))
      output$compatibility.table <- renderTable({
        compatibility.table <- cbind(dplyr::select(survey.table, "question"), 
              ReturnUsersAnswers(input$username, answers, survey.table),
              ReturnUsersAnswers(optimate.result$username2, answers, survey.table))
        colnames(compatibility.table) <- c("Question", input$username, optimate.result$username2)
        compatibility.table
      })
    }
  })
  
  
}
    
## To be copied in the UI
# mod_optimate_ui("optimate_ui_1")
    
## To be copied in the server
# callModule(mod_optimate_server, "optimate_ui_1")
 