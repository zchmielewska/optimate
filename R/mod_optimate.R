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
        htmlOutput(ns("info")),                  
        htmlOutput(ns("result")),
        HTML("<div style='display: flex; justify-content: center;'>"),
        tableOutput(ns("compatibility.table")),
        HTML("</div>"),
        htmlOutput(ns("message"))
      )
    )
  )
}
    
#' optimate Server Function
#'
#' @noRd 
mod_optimate_server <- function(input, output, session){
  ns <- session$ns
  
  output$info <- renderUI({
    HTML("<p><i>Please, provide your nickname to check your optimate.</i></p>")
  })
  
  observeEvent(input$button, {
    output$info <- renderUI(NULL)
    
    # Matches are stored in DB
    db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = options()$mysql$dbname, host = options()$mysql$host, 
                            port = options()$mysql$port, user = options()$mysql$user, 
                            password = options()$mysql$password)
    
    query.answers <- "SELECT * FROM answers"
    answers <- DBI::dbGetQuery(db, query.answers)
    
    query.matches <- sprintf("SELECT * FROM matches 
                              WHERE when_added = (SELECT MAX(when_added) FROM matches)
                              AND username1 = '%s';", input$username)
    matches <- DBI::dbGetQuery(db, query.matches)
    
    optimate.result <- dplyr::filter(matches, match_indicator == 1)
    
    # User can fill in a wrong username
    if (nrow(optimate.result) == 0 & !(input$username %in% matches$username1)) {
      showModal(modalDialog(
        title = "Incorrect nickname",
        "Can't find the result for this nickname. 
        You need to complete the survey before checking the result. 
        If you've already completed the survey, please check the nickname and try again.", 
        footer = modalButton("OK"), fade = FALSE
      ))
      
    # User can have no optimate if there is an odd number of participants
    } else if (nrow(optimate.result) == 0 & input$username %in% matches$username1) {  
      output$result <- renderUI(
        HTML("
        <h3>Damn!</h3>
        <p>Currently there is an <b>odd</b> number of participants.</p> 
        <p>Unfortunately, there is no optimate for you... Please come back later as there will be new participants joining!</p>"))
      
      output$compatibility.table <- NULL
      output$message <- NULL
      
    } else {
      
      # Main information about the result
      output$result <- renderUI({
        HTML(paste0("
        <h3 style='text-align: center;'>Your optimate is <span style='color: #552583;'>", optimate.result$username2, "</span>
        with score <span style='color: #552583;'>", paste0(optimate.result$score, '0%'), "</span>!</h3>
        <h4 style='text-align: center;'>Answers:</h4>"))
      })
      
      # Table with comparison of answers
      output$compatibility.table <- renderTable({
        isolate({
          compatibility.table <- data.frame(dplyr::select(survey.table, "question"), 
                                            user1 = ReturnUsersAnswers(input$username, answers, survey.table),
                                            user2 = ReturnUsersAnswers(optimate.result$username2, answers, survey.table),
                                            stringsAsFactors = FALSE) %>% 
            dplyr::mutate(Score = dplyr::if_else(user1 == user2, "10%", "0%"))
          
          colnames(compatibility.table) <- c("Favourite", input$username, optimate.result$username2, "Score")
          
          compatibility.table  
        })
      })
      
      # Optional message from the optimate
      output$message <- renderUI({
        msg <- answers %>% 
          dplyr::filter(username == optimate.result$username2) %>% 
          dplyr::select(message) %>% 
          dplyr::pull()
        
        if(msg == "") {
          NULL
        } else {
          HTML(
          paste0("
          <h4 style='text-align: center;'>Message from ", optimate.result$username2, ":</h4>
          <div style='border-style: dashed; border-width: 1px; border-color: #d3d3d3; width: 100%; padding: 10px;'>", msg, "</div>"))
        }
      })
    }
  })
}
    
## To be copied in the UI
# mod_optimate_ui("optimate_ui_1")
    
## To be copied in the server
# callModule(mod_optimate_server, "optimate_ui_1")
  