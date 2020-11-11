#' start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_start_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      shinydashboard::box(width = 4, 
        HTML("<p>On your right, you can see a questionnaire.</p> 
        <p>This questionnaire is the basis to create <b>optimates</b>. 
        Optimates are pairs of people that are the best matches for each other in the context of the whole <b>population</b>.</p>
        <p>If you want to check who's your optimate, please fill in the questionnaire and go to the Optimate section of the app.</p>")
      ),  
      shinydashboard::box(width = 8,
        HTML("<h3>What's your <span style='color: #552583;'>nickname</span>?</h3>"),
        textInput(ns("username"), label = NULL, width = '250px', placeholder = "Your nickname"),
        HTML("<span style='font-size: x-small; font-weight: bold;'>
        Please don't use personal data. Your nickname will be publicly available!</span>"),
                          
        HTML("<h3>What's your <span style='color: #552583;'>favourite</span>...?</h3>"),
        
        shinyWidgets::radioGroupButtons(ns("q1"), label = h4("[1/10] Season:"),
        choiceNames = c("Winter", "Spring", "Summer", "Autumn"),
        justified = TRUE, individual = TRUE, choiceValues = 1:4),
        
        shinyWidgets::radioGroupButtons(ns("q2"), label = h4("[2/10] Colour:"),
        choiceNames = c("Red", "Green", "Blue", "Yellow"),
        justified = TRUE, individual = TRUE, choiceValues = 1:4),
        
        shinyWidgets::radioGroupButtons(ns("q3"), label = h4("[3/10] Superhero:"),
        choiceNames = c("Superman", "Wonder Woman", "Batman", "Spider-Man"),
        justified = TRUE, individual = TRUE, choiceValues = 1:4),
        
        shinyWidgets::radioGroupButtons(ns("q4"), label = h4("[4/10] Food:"),
        choiceNames = c("Pizza", "Salad", "Burger", "Pasta"),
        justified = TRUE, individual = TRUE, choiceValues = 1:4),
        
        shinyWidgets::radioGroupButtons(ns("q5"), label = h4("[5/10] TV serie:"),
        choiceNames = c("Friends", "Breaking Bad", "Game of Thrones", "Family Guy"),
        justified = TRUE, individual = TRUE, choiceValues = 1:4),
        
        shinyWidgets::radioGroupButtons(ns("q6"), label = h4("[6/10] Pet:"),
        choiceNames = c("Cat", "Dog", "Fish", "Parrot"),
        justified = TRUE, individual = TRUE, choiceValues = 1:4),
        
        shinyWidgets::radioGroupButtons(ns("q7"), label = h4("[7/10] Sport:"),
        choiceNames = c("Football", "Tennis", "Swimming", "Skiing"),
        justified = TRUE, individual = TRUE, choiceValues = 1:4),
        
        shinyWidgets::radioGroupButtons(ns("q8"), label = h4("[8/10] Ice cream flavour:"),
        choiceNames = c("Chocolate", "Vanilla", "Strawberry", "Pistachio"),
        justified = TRUE, individual = TRUE, choiceValues = 1:4),
        
        shinyWidgets::radioGroupButtons(ns("q9"), label = h4("[9/10] Outdoor place:"),
        choiceNames = c("Beach", "Forest", "City", "Mountains"),
        justified = TRUE, individual = TRUE, choiceValues = 1:4),
        
        shinyWidgets::radioGroupButtons(ns("q10"), label = h4("[10/10] Board game:"),
        choiceNames = c("Chess", "Monopoly", "Scrabble", "Jenga"),
        justified = TRUE, individual = TRUE, choiceValues = 1:4),
      
        HTML("<div style='text-align: center;'>"),
        actionButton(ns("button"), label = "Send", width = "200px", class = "btn btn-success", 
                     style = "color: #fff; margin-top: 15px; margin-bottom: 15px;"),
        HTML("</div>")
      )
    )
  )
}

#' start Server Function
#'
#' @noRd 
mod_start_server <- function(input, output, session){
  ns <- session$ns

  observeEvent(input$button, {
    # Nickname/Username must be provided
    if(input$username == "") {
      showModal(modalDialog(
        title = "Nickname is empty",
        "Nickname can't be empty. Please provide a nickname.", footer = modalButton("OK"), fade = FALSE
      ))
    } else {
      # Answers are added to the answers table
      users_answers <- data.frame(
        username = input$username,
        q1 = input$q1,
        q2 = input$q2,
        q3 = input$q3,
        q4 = input$q4,
        q5 = input$q5,
        q6 = input$q6,
        q7 = input$q7,
        q8 = input$q8,
        q9 = input$q9,
        q10 = input$q10,
        stringsAsFactors = FALSE
      )
      SaveData(data = users_answers, databaseName = "optimate_schema", table = "answers")
      
      answers <- LoadData(databaseName = "optimate_schema", table = "answers")
      scores.matrix <- ReturnScoresMatrix(answers)
      optimal.pairs <- ReturnOptimalPairs(scores.matrix)
      matches <- ReturnMatches(scores.matrix, optimal.pairs)
      SaveData(data = matches, databaseName = "optimate_schema", table = "matches")
      
      showModal(modalDialog(
        title = "Success!",
        "Your answers have been saved. Now, please go to the Optimate section to check your pair.", 
        footer = modalButton("OK")
      ))
    }
  })  
}
    
## To be copied in the UI
# mod_start_ui("start_ui_1")
    
## To be copied in the server
# callModule(mod_start_server, "start_ui_1")
 
