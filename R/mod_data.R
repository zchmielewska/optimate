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
    ),
    fluidRow(
      shinydashboard::box(width = 6, title = "Scores frequency chart",
        HTML("<div style='height: 70px'>"),
        radioButtons(ns("type"), label = "Scores of which pairs?", inline = TRUE,
                     choiceNames = c("All users", "Only optimates"), choiceValues = c("all", "optimates")),                          
        HTML("</div>"),
        plotOutput(ns("scores.freq"))
      ),
      shinydashboard::box(width = 6, title = "Answers frequency chart",
        HTML("<div style='height: 70px'>"),
        selectInput(ns("question"), "Favourite", choices = survey.table$question),
        HTML("</div>"),
        plotOutput(ns("answers.freq"))
      )
    ),
    fluidRow(
      shinydashboard::box(width = 8, 
        DT::DTOutput(ns("table"))
      ),
      shinydashboard::box(width = 4,
        HTML("<h3>How does it work?</h3>
        <p>The algorithm matches people into pairs to maximize the score of the overall population.
        The score is measured by the number of questions you and the other person answered the same.</p>
        <p>For example, let's imagine that the population consists of four people: Alice, Ben, Charlie and Dona 
        with the following scores.</p>
        <img src='www/diagram.png', style='max-width:100%; max-height:100%;'>
        The algorithm matches Alice with Ben (score of 7) and Charlie with Donna (score of 6). In that way the overall score is 13.
        Even though Alice and Charlie have the highest score of 8, they are not optimates, 
        because Ben would be paired with Donna (score of 4). 
        The overall score would be then 12 which is worse than 13.</p>")
      )
    )
  )
}
    
#' data Server Function
#'
#' @noRd 
mod_data_server <- function(input, output, session, parent_session){
  ns <- session$ns
  
  # Data might get added so the page needs to be constantly fresh
  observe({
    if(parent_session$input$navbar.page.id == "Data") {
      
      answers.matches <- LoadAnswersMatches()
      answers <- answers.matches$answers
      matches <- answers.matches$matches
      
      output$no.users <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = nrow(answers),    
          subtitle = "Users", icon = icon("user"), color = "olive"
        )
      })
      
      output$avg.opti <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = matches %>% dplyr::filter(match_indicator == 1) %>% dplyr::select(score) %>% dplyr::pull() %>% mean() %>% round(2),
          subtitle = "Optimates average score", icon = icon("trophy"), color = "orange"
        )
      })
      
      output$avg.overall <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = matches %>% dplyr::select(score) %>% dplyr::pull() %>% mean() %>% round(2),
          subtitle = "Overall average score", icon = icon("star"), color = "light-blue"
        )
      })
      
      output$scores.freq <- renderPlot({
        
        matches <- dplyr::mutate(matches, score = factor(score, levels = 0:10))
        
        if(input$type == "all") {
          plot.matches <- matches  
        } else if (input$type == "optimates") {
          plot.matches <- dplyr::filter(matches, match_indicator == 1) 
        }
        
        ggplot2::ggplot(plot.matches, ggplot2::aes(x = score)) +
          ggplot2::geom_bar(ggplot2::aes(y = (..count..)/sum(..count..)), fill = "#552583", colour = "black", alpha = 0.4) +
          ggplot2::scale_x_discrete(limits = as.factor(0:10), drop = FALSE) +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         axis.text = ggplot2::element_text(size = 12)) + 
          ggplot2::xlab(NULL) +
          ggplot2::ylab(NULL)
          
      })
      
      
      output$answers.freq <- renderPlot({
        # Factors are needed for geom_bar
        cols <- paste0("q", 1:10)
        answers[cols] <- lapply(answers[cols], factor) 
        
        q.no <- which(survey.table$question == input$question)
        x.var <- paste0("q", q.no)
        
        labels <- survey.table %>% 
          dplyr::filter(question == input$question) %>% 
          dplyr::select(answer1, answer2, answer3, answer4) %>% 
          unlist(., use.names=FALSE)
        
        ggplot2::ggplot(answers, ggplot2::aes_string(x = x.var)) +
          ggplot2::geom_bar(ggplot2::aes(y = (..count..)/sum(..count..)), fill = "#552583", colour = "black", alpha = 0.4) +
          ggplot2::scale_x_discrete(limits = as.factor(1:4), labels = labels, drop = FALSE) +
          ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         axis.text = ggplot2::element_text(size = 12),
                         axis.title = ggplot2::element_text(size = 14)) + 
          ggplot2::xlab(NULL) + 
          ggplot2::ylab(NULL) + 
          ggplot2::coord_flip()
      })
      
      table.data <- matches %>% 
        dplyr::mutate(match_indicator = as.logical(match_indicator)) %>% 
        dplyr::select(`1st person` = username1, `2nd person` = username2, Score = score, `Optimates?` = match_indicator)
      
      output$table <- DT::renderDataTable(table.data, options = list(pageLength = 15))
      
  
    }
  })
}
    
## To be copied in the UI
# mod_data_ui("data_ui_1")
    
## To be copied in the server
# callModule(mod_data_server, "data_ui_1")
 
