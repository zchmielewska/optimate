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
          checkboxInput(ns("chart1.checkbox"), "Only optimates"),
          plotOutput(ns("chart1"))
        ),
        shinydashboard::box(width = 6,
          plotOutput(ns("chart2"))
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
            The overall score would be then 12 which is worse than 13.</p>
            ")
        )
    )
  )
}
    
#' data Server Function
#'
#' @noRd 
mod_data_server <- function(input, output, session){
  ns <- session$ns
  
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = options()$mysql$dbname, host = options()$mysql$host, 
                          port = options()$mysql$port, user = options()$mysql$user, 
                          password = options()$mysql$password)
  
  query.answers <- "SELECT * FROM answers"
  query.matches <- "SELECT * FROM matches WHERE when_added = (SELECT MAX(when_added) FROM matches)"
  
  answers <- DBI::dbGetQuery(db, query.answers)
  matches <- DBI::dbGetQuery(db, query.matches)
  
  RMySQL::dbDisconnect(db)
  
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
      value = matches %>% dplyr::select(score) %>% dplyr::pull() %>% mean() %>% round(2),
      subtitle = "Overall average score",
      icon = icon("star"),
      color = "light-blue"
    )
  })
  
  output$chart1 <- renderPlot({
    plot.matches <- matches %>% 
      dplyr::mutate(as.factor(score))
    
    ggplot2::ggplot(plot.matches, ggplot2::aes(x = score)) +
      ggplot2::geom_bar(ggplot2::aes(y = (..count..)/sum(..count..))) +
      ggplot2::scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
      ggplot2::theme(panel.background = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) + 
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::ylab(NULL)
  })
  
  
  output$chart2 <- renderPlot({
    ggplot2::ggplot(matches, ggplot2::aes(x = score, y = ..density..)) +
      ggplot2::stat_bin(binwidth = 1) + 
      ggplot2::scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
      ggplot2::theme(panel.background = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) + 
      ggplot2::ylab(NULL)
  })
  
  table.data <- matches %>% 
    dplyr::mutate(match_indicator = as.logical(match_indicator)) %>% 
    dplyr::select(`1st person` = username1, `2nd person` = username2, Score = score, `Optimates?` = match_indicator)
  
  output$table <- DT::renderDataTable(table.data, options = list(pageLength = 12))
}
    
## To be copied in the UI
# mod_data_ui("data_ui_1")
    
## To be copied in the server
# callModule(mod_data_server, "data_ui_1")
 
