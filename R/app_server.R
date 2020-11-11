#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_start_server, "start_ui_1")
  callModule(mod_optimate_server, "optimate_ui_1")
  callModule(mod_data_server, "data_ui_1")
}
