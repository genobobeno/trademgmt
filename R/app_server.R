#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  r<-reactiveValues()
  callModule(mod_GrowthRisk_server,"growth",r)
  session$onSessionEnded(function() {
    stopApp()
  })
  
}
