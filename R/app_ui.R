#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @noRd
app_ui <- function(request) {
  library(shinydashboard)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "Trade Management",
                                      dropdownMenu(type = "messages",
                                                   messageItem(
                                                     from = "Geno",
                                                     message = "Email me: eugene.geis@gmail.com"
                                                   ))),
      shinydashboard::dashboardSidebar(
        mod_GrowthRiskSB_ui("growth")),
      shinydashboard::dashboardBody(
        mod_GrowthRiskBD_ui("growth")
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'trademgmt'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

