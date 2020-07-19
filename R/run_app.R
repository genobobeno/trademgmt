#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  ...
) {
  library("googledrive")
  library("googlesheets4")
  library("gargle")
  options(gargle_quiet = FALSE)
  #options(gargle_oauth_cache = "/home/egeis/Documents/RProjects/.secrets",stringsAsFactors = FALSE,scipen=999)
  options(gargle_oauth_cache = "inst/secret",stringsAsFactors = F,scipen=999)
  readRenviron(system.file(".Renviron",package = "trademgmt"))
  #print(Sys.getenv("SHINYAPPS_TOKEN"))
  
  GList<-list()
  GList$accountSize<-25000
  GList$expectedGrowth<-c(15,25)
  GList$weeklyIncomeGoal<-5000  
  GList$friday<-TRUE
  #GList$numTrades<-5
  GList$percentGain<-20
  GList$percentWager<-10
  GList$email<-""
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(...)
  )
}
