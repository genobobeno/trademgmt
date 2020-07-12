#' Survey UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_SurveySB_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::sidebarMenu(id = "surveyMenu",
                                shinydashboard::menuItem(tabName = "survey", text = "Take the Survey!",
                                                         icon = icon(name = "tasks"),selected = TRUE),
                                shinydashboard::menuItem(tabName = "results", text = "See Current Results",
                                                         icon = icon(name = "chart-bar"))
    )
  )
}

mod_SurveyBD_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(
        HTML('#vClick{display: none;} 
              #sClick{display: none;}')
      )
    ),
    conditionalPanel("input.surveyMenu=='survey'",
                     h2("Tell Me About The Content You'd Like To See!"),
        shinydashboard::tabBox(width = 12,#height = "250px",
            tabPanel(title = "Take My Survey",#icon = "check",
                     fluidRow(column(4,
                                     uiOutput(ns("email1")),
                                     uiOutput(ns("selectCategories")),
                                     uiOutput(ns("vClick"))
                                     ),
                              column(8,
                                     uiOutput(ns("voteTopics")),
                                     uiOutput(ns("comment1")),
                                     hr(),
                                     uiOutput(ns("votes"))
                                     )
                              ),
                     fluidRow(column(12,
                                     uiOutput(ns("done"))))
                     ),
            tabPanel(title = "Leave a Note!",#icon = "address-card",
                     uiOutput(ns("email2")),
                     uiOutput(ns("comment2")),
                     uiOutput(ns("sClick")),
                     uiOutput(ns("thanks"))
                     )
            )
    ),
    conditionalPanel("input.surveyMenu=='results'",
                     plotOutput(ns("currentVotes")),
                     DT::dataTableOutput(ns("resultsLegend")))
    # conditionalPanel(condition="(input.uploadStory>0 | input.uploadVote>0) & input.surveyMenu!='results'",
    #                  h3("Thank You For The Sentiments!"),
  )   
}

#' Survey Server Function
#'
#' @noRd 
mod_Survey_server <- function(input, output, session, r){
  ns <- session$ns
  
  #options(gargle_oauth_cache = "/home/egeis/Documents/RProjects/.secrets",stringsAsFactors = FALSE,scipen=999)
  
  # googlesheets4::gs4_auth(
  #   email = gargle::token_email(gargle::token_fetch(  path = "/home/egeis/Documents/RProjects/.secrets/BuildAFlame-d80788031549.json" )),
  #   path = "/home/egeis/Documents/RProjects/.secrets/BuildAFlame-d80788031549.json" ,
  #   cache = gargle::gargle_oauth_cache(),
  #   use_oob = gargle::gargle_oob_default()
  # )
  print(dir())
  print(gargle::gargle_oauth_cache())
  
  json<-gargle:::secret_read("buildaflame","gargle-testing.json")
  # # gargle:::token_fetch(  path = rawToChar(json) )
  googlesheets4::gs4_auth(
    email = gargle::token_email(gargle:::token_fetch(  path = rawToChar(json) )),
    path = rawToChar(json) ,
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default()
  )
  
  r$s <- r$v <- 0
  r$selectedTopics<-c()  
  r$uploadVote<-0
  r$uploadStory<-0
  Topics<-googlesheets4::read_sheet(ss = "1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "Topics")
  Topics<-Topics[order(Topics$Index),]
  #Votes<-googlesheets4::read_sheet(ss = "1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "Votes")
  #Notes<-googlesheets4::read_sheet(ss = "1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "Notes")
  
  Cats<-unique(Topics$Category)
  Articles<-list()
  for (i in Cats) {
    Articles[[i]]<-setNames(object = 1:sum(Topics$Category==i),nm=Topics$Topic[Topics$Category==i])
  }
  
  output$email1<-renderUI({
    if (r$uploadVote==0) {
      textInput(ns("voterEmail"),label="Your email? (optional)",value="")
    } else {
      NULL
    }
  })

  
  output$comment1<-renderUI({
    if (r$uploadVote==0) {
      textInput(ns("addComments"),"Any comments in reference to your vote?")
    } else {
      NULL
    }
  })
  
  output$selectCategories<-renderUI({
    req(nchar(Cats[1])>0) 
    l<-setNames(1:length(Cats),Cats)
    print("category list")
    print(l)
    print(class(l))
    if (r$uploadVote==0) {
      radioButtons(ns("categoryChoice"),label = "Choose the category of articles to display:",
                   choices = l,selected = 1)
    } else {
      NULL
    }
  })

  observe({
    r$selectedTopics<-unique(c(r$selectedTopics,as.numeric(input$topicVotes)))
  })
  
  output$voteTopics<-renderUI({
    req(nchar(Cats[1])>0)
    if (r$uploadVote==0) {
      if (!is.na(r$selectedTopics) && sum(r$selectedTopics %in% Topics$Index[Topics$Category==Cats[as.numeric(input$categoryChoice)]])>0) {
        selectedTopics<-r$selectedTopics[r$selectedTopics %in% Topics$Index[Topics$Category==Cats[as.numeric(input$categoryChoice)]]]
      } else {
        selectedTopics<-NA
      }
      print("topic list")
      ll<-setNames(Topics$Index[Topics$Category==Cats[as.numeric(input$categoryChoice)]],
                  Topics$Topic[Topics$Category==Cats[as.numeric(input$categoryChoice)]])
      print(ll)
      print(class(ll))
      checkboxGroupInput(ns("topicVotes"),label = "Check the articles you'd like to read:",
                         choices = ll,
                         selected = selectedTopics)
    } else {
      NULL 
    }
  })
  
  output$vClick<-renderUI({
    if (r$v<1) {
      actionButton(ns("uploadVote"),label = "Submit Vote!")
    } else {
      NULL
    }
  })
    
  output$votes<-renderUI({
    req(length(r$selectedTopics)>0)
    if (r$uploadVote==0) {
      txt<-paste0("<ul>",paste0(paste0("<li>",Topics$Category[Topics$Index %in% r$selectedTopics],": ",
                                  Topics$Topic[Topics$Index %in% r$selectedTopics]),collapse=""),
             "</ul>")
      HTML(txt)
    } else {
      NULL
    }
  })
  
  output$done<-renderUI({
    req(r$uploadVote>0)
      txt<-paste0("<ul>",paste0(paste0("<li>",Topics$Category[Topics$Index %in% r$selectedTopics],": ",
                                       Topics$Topic[Topics$Index %in% r$selectedTopics]),collapse=""),
                  "</ul>")
      txt<-paste0("<h3>Thank you for submitting these votes!</h3><p>",txt,
                  "<p><h3>If you'd like to tell me more, use the 'Leave A Note' form!</h3>
                   <h3>Also, feel free to check out the results of voting!</h3>")
    HTML(txt)
  })
  
  #                  p()
  observeEvent(input$uploadVote,{
    print(input$uploadVote)
    r$uploadVote<-input$uploadVote
    if (length(r$selectedTopics)>0) {
      Votes<-data.frame(#Category=Topics$Category[r$selectedTopics], Topic = Topics$Topic[r$selectedTopics],
                        Index = Topics$Index[r$selectedTopics], User = input$voterEmail,
                        Comment = input$addComments, Timestamp = Sys.time())
      googlesheets4::sheet_append(data = Votes,ss="1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "Votes")
      fct_readVotes()
      r$selectedTopics<-c()
      #hide("uploadVote")
      #utils_disableActionButton("uploadVote",session)
    }
    r$v<-1
  })

  output$email2<-renderUI({
    if (r$uploadStory==0) {
      textInput(ns("noteEmail"),label = "Your email? (optional)",value = "")
    } else {
      NULL
    }
  })
  
  output$comment2<-renderUI({
    if (r$uploadStory==0) {
      textInput(ns("userStory"),label = "Let me know what topics you're thinking about:",value = "e.g. life, education, stats, money",width = '100%')
    } else {
      NULL
    }
  })
  
  output$sClick<-renderUI({
    if (r$s<1) {
      actionButton(ns("uploadStory"),label = "Submit!")
    } else {
      NULL
    }
  })
  
  output$thanks<-renderUI({
    req(r$uploadStory>0)
    if (r$uploadStory==1) {
      txt<-paste0("<h2>Thank you for sharing!</h2>",txt,
                  "<h3>If you'd like to vote for some topics you'd like to hear about,</h3>
                   <h3>click on the previous tab or reload the page!</h3>
                   <h3>Also, feel free to check out the results of the voting so far!</h3>")
    }
    HTML(txt)
  })
  
  
  observeEvent(input$uploadStory,{
    print(input$uploadStory)
    r$uploadStory<-input$uploadStory
    story<-input$userStory
    if (nchar(story)>0) {
      Notes<-data.frame(User = input$noteEmail,Comment = story,Timestamp = Sys.time())
      googlesheets4::sheet_append(data = Notes,ss="1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "Notes")
      story<-""
      #hide("uploadStory")
      #utils_disableActionButton("uploadStory",session)
    }
    r$s<-1
  })
  
  output$currentVotes<-renderPlot({
    Results<-googlesheets4::read_sheet(ss = "1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "Topics")
    Results<-Results[!is.na(Results$Votes),]
    r$DataTable<-Results[order(-Results$Votes),c(1,2,3,4)]
    barplot(setNames(object = Results$Votes,Results$Index),main="Current Votes on Topics (Indices)",xlab="Topic Index (see Table)",
            ylab="Votes")
  })
  
  output$resultsLegend<-DT::renderDataTable(r$DataTable,extensions=c('Scroller'),
                                            options = list(dom = 'Bfrtip',
                                                           scrollY = 500,
                                                           scroller = TRUE,
                                                           scrollX = TRUE),rownames = FALSE)
  
  # output$LinkBox <- DT::renderDataTable({
  #   IE<-utils_createLink("https://ie.datamyx.com/","IntellidataExpress")
  #   DS<-utils_createLink("https://www.pivotaltracker.com/n/projects/2055771","DS Pivotal Tracker")
  #   my_table <- data.frame("Link"=c(IE,DS),
  #                        "Description"=c("Intellidata Express is our interface to all of our data!",
  #                                        "The Data Science Pivotal Tracker Backlog of projects."))
  #   DT::datatable(my_table,options = list(paging=FALSE,searching=FALSE,processing=FALSE),escape=FALSE)
  # })
   
}
    
## To be copied in the UI
# mod_Survey_ui("Survey_ui_1")
    
## To be copied in the server
# callModule(mod_Survey_server, "Survey_ui_1")
 
