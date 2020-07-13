#' Survey UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_GrowthRiskSB_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::sidebarMenu(id = "tradeMgmtMenu",
                                shinydashboard::menuItem(tabName = "growth", text = "Start Here With Growth",
                                                         icon = icon(name = "money"),selected = TRUE),
                                shinydashboard::menuItem(tabName = "risk", text = "Risk Management",
                                                         icon = icon(name = "medkit"))
    )
  )
}

mod_GrowthRiskBD_ui <- function(id){
  ns <- NS(id)
  tagList(
    # tags$head(
    #   tags$style(
    #     HTML('#vClick{display: none;} 
    #           #sClick{display: none;}')
    #   )
    # ),
    conditionalPanel("input.tradeMgmtMenu=='growth'",
        fluidRow(
          shinydashboard::box(title = "Your Account Details:",width = 3,#height = "250px",
                              uiOutput(ns("dAccountSize")), #numericInput(ns("accountSize"),label = "Size of Current Account:",value = 25000,min = 100,max = 5000000,step=100),
                              uiOutput(ns("dExpectedGrowth")), #sliderInput(ns("expectedGrowth"),label = "The range of expected weekly profit (%):",min = 0,max = 100,step = .05,value = c(15,25)),
                              uiOutput(ns("dWeeklyIncomeGoal")), #numericInput(ns("weeklyIncomeGoal"),label = "What is your weekly income goal?",value = 5000,min = 1000,max = 200000,step = 100)),
                              uiOuput(ns("dFriday")) #checkboxInput(ns("friday"),label = "Do you intend to trade on Fridays?",value = TRUE)
          ),
          shinydashboard::tabBox(title = "Some Calculations:",width = 5,#height = "250px",
                                 tabPanel(title = "Expectations",
                                          uiOutput(ns("timeCalc")),
                                          uiOutput(ns("weeklyAmountCalc"))
                                 ),
                                 tabPanel(title= "Amortized Weekly",
                                          uiOutput("dLowHigh1"), #radioButtons(ns("lowHigh1"),label = "Do you want low or high win rate estimates?",choices = list(""))
                                          DT::dataTableOutput(ns("weeklyIncomeTab"))
                                 ),
                                 tabPanel(title= "Amortized Daily",
                                          uiOutput("dLowHigh2"), #radioButtons(ns("lowHigh2"),label = "Do you want low or high win rate estimates?",choices = list(""))
                                          DT::dataTableOutput(ns("dailyIncomeTab"))
                                 ),
                                 tabPabel(title="Trade Plan",
                                          numericInput(ns("numTrades"),"How many trades do you plan to trade each week?",value = 5,step=1,min = 1,max=25),
                                          sliderInput(ns("percentGain"),"What's your expected gain in each trade (%)?",value = 20,step = 1,min = 1,max=100),
                                          sliderInput(ns("percentWager"),"How much of your account do you want to use in each trade (%)?",value = 10,step = 1,min = 1,max=100),
                                          uiOutput(ns("tradeMessage")), # h4(txt)
                                          DT::dataTableOutput(ns("tradePlan"))
                                          )
          ),
          shinydashboard::box(title = "The Math:",width = 4,#height = "250px",
                              radioButtons(ns("dayWeek"),label = "Plot by Day or Week?",choices = list("Day"=1,"Week"=2),selected = 1),
                              renderPlot(ns("accountGrowth")), #Account Size vs. Time
                              renderPlot(ns("tradeSize")), #
                              
          ),
          
        ),
        
        #              uiOutput(ns("email2")),
        #              uiOutput(ns("comment2")),
        #              uiOutput(ns("sClick")),
        #              uiOutput(ns("thanks"))
        #              )
        #     )
    conditionalPanel("input.tradeMgmtMenu=='risk'",
                     # plotOutput(ns("currentVotes")),
                     # DT::dataTableOutput(ns("resultsLegend")))
    # conditionalPanel(condition="(input.uploadStory>0 | input.uploadVote>0) & input.surveyMenu!='results'",
    #                  h3("Thank You For The Sentiments!"),
    )
  ) 
  )
}

#' Survey Server Function
#'
#' @noRd 
mod_GrowthRisk_server <- function(input, output, session, r){
  ns <- session$ns
  
  #options(gargle_oauth_cache = "/home/egeis/Documents/RProjects/.secrets",stringsAsFactors = FALSE,scipen=999)
  
  # googlesheets4::gs4_auth(
  #   email = gargle::token_email(gargle::token_fetch(  path = "/home/egeis/Documents/RProjects/.secrets/BuildAFlame-d80788031549.json" )),
  #   path = "/home/egeis/Documents/RProjects/.secrets/BuildAFlame-d80788031549.json" ,
  #   cache = gargle::gargle_oauth_cache(),
  #   use_oob = gargle::gargle_oob_default()
  # )
  r$accountSize<-25000
  r$expectedGrowth<-c(15,25)
  r$weeklyIncomeGoal<-5000  
  r$friday<-TRUE
  input$numTrades<-5
  input$percentGain<-20
  
  output$dFriday<-renderUI({
    checkboxInput(ns("friday"),label = "Do you intend to trade on Fridays?",value = r$friday)
  })
  output$dAccountSize<- renderUI({
    numericInput(ns("accountSize"),label = "Size of Current Account:",value = r$accountSize,min = 100,max = 5000000,step=100)
  })
  output$dExpectedGrowth<- renderUI({
    sliderInput(ns("expectedGrowth"),label = "The range of expected weekly profit (%):",min = 0,max = 100,step = .05,value = r$expectedGrowth)
  })
  output$dWeeklyIncomeGoal<- renderUI({
    numericInput(ns("weeklyIncomeGoal"),label = "What is your weekly income goal?",value = r$weeklyIncomeGoal,min = 1000,max = 200000,step = 100)
  })

  # observe({
  #   
  # })
  
  output$dLowHigh1<-renderUI({
    CH<-utils_createNumList(c(paste0(input$expectedGrowth[1],"%"),paste0(input$expectedGrowth[2],"%")))
    radioButtons(ns("lowHigh1"),label = "Do you want low or high win rate estimates?",choices = CH )
  })
  output$dLowHigh2<-renderUI({
    CH<-utils_createNumList(c(paste0(input$expectedGrowth[1],"%"),paste0(input$expectedGrowth[2],"%")))
    radioButtons(ns("lowHigh2"),label = "Do you want low or high win rate estimates?",choices = CH )
  })
  
  observe({
    t1<-ceiling(fct_timeForGrowth(input$accountSize,input$expectedGrowth[as.numeric(input$lowHigh1)]/100,input$weeklyIncomeGoal))
    i1<-as.vector(sapply(1:t1,function(x) fct_expGrowth(input$accountSize,input$expectedGrowth[as.numeric(input$lowHigh1)]/100,x,"w",input$friday)))
    t2<-ceiling(fct_timeForGrowth(input$accountSize,input$expectedGrowth[as.numeric(input$lowHigh1)]/100,input$weeklyIncomeGoal)*(4+input$friday))
    i2<-as.vector(sapply(1:t2,function(x) fct_expGrowth(input$accountSize,input$expectedGrowth[as.numeric(input$lowHigh1)]/100,x,"d",input$friday)))
    r$weeklyIncomeTable<-data.frame("Weeks"=1:t1,"BeginningAmount"=c(input$accountSize,i1[-length(i1)]),"Gain"=diff(c(input$accountSize,i1)),"EndAmount"=i1)
    r$dailyIncomeTable<-data.frame("Days"=1:t2,"BeginningAmount"=c(input$accountSize,i2[-length(i2)]),"Gain"=diff(c(input$accountSize,i2)),"EndAmount"=i2)
  })

  output$timeCalc<-renderUI({
    t1<-ceiling(fct_timeForGrowth(input$accountSize,input$expectedGrowth[1]/100,input$weeklyIncomeGoal))
    t2<-ceiling(fct_timeForGrowth(input$accountSize,input$expectedGrowth[2]/100,input$weeklyIncomeGoal))
    txt<-paste0("It will take between ",t2," and ",t1," weeks to reach your salary goal.")
    h3(txt)
  })
  
  output$weeklyAmountCalc<-renderUI({
    m1<-round(input$accountSize*input$expectedGrowth[1]/100)
    m2<-round(input$accountSize*input$expectedGrowth[2]/100)
    txt<-paste0("In the first week, you'll need to make between $",m1," and $",m2,".")
    h3(txt)
  })
  output$weeklyIncomeTable<-DT::renderDataTable(r$weeklyIncomeTable,extensions=c('Scroller'),
                                                options = list(dom = 'Bfrtip',
                                                               scrollY = 500,
                                                               scroller = TRUE,
                                                               scrollX = TRUE),rownames = FALSE)
  output$dailyIncomeTable<-DT::renderDataTable(r$dailyIncomeTable,extensions=c('Scroller'),
                                                options = list(dom = 'Bfrtip',
                                                               scrollY = 500,
                                                               scroller = TRUE,
                                                               scrollX = TRUE),rownames = FALSE)
  
  observe({
    winGain<-input$numTrades*input$percentGain/10$
    
  })
  
  numericInput(ns("numTrades"),"How many trades do you plan to trade each week?",value = 5,step=1,min = 1,max=25),
  sliderInput(ns("percentGain"),"What's your expected gain in each trade (%)?",value = 20,step = 1,min = 1,max=100),
  sliderInput(ns("percentWager"),"How much of your account do you want to use in each trade (%)?",value = 10,step = 1,min = 1,max=100),
  uiOutput(ns("tradeMessage")), # h4(txt)
  DT::dataTableOutput(ns("tradePlan"))
  

    
  # json<-gargle:::secret_read("trademgmt","gargle-testing.json")
  # # # gargle:::token_fetch(  path = rawToChar(json) )
  # googlesheets4::gs4_auth(
  #   email = gargle::token_email(gargle:::token_fetch(  path = rawToChar(json) )),
  #   path = rawToChar(json) ,
  #   cache = gargle::gargle_oauth_cache(),
  #   use_oob = gargle::gargle_oob_default()
  # )
  # Topics<-googlesheets4::read_sheet(ss = "1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "Topics")


  output$email<-renderUI({
    if (r$emailSubmit==0 & r$emailRecall==0) {
      textInput(ns("voterEmail"),label="Need your email if you'd like to save this (optional):",value="")
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
 
