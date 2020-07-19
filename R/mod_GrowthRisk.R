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
                                                         icon = icon("money"),selected = TRUE),
                                shinydashboard::menuItem(tabName = "risk", text = "Risk Management",
                                                         icon = icon("medkit"))
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
                              radioButtons(ns("dayWeek"),label = "Do you want to see goals by Day or Week?",choices = list("Day"=1,"Week"=2),selected = 1),
                              uiOutput(ns("dFriday")) #checkboxInput(ns("friday"),label = "Do you intend to trade on Fridays?",value = TRUE)
          ),
          shinydashboard::tabBox(title = "Some Calculations",width = 5,#height = "250px",
                                 tabPanel(title = "Expectations",
                                          uiOutput(ns("calcSummary")),
                                          # uiOutput(ns("timeCalc")),
                                          # uiOutput(ns("weeklyAmountCalc"))
                                          hr(),
                                          h4("Gains Table"),
                                          uiOutput(ns("dLowHigh1")), #radioButtons(ns("lowHigh1"),label = "Do you want low or high win rate estimates?",choices = list(""))
                                          DT::dataTableOutput(ns("incomeTable"))
                                          # DT::dataTableOutput(ns("weeklyIncomeTable"))
                                          # uiOutput(ns("dLowHigh2")), #radioButtons(ns("lowHigh2"),label = "Do you want low or high win rate estimates?",choices = list(""))
                                          # DT::dataTableOutput(ns("dailyIncomeTable"))
                                 ),
                                 tabPanel(title="Trade Plan",
                                          # uiOutput(ns("dNumTrades")), #numericInput(ns("numTrades"),"How many trades do you plan to trade each week?",value = 5,step=1,min = 1,max=25),
                                          uiOutput(ns("dPercentGain")), #sliderInput(ns("percentGain"),"What's your expected gain in each trade (%)?",value = 20,step = 1,min = 1,max=100),
                                          uiOutput(ns("dPercentWager")), #sliderInput(ns("percentWager"),"How much of your account do you want to use in each trade (%)?",value = 10,step = 1,min = 1,max=100),
                                          hr(),
                                          uiOutput(ns("tradeMessage")), # h4(txt)
                                          DT::dataTableOutput(ns("tradePlan"))
                                          )
          ),
          shinydashboard::box(title = "The Math:",width = 4,#height = "250px",
                              plotOutput(ns("accountGrowth"))#, #Account Size vs. Time
                              #renderPlot(ns("tradeSize")) # Trade Size vs. Number of Trades, lines = Target Percent Gains
          ),
          shinydashboard::box(title = "Save Data?",width = 3,
                              uiOutput(ns("dEmail")), #textInput(ns("email"),"Enter Email to Save Your Analysis (optional))
                              actionButton(ns("saveData"),label = "Save Data",icon = icon("upload")),
                              actionButton(ns("loadData"),label = "Load Data",icon= icon("download")),
                              hr(),
                              uiOutput(ns("dataMessage")) # h5("Data Saved"), h5("Data Loaded"), h5("Doesn't look like an email")
          )
          
        ),
        
        #              uiOutput(ns("email2")),
        #              uiOutput(ns("comment2")),
        #              uiOutput(ns("sClick")),
        #              uiOutput(ns("thanks"))
        #              )
        #     )
    conditionalPanel("input.tradeMgmtMenu=='risk'",
                     h3("Hello")
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
  
  # 
  options(gargle_oob_default = TRUE)
  json<-gargle:::secret_read("trademgmt","gargle-testing.json")
  # # gargle:::token_fetch(  path = rawToChar(json) )
  googlesheets4::gs4_auth(
    email = gargle::token_email(gargle:::token_fetch(  path = rawToChar(json) )),
    path = rawToChar(json) ,
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default()
  )
  # r$accountSize<-25000
  # r$expectedGrowth<-c(15,25)
  # r$weeklyIncomeGoal<-5000  
  # r$friday<-TRUE
  # #r$numTrades<-5
  # r$percentGain<-20
  # r$percentWager<-10
  print("1")
  r$saveData<-0
  r$loadData<-0
  r$emailFlag<-FALSE
  r$saveTime<-0
  r$loadTime<-0
  # r$email<-""
  r$badLoad=FALSE
  
  print("2")
  
  # SaveData=data.frame(email="eugene.quickreaction@gmail.com",
  #                     accountSize=5000,
  #                     percentGain=20,
  #                     percentWager=10,
  #                     expectedGrowthLow=15,
  #                     expectedGrowthHigh=25,
  #                     weeklyIncomeGoal=5000,
  #                     friday=TRUE,
  #                     numTrades=10,
  #                     timeStamp=as.character(Sys.time()))
  # write_sheet(data=SaveData,ss="1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "TradePlanData")
  
  output$dFriday<-renderUI({
    print("Checkbox")
    checkboxInput(ns("friday"),label = "Do you intend to trade on Fridays?",value = TRUE)
  })
  output$dAccountSize<- renderUI({
    print("account")
    numericInput(ns("accountSize"),label = "Size of Current Account:",value = 5000,min = 100,max = 5000000,step=100)
  })
  output$dExpectedGrowth<- renderUI({
    print("growth")
    sliderInput(ns("expectedGrowth"),label = "The range of expected weekly profit (%):",min = 0,max = 100,step = .5,value = c(15,25))
  })
  output$dWeeklyIncomeGoal<- renderUI({
    print("income")
    numericInput(ns("weeklyIncomeGoal"),label = "What is your weekly income goal?",value = 5000,min = 1000,max = 200000,step = 100)
  })

  # observe({
  #   
  # })
  
  output$dLowHigh1<-renderUI({
    print("LH1")
    CH<-utils_createNumList(c(paste0(input$expectedGrowth[1],"%"),paste0(input$expectedGrowth[2],"%")))
    radioButtons(ns("lowHigh1"),label = "Do you want low or high win rate estimates?",choices = CH )
  })
  output$dLowHigh2<-renderUI({
    print("LH2")
    CH<-utils_createNumList(c(paste0(input$expectedGrowth[1],"%"),paste0(input$expectedGrowth[2],"%")))
    radioButtons(ns("lowHigh2"),label = "Do you want low or high win rate estimates?",choices = CH )
  })
  

  output$calcSummary<-renderUI({
    req(input$accountSize>0 & input$expectedGrowth[1]>0 & input$weeklyIncomeGoal>0)
    if (as.numeric(input$dayWeek)==1) dw<-"d" else dw<-"w"
    t1<-ceiling(fct_timeForGrowth(input$accountSize,input$expectedGrowth[1]/100,input$weeklyIncomeGoal,dw,input$friday))
    t2<-ceiling(fct_timeForGrowth(input$accountSize,input$expectedGrowth[2]/100,input$weeklyIncomeGoal,dw,input$friday))
    if (dw=="d") {
      t1<-ceiling(t1/(4+input$friday))
      t2<-ceiling(t2/(4+input$friday))
    }
    m1<-round(input$accountSize*input$expectedGrowth[1]/100)
    m2<-round(input$accountSize*input$expectedGrowth[2]/100)
    g1<-round(input$weeklyIncomeGoal/(input$expectedGrowth[1]/100))
    g2<-round(input$weeklyIncomeGoal/(input$expectedGrowth[2]/100))
    txt<-"<h4>If You Hit Your SLOW Target For Account Growth:</h4><ul>"
    txt<-paste0(txt,"<li>It will take around ",t1," weeks to reach your weekly salary goal.")
    txt<-paste0(txt,"<li>If you grow your account at ",input$expectedGrowth[1],"% each week, your target account size is $",g1,".")
    txt<-paste0(txt,"<li>In the first week, you'll need to make between $",m1,".</ul>")
    txt<-paste0(txt,"<h4>If You Hit Your FAST Target For Account Growth:</h4><ul>")
    txt<-paste0(txt,"<li>It will take around ",t2," weeks to reach your weekly salary goal.")
    txt<-paste0(txt,"<li>If you grow your account at ",input$expectedGrowth[2],"% each week, your target account size is $",g2,".")
    txt<-paste0(txt,"<li>In the first week, you'll need to make between $",m2,".</ul>")
    HTML(txt)
  })
  
  observe({
    req(input$accountSize>0 & input$expectedGrowth[1]>0 & input$weeklyIncomeGoal>0 & !is.na(input$lowHigh1))
    if (as.numeric(input$dayWeek)==1) dw<-"d" else dw<-"w"
    t1<-ceiling(fct_timeForGrowth(input$accountSize,input$expectedGrowth[as.numeric(input$lowHigh1)]/100,input$weeklyIncomeGoal,dw,input$friday))
    i1<-as.vector(sapply(1:t1,function(x) fct_expGrowth(input$accountSize,input$expectedGrowth[as.numeric(input$lowHigh1)]/100,x,dw,input$friday)))
    if (as.numeric(input$dayWeek)==1) {
      r$incomeTable<-data.frame("Days"=1:t1,"BeginningAmount"=c(input$accountSize,round(i1[-length(i1)])),"Gain"=diff(c(input$accountSize,round(i1))),"EndAmount"=round(i1))      
    } else {
      r$incomeTable<-data.frame("Weeks"=1:t1,"BeginningAmount"=c(input$accountSize,round(i1[-length(i1)])),"Gain"=diff(c(input$accountSize,round(i1))),"EndAmount"=round(i1))  
    }
    
  })


  output$incomeTable<-DT::renderDataTable(r$incomeTable,extensions=c('Scroller'),
                                                options = list(dom = 'Bfrtip',
                                                               scrollY = 500,
                                                               scroller = TRUE,
                                                               scrollX = TRUE),rownames = FALSE)

  output$accountGrowth<- renderPlot({
    req(!is.na(input$dayWeek) & input$weeklyIncomeGoal>0 & input$accountSize>0 & !is.na(input$friday))
    if (as.numeric(input$dayWeek)==1) dw<-"d" else dw<-"w"
    MOD<-4+input$friday
    XtraDays<-3
    t1<-ceiling(fct_timeForGrowth(input$accountSize,input$expectedGrowth[1]/100,input$weeklyIncomeGoal,dw,input$friday))
    t2<-ceiling(fct_timeForGrowth(input$accountSize,input$expectedGrowth[2]/100,input$weeklyIncomeGoal,dw,input$friday))
    tLow<-fct_timeForGrowth(input$accountSize,input$expectedGrowth[1]/100,input$weeklyIncomeGoal,dw,input$friday)
    tHigh<-fct_timeForGrowth(input$accountSize,input$expectedGrowth[2]/100,input$weeklyIncomeGoal,dw,input$friday)
    i1<-sapply(1:(t1+XtraDays),function(x) fct_expGrowth(input$accountSize,input$expectedGrowth[1]/100,x,dw,input$friday))
    i2<-sapply(1:(t1+XtraDays),function(x) fct_expGrowth(input$accountSize,input$expectedGrowth[2]/100,x,dw,input$friday))
    g1<-input$weeklyIncomeGoal/(input$expectedGrowth[1]/100)
    g2<-input$weeklyIncomeGoal/(input$expectedGrowth[2]/100)
    if (as.numeric(input$dayWeek)==1) {  
      plot(c(0,t1),c(0,g1*3),type="n",main="Account Growth Over Time",ylab="Account Size ($)",xaxt="n",xlab=NA)
      xloc<-1:ceiling(1.05*tLow)
      xloc<-xloc[xloc%%MOD==0]
      axis(1,at=xloc,labels = paste("Week",1:length(xloc)),las=2)
    } else {
      plot(c(0,t1),c(0,g1*3),type="n",main="Account Growth Over Time",ylab="Account Size ($)",xaxt="n",xlab="Weeks")
    }
    # Day
      # tLow<-ceiling(fct_timeForGrowth(input$accountSize,input$expectedGrowth[1]/100,input$weeklyIncomeGoal)*(4+input$friday))
      # iLow<-sapply(1:tLow,function(x) fct_expGrowth(input$accountSize,input$expectedGrowth[1]/100,x,"d",input$friday))
      # tHigh<-ceiling(fct_timeForGrowth(input$accountSize,input$expectedGrowth[2]/100,input$weeklyIncomeGoal)*(4+input$friday))
      # iHigh<-sapply(1:tLow,function(x) fct_expGrowth(input$accountSize,input$expectedGrowth[2]/100,x,"d",input$friday))
      # 
      # t1<-fct_timeForGrowth(input$accountSize,input$expectedGrowth[1]/100,input$weeklyIncomeGoal)*(4+input$friday)
      # t2<-fct_timeForGrowth(input$accountSize,input$expectedGrowth[2]/100,input$weeklyIncomeGoal)*(4+input$friday)
      
      #Account Size vs. Time
      abline(h=c(g1,g2),col=c("red","green"))
      text(0.5*tHigh,c(g1,g2),
           labels = c(paste0("slow target: $",round(g1)),
                      paste0("fast target: $",round(g2))),
           col=c("red","green"),pos=3)
      print(length(c(0:(t1+XtraDays),(t1+XtraDays):0)))
      print(length(c(input$accountSize,i1,i2[order(-i2)],input$accountSize)))
      polygon(c(0:(t1+XtraDays),(t1+XtraDays):0),c(input$accountSize,i1,i2[order(-i2)],input$accountSize),col = "lightblue")
      lines(c(0:(t1+XtraDays)),c(input$accountSize,i1),col = "red",lwd=2)
      lines(c(0:(t1+XtraDays)),c(input$accountSize,i2),col = "green",lwd=2)
      text(ceiling(0.75*tLow),0.4*g1,"Low Growth",col="red")
      text(ceiling(0.25*tHigh),0.7*g2,"High Growth",col="green")
      abline(v=c(tLow,tHigh),col=c("red","green"))
      if (as.numeric(input$dayWeek)==1) {  
        text(c(tLow,tHigh),2.6*g1,
           labels = c(paste0("slow target: ",round(tLow,digits=1)," days"),paste0("fast target: ",round(tHigh,digits=1)," days")),pos = 2,las=2,col=c("red","green"), srt=90)
      } else {
        text(c(tLow,tHigh),2.6*g1,
             labels = c(paste0("slow target: ",round(tLow,digits=1)," weeks"),paste0("fast target: ",round(tHigh,digits=1)," weeks")),
             pos = 2,las=2,col=c("red","green"), srt=90)
      }
      points(c(tLow,tHigh),c(g1,g2),pch=20,col="blue")
  })
  
  # output$tradeSize<- plotOutput({
  #   # Trade Size vs. Number of Trades, lines = Target Percent Gains
  #   if (as.numeric(input$dayWeek)==1) {  # Day
  #     #Account Size vs. Time
  #     
  #   } else { # Week
  #     
  #   }
  #   
  # })

  
  # output$dNumTrades<- renderUI({
  #   numericInput(ns("numTrades"),"How many trades do you plan to trade each week?",value = r$numTrades,step=1,min = 1,max=25)
  # })
  output$dPercentGain<- renderUI({
    print("pGain")
    sliderInput(ns("percentGain"),label = "What's your expected gain in each trade (%)?",
                value = 20,step = 1,min = 1,max=100)
  })
  output$dPercentWager<- renderUI({
    print("pWager")
    sliderInput(ns("percentWager"),label = "How much of your account do you want to use in each trade (%)?",
                value = 10,step = 1,min = 1,max=100)
  })
  

  output$tradeMessage<- renderUI({
    req(nrow(r$incomeTable)>0 & input$percentGain>0)
    print("trademessage")
    nT<-round(r$incomeTable$Gain[1]/((input$percentGain/100)*(input$percentWager/100)*input$accountSize))
    if (as.numeric(input$dayWeek)==1) {
      txt<-paste("<h4>Rounding up: You must have at least",nT,"successful trades to hit your daily goal.</h4>")
    } else {
      txt<-paste("<h4>Rounding up: You must have at least",nT,"successful trades to hit your weekly goal.</h4>")
    }
    txt<-paste0(txt,"<h4>The table below shows number of trades if you're a little below or above your expected profit per trade.")
    HTML(txt)
  })
  
  
  output$tradePlan<-DT::renderDataTable({
    req(nrow(r$incomeTable)>0)
    print("tplan")
    gainRange<-round((input$percentGain)*c(0.75,1,1.25))
    print(gainRange)
    tradeSize<-(input$percentWager/100)*input$accountSize
    print(tradeSize)
    perTradeProfit<-gainRange/100*tradeSize
    print(perTradeProfit)
    print(r$incomeTable)
    tradePlan<-data.frame(P1=ceiling(r$incomeTable$Gain[1]/perTradeProfit[1]),
                            P2=ceiling(r$incomeTable$Gain[1]/perTradeProfit[2]),
                            P3=ceiling(r$incomeTable$Gain[1]/perTradeProfit[3]))
    names(tradePlan)<-paste0(gainRange,"%")
    print("data.frame")
    tradePlan
    },options = list(dom="t"),rownames = FALSE)
  
  output$dEmail<- renderUI({
    print("email")
    textInput(ns("email"),label = "Enter Email to Save Your Analysis (optional)",value = "")
  })
  
  observeEvent(input$saveData,{
    print("save")
    values<<-lapply(reactiveValuesToList(input), unclass)
    print(values)
    if (grepl("@",tolower(input$email)) & grepl("\\.",tolower(input$email)) & nchar(tolower(input$email))>5) { 
      SaveData=data.frame(email=tolower(input$email),accountSize=input$accountSize,percentGain=input$percentGain,percentWager=input$percentWager,
                 expectedGrowthLow=input$expectedGrowth[1],expectedGrowthHigh=input$expectedGrowth[2],
                 weeklyIncomeGoal=input$weeklyIncomeGoal,friday=input$friday,
                 timeStamp=as.character(Sys.time()))
      print(paste(input$email,input$accountSize,Sys.time()))
      r$saveData<-r$saveData+1
      r$saveTime<-Sys.time()
      googlesheets4::sheet_append(data = SaveData,ss="1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "TradePlanData")
      r$emailFlag<-TRUE
    } else {
      r$emailFlag<-FALSE
    }
  })
  
  observeEvent(input$loadData,{
    print("load")
    if (grepl("@",tolower(input$email)) & grepl("\\.",tolower(input$email)) & nchar(tolower(input$email))>5) { 
      lapply(names(values),function(x) session$sendInputMessage(x, list(value = values[[x]])))
      r$loadTime<-Sys.time()
      LoadData<-googlesheets4::read_sheet(ss = "1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "TradePlanData")
      rows<-which(LoadData$email==input$email) 
      if (length(rows)>0) {
        r$badLoad=FALSE
        i<-rows[length(rows)]
        r$email<-LoadData$email
        r$accountSize<-LoadData$accountSize
        r$percentGain<-LoadData$percentGain
        r$percentWager<-LoadData$percentWager
        r$expectedGrowth<-c(LoadData$expectedGrowth[1],LoadData$expectedGrowth[2])
        r$weeklyIncomeGoal<-LoadData$weeklyIncomeGoal
        r$friday<-as.logical(LoadData$friday)
        # r$numTrades<-LoadData$numTrades
        print(paste(LoadData$email,LoadData$accountSize,LoadData$timeStamp))
        r$loadData<-input$loadData
        r$emailFlag<-TRUE
      } else {
        r$badLoad=TRUE
      }
    } else {
      r$emailFlag<-FALSE
    }
  })

  output$dataMessage <- renderUI({
    if (r$badLoad & r$saveTime<r$loadTime) {
      h5("We're not finding that email in the data. Try again, or configure your Trade Plan again.")
    } else if (r$saveData>0 & r$emailFlag & r$saveTime>r$loadTime) {
      h5("Data Saved")
    }  else if (r$loadData>0 & r$emailFlag & r$saveTime<r$loadTime) {
      h5("Data Loaded")
    } else if (r$emailFlag & (r$loadData+r$saveData)>0) {
      h5("Doesn't look like an email")  
    } else {
      NULL
    }
  })
  
  # json<-gargle:::secret_read("trademgmt","gargle-testing.json")
  # # # gargle:::token_fetch(  path = rawToChar(json) )
  # googlesheets4::gs4_auth(
  #   email = gargle::token_email(gargle:::token_fetch(  path = rawToChar(json) )),
  #   path = rawToChar(json) ,
  #   cache = gargle::gargle_oauth_cache(),
  #   use_oob = gargle::gargle_oob_default()
  # )
  # Topics<-googlesheets4::read_sheet(ss = "1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "Topics")

  # output$sClick<-renderUI({
  #   if (r$s<1) {
  #     actionButton(ns("uploadStory"),label = "Submit!")
  #   } else {
  #     NULL
  #   }
  # })
  # 
  # observeEvent(input$uploadStory,{
  #   print(input$uploadStory)
  #   r$uploadStory<-input$uploadStory
  #   story<-input$userStory
  #   if (nchar(story)>0) {
  #     Notes<-data.frame(User = input$noteEmail,Comment = story,Timestamp = Sys.time())
  #     googlesheets4::sheet_append(data = Notes,ss="1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "Notes")
  #     story<-""
  #     #hide("uploadStory")
  #     #utils_disableActionButton("uploadStory",session)
  #   }
  #   r$s<-1
  # })
  # 
  # output$resultsLegend<-DT::renderDataTable(r$DataTable,extensions=c('Scroller'),
  #                                           options = list(dom = 'Bfrtip',
  #                                                          scrollY = 500,
  #                                                          scroller = TRUE,
  #                                                          scrollX = TRUE),rownames = FALSE)
  
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
 
