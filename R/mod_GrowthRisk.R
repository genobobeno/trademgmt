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
        fluidRow(column(3,
                        shinydashboard::box(title = "Your Account Details:",width = 12,#height = "250px",
                                            h3("Let's start with the basics."),
                                            h5("A good plan requires achievable goals. And you should know the parameters that will affect your account. First, the size."),
                              uiOutput(ns("dAccountSize")), #numericInput(ns("accountSize"),label = "Size of Current Account:",value = 25000,min = 100,max = 5000000,step=100),
                              h5("Next, every week will be different. So what range of growth do you expect to achieve each week?"),
                              uiOutput(ns("dExpectedGrowth")), #sliderInput(ns("expectedGrowth"),label = "The range of expected weekly profit (%):",min = 0,max = 100,step = .05,value = c(15,25)),
                              h5("And here's where your goals should be specific and achievable: How much money do you want to make each week?"),
                              uiOutput(ns("dWeeklyIncomeGoal")), #numericInput(ns("weeklyIncomeGoal"),label = "What is your weekly income goal?",value = 5000,min = 1000,max = 200000,step = 100)),
                              h5("So when it comes to your plan, do you want to see the numbers in the calculations broken down by days or weeks?"),
                              radioButtons(ns("dayWeek"),label = "Do you want to see goals by Day or Week?",choices = list("Day"=1,"Week"=2),selected = 1),
                              h5("Some traders don't like to trade on Fridays (bad volume), so make your choice!"),
                              uiOutput(ns("dFriday")) #checkboxInput(ns("friday"),label = "Do you intend to trade on Fridays?",value = TRUE)
                        ),
                        shinydashboard::box(title = "Save Data?",width = 12,
                                            h3("If you like the plan you're seeing and you'd like to come back to this site and review it, I've included a way for you to save your inputs and reload them later."),
                              uiOutput(ns("dEmail")), #textInput(ns("email"),"Enter Email to Save Your Analysis (optional))
                              actionButton(ns("saveData"),label = "Save Data",icon = icon("upload")),
                              actionButton(ns("loadData"),label = "Load Data",icon= icon("download")),
                              hr(),
                              uiOutput(ns("dataMessage")) # h5("Data Saved"), h5("Data Loaded"), h5("Doesn't look like an email")
                        )),
                 column(4,
                        shinydashboard::tabBox(title = "Some Calculations",width = 12,#height = "250px",
                                 tabPanel(title = "Expectations",
                                          uiOutput(ns("calcSummary")),
                                          # uiOutput(ns("timeCalc")),
                                          # uiOutput(ns("weeklyAmountCalc"))
                                          hr(),
                                          h3("Here, we're breaking out your weekly or daily goals if you strictly follow your expected growth trajectory."),
                                          h5("NOTE: We're allowing for some variation here. We'll have good weeks and bad weeks. Below, you can see the difference between your low growth weeks and high growth weeks."),
                                          uiOutput(ns("dLowHigh1")), #radioButtons(ns("lowHigh1"),label = "Do you want low or high win rate estimates?",choices = list(""))
                                          DT::dataTableOutput(ns("incomeTable"))
                                          # DT::dataTableOutput(ns("weeklyIncomeTable"))
                                          # uiOutput(ns("dLowHigh2")), #radioButtons(ns("lowHigh2"),label = "Do you want low or high win rate estimates?",choices = list(""))
                                          # DT::dataTableOutput(ns("dailyIncomeTable"))
                                 ),
                                 tabPanel(title="Trade Plan",
                                          # uiOutput(ns("dNumTrades")), #numericInput(ns("numTrades"),"How many trades do you plan to trade each week?",value = 5,step=1,min = 1,max=25),
                                          h3("In this box, we're trying to determine how many trades you should make given the average profit of each trade in your edge."),
                                          h5("So we need to know two things: How much of your account will you risk, and what percent do you expect to gain on that one trade."),
                                          uiOutput(ns("dPercentWager")), #sliderInput(ns("percentWager"),"How much of your account do you want to use in each trade (%)?",value = 10,step = 1,min = 1,max=100),
                                          uiOutput(ns("dPercentGain")), #sliderInput(ns("percentGain"),"What's your expected gain in each trade (%)?",value = 20,step = 1,min = 1,max=100),
                                          hr(),
                                          uiOutput(ns("tradeMessage")), # h4(txt)
                                          DT::dataTableOutput(ns("tradePlan")),
                                          hr(),
                                          uiOutput(ns("planStatement")) # h4(txt)
                                          )
                        )),
                column(5,
                        shinydashboard::box(title = "The Math:",width = 12,#height = "250px",
                                            h3("The plot here is a nice visual of your overall timeline."),
                                            h5("NOTE: Again, this assumes you're meeting the weekly expectations set forth in the plan."),
                                            h5("Feel free to right-click on this graph and save it for yourself."),
                              plotOutput(ns("accountGrowth"))#, #Account Size vs. Time
                              #renderPlot(ns("tradeSize")) # Trade Size vs. Number of Trades, lines = Target Percent Gains
                      ))
        )),
        
        #              uiOutput(ns("email2")),
        #              uiOutput(ns("comment2")),
        #              uiOutput(ns("sClick")),
        #              uiOutput(ns("thanks"))
        #              )
        #     )
    conditionalPanel("input.tradeMgmtMenu=='risk'",
                     shinydashboard::box(title = "Edge Estimates",width = 3,
                                         h3("Risk Management is likely more important than your growth plan."),
                                         h5("Here, you should already have some idea of your edge. Do you win 50%, 60%, or maybe even 90% of your trades?"),
                                         sliderInput(ns("winRate"),label = "Success Rate (%)",min = 0,max = 100,step=1,value=75),
                                         h5("Also, your edge should include a typical reward-to-risk ratio. Are you looking to gain $2 for every $1 of risk?"),
                                         sliderInput(ns("rewardRisk"),label = "Reward to Risk Ratio?", min=0, max=5, step=0.1, value = 2),
                                         h5("And finally, how much of your account will you risk in each trade?"),
                                         sliderInput(ns("riskAmount"),label = "Risk in Each Trade (%)?", min=0, max=50, step=0.1, value = 10)
                     ),
                     shinydashboard::box(title = "Let's Run Some Samples",width=3,
                                         h3("Given the probabilities you just set up, we can run some simulations!"),
                                         h5("Let's start again with your account size."),
                                         numericInput(ns("initialSize"),label = "Account Size?",value = 100000,min = 5000,max = 1000000,step = 1000),
                                         h5("How much of that account, in dollars, to intend to position in each trade?"),
                                         uiOutput(ns("dTradeSize")), #numericInput(ns("tradeSize"),label = "Amount of Each Trade?",value = 100000,min = 5000,max = 1000000,step = 1000),
                                         h5("And finally, here is the work of a professional trader... how many trades do you want to simulate given the edge probabilities you've defined?"),
                                         numericInput(ns("sampledTrades"),label = "Number of Trades to Simulate",value = 100,min = 1,max=1000),
                                         actionButton(ns("runTrades"),label = "Run Simulation!")),
                     shinydashboard::box(title = "Your Account",width=6,
                                         h3("These are 10 randomly chosen trajectories of the simulation."),
                                         h5("Each line is a progression, showing how your account may grow or decay."),
                                         plotOutput(ns("tradeSeries")), # paths
                                         hr(),
                                         h5("Assuming all the details you've entered, this probability distribution shows the possible outcomes."),
                                         h5("NOTE: Even with an edge that feels 'positive', there can still be a significant probability that you end up with zero account growth!"),
                                         plotOutput(ns("distribution"))) # possible account results
                     # plotOutput(ns("currentVotes")),
                     # DT::dataTableOutput(ns("resultsLegend")))
    # conditionalPanel(condition="(input.uploadStory>0 | input.uploadVote>0) & input.surveyMenu!='results'",
    #                  h3("Thank You For The Sentiments!"),
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
  set.seed(ceiling(as.numeric(Sys.time())/(rnorm(1,mean = 10)*14703))%%1000)
  r$tradeSeries<-c()

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
    nT<-ceiling(r$incomeTable$Gain[1]/((input$percentGain/100)*(input$percentWager/100)*input$accountSize))
    if (as.numeric(input$dayWeek)==1) {
      txt<-paste("<h4>Rounding up: Assuming NO losses, you must have at least",nT,"successful trades each day to hit your daily goal.</h4>")
    } else {
      txt<-paste("<h4>Rounding up: Assuming NO losses, you must have at least",nT,"successful trades each week to hit your weekly goal.</h4>")
    }
    txt<-paste0(txt,"<h4>The table below shows number of trades if you're a little below or above your expected profit per trade.")
    HTML(txt)
  })

  output$planStatement<- renderUI({
    req(nrow(r$incomeTable)>0 & input$percentGain>0)
    nT<-round(r$incomeTable$Gain[1]/((input$percentGain/100)*(input$percentWager/100)*input$accountSize),digits=2)
    if (as.numeric(input$dayWeek)==1) {
      txt<-paste0("<h4>In English: If you think you will make ",input$percentGain,"% each time you wager ",
                  input$percentWager,"% of your account, then in order to be ON TRACK to hit your weekly salary, you have to make ",
                  nT," successful trades every day. In the table and the statement above, we are rounding UP!</h4>")
    } else {
      txt<-paste0("<h4>In English: If you think you will make ",input$percentGain,"% each time you wager ",
                  input$percentWager,"% of your account, then in order to be ON TRACK to hit your weekly salary, you have to make ",
                  nT," successful trades every week. In the table and the statement above, we are rounding UP!</h4>")
    }
    txt<-paste0(txt,"<h4>NOTE: We are not accounting for any losses!</h4>")
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
    values<-lapply(reactiveValuesToList(input), unclass)
    print(values)
    values<-values[names(values) %in% c("dayWeek","friday","accountSize","weeklyIncomeGoal",
                                        "expectedGrowth","email","percentGain","percentWager")]
    if (sum(c("percentGain","percentWager") %in% names(values))<2) {
      values$percentGain<-20
      values$percentWager<-10
    }
    # SaveData=data.frame(email="eugene.quickreaction@gmail.com",accountSize=5000,percentGain=20,
    #                     dayWeek="1",percentWager=10,expectedGrowthLow=15,
    #                     expectedGrowthHigh=25,weeklyIncomeGoal=5000,
    #                     friday=TRUE,timeStamp=as.character(Sys.time()))
    # googlesheets4::write_sheet(data = SaveData,ss="1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "TradePlanData")

    if (grepl("@",tolower(input$email)) & grepl("\\.",tolower(input$email)) & nchar(tolower(input$email))>5) { 
      SaveData=data.frame(email=tolower(values$email),accountSize=values$accountSize,percentGain=values$percentGain,
                          percentWager=values$percentWager,expectedGrowthLow=values$expectedGrowth[1],
                          expectedGrowthHigh=values$expectedGrowth[2],weeklyIncomeGoal=values$weeklyIncomeGoal,
                          friday=values$friday,timeStamp=as.character(Sys.time()))
      print(paste(input$email,input$accountSize,Sys.time()))
      r$saveData<-r$saveData+1
      r$saveTime<-Sys.time()
      googlesheets4::sheet_append(data = SaveData,ss="1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "TradePlanData")
      print(r$saveData)
      r$emailFlag<-TRUE
    } else {
      r$emailFlag<-FALSE
    }
  })
  
  observeEvent(input$loadData,{
    print("load")
    if (grepl("@",tolower(input$email)) & grepl("\\.",tolower(input$email)) & nchar(tolower(input$email))>5) { 
      r$loadTime<-Sys.time()
      LoadData<-googlesheets4::read_sheet(ss = "1-4kwf6x4-zJC7JOKly-Wp4VZ47arooxO87PUTlOgI6I",sheet = "TradePlanData")
      rows<-which(LoadData$email==tolower(input$email)) 
      if (length(rows)>0) {
        r$badLoad=FALSE
        i<-rows[length(rows)]
        # SaveData=data.frame(email="eugene.quickreaction@gmail.com",accountSize=5000,percentGain=20,
        #                     dayWeek="1",percentWager=10,expectedGrowthLow=15,
        #                     expectedGrowthHigh=25,weeklyIncomeGoal=5000,
        #                     friday=TRUE,timeStamp=as.character(Sys.time()))
        iList<-list(dayWeek=as.character(LoadData$dayWeek[i]),
                    friday=as.logical(LoadData$friday[i]),
                    accountSize=LoadData$accountSize[i],
                    weeklyIncomeGoal=LoadData$weeklyIncomeGoal[i],
                    expectedGrowth=c(LoadData$expectedGrowthLow[i],LoadData$expectedGrowthHigh[i]),
                    percentGain=LoadData$percentGain[i],
                    percentWager=LoadData$percentWager[i])
        #iList<-values[names(values) %in% c("dayWeek","friday","accountSize","weeklyIncomeGoal","expectedGrowth","email","lowHigh1")]
        lapply(names(iList),function(x) session$sendInputMessage(x, list(value = iList[[x]])))
        # r$email<-LoadData$email
        # r$accountSize<-LoadData$accountSize
        # r$percentGain<-LoadData$percentGain
        # r$percentWager<-LoadData$percentWager
        # r$expectedGrowth<-c(LoadData$expectedGrowth[1],LoadData$expectedGrowth[2])
        # r$weeklyIncomeGoal<-LoadData$weeklyIncomeGoal
        # r$friday<-as.logical(LoadData$friday)
        # r$numTrades<-LoadData$numTrades
        print(paste(LoadData$email[i],LoadData$accountSize[i],LoadData$timeStamp[i]))
        r$savedDate<-LoadData$timeStamp[i]
        r$loadData<-input$loadData
        r$emailFlag<-TRUE
        r$email<-LoadData$email[i]
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
      h5(paste("Data Saved for",tolower(input$email)))
    }  else if (r$loadData>0 & r$emailFlag & r$saveTime<r$loadTime) {
      h5(paste0("Data Loaded for ",tolower(input$email),", originally saved on ",r$savedDate))
    } else if (r$emailFlag & (r$loadData+r$saveData)>0) {
      h5("Doesn't look like an email")  
    } else {
      NULL
    }
  })
  
  output$dTradeSize<- renderUI({
    numericInput(ns("tradeSize"),label = "Amount of Each Trade?",value = 10000,min = 100,max = input$initialSize,step = 100)
  })

  #sliderInput(ns("winRate"),label = "Success Rate (%)",min = 0,max = 100,step=1,value=75)
  #sliderInput(ns("rewardRisk"),label = "Reward to Risk Ratio?", min=0, max=10, step=0.1, value = 2)
  #sliderInput(ns("riskAmount"),label = "Risk Allowed in Each Trade (%)?", min=0, max=50, step=0.1, value = 10)
  #numericInput(ns("initialSize"),label = "Account Size?",value = 100000,min = 5000,max = 1000000,step = 1000)
  #numericInput(ns("sampledTrades"),label = "How Many Trades Do You Want To Simulate?",value = 100,min = 1,max=1000))


  observeEvent(input$runTrades,{
    req(input$sampledTrades>0)
    Samples<-2500
    r$aSize<-unlist(sapply(1:Samples, function(x) {
      WL<-rbinom(input$sampledTrades,size = 1,prob = input$winRate/100)
      input$initialSize+sum(WL*input$rewardRisk*(input$riskAmount/100)*input$tradeSize-(1-WL)*(input$riskAmount/100)*input$tradeSize)
    }))
    WL<-rbinom(input$sampledTrades,size = 1,prob = input$winRate/100)
    WL<-WL*input$rewardRisk*(input$riskAmount/100)*input$tradeSize-(1-WL)*(input$riskAmount/100)*input$tradeSize
    r$tradeSeries<-as.matrix(input$initialSize+cumsum(WL))
    for (i in 1:10) {
      WL<-rbinom(input$sampledTrades,size = 1,prob = input$winRate/100)
      WL<-WL*input$rewardRisk*(input$riskAmount/100)*input$tradeSize-(1-WL)*(input$riskAmount/100)*input$tradeSize
      r$tradeSeries<-cbind(r$tradeSeries,input$initialSize+cumsum(WL))
    }
  })

  output$tradeSeries<-renderPlot({
    req(input$runTrades)
    COLS<-rainbow(ncol(r$tradeSeries))
    plot(c(0,input$sampledTrades),c(0,max(r$tradeSeries)),type="n",main="Account Progression",xlab="Number of trades",ylab="Account Size")
    for (j in 1:ncol(r$tradeSeries)) lines(0:nrow(r$tradeSeries),c(input$initialSize,r$tradeSeries[,j]),col=COLS[j])
  })

  output$distribution<-renderPlot({
    req(length(r$aSize)>0)
    plot(density(r$aSize),main="Distribution of Possible Account Sizes",ylab="Probability Density",yaxt="n",xlab=paste0("Final Account Size (",input$sampledTrades," trades)"))
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
 
