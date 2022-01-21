#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinyjs)

# Define UI for application that builds a calculator to compute interest
ui <- fluidPage(
  #tags$style(type = "text/css", ".form-control.shiny-bound-input,.selectize-input {background-color:red}"),
  
  # Application title
  titlePanel(" Simple/Compound Interest Calculator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(style="width:400px",
                 fluidRow(
                   column(8,numericInput("PVAmt","Present Value", value = 0 )
                   ),
                   column(4,style="padding-top:24px",actionButton("btnCOMPUTE_PV","Compute")
                   )
                 ),
                 fluidRow(
                   column(8,numericInput("rate","Rate", value = 0 )
                   ),
                   column(4,style="padding-top:24px",actionButton("btnCOMPUTE_RATE","Compute")
                   )
                 ),
                 fluidRow(
                   column(8,numericInput("time","Time", value = 0 )
                   ),
                   column(4,style="padding-top:24px",actionButton("btnCOMPUTE_TIME","Compute")
                   )
                 ),
                 fluidRow(
                   column(8,numericInput("periods","Number of Periods", value = 0 )
                   ),
                   column(4,style="padding-top:24px",actionButton("btnCOMPUTE_PERIODS","Compute")
                   )
                 ),
                 fluidRow(
                   column(8,numericInput("Int","Interest Earned", value = 0 )
                   ),
                   column(4,style="padding-top:24px",actionButton("btnCOMPUTE_INTEREST_EARNED","Compute")
                   )
                 ),
                 fluidRow(
                   column(8,numericInput("FVAmt","Future Value", value = 0 )
                   ),
                   column(4,style="padding-top:24px",actionButton("btnCOMPUTE_FV","Compute")
                   )
                 ),
                 
                 
                 #numericInput("rate"," Rate", value = 0 ),
                 #numericInput("time","Time", value = 0 ),
                 #numericInput("periods","Number of Periods", value = 0 ),
                 #numericInput("Int","Interest earned", value = 0 ),
                 #numericInput("FVAmt","Future Value", value = 0 ),
                 selectInput(inputId = "drpMain_Simple_or_Compound", label = "Select Category",
                             choices = c("Simple Interests"=1,"Compound Interest"=2),
                             selected = 1),
                 actionButton("btnCLEAR", "Clear"),
                 div(style="height:2px;width:100%;background-color:gray;margin-top:40px"),
                 HTML("<h1>RATE CONVERSION</h1>"),
                 
                 fluidRow(
                   column(6,
                          numericInput("effectiveInterest","Effective Interest", value = 0)
                   ),
                   column(6,
                          numericInput("effectiveDiscount","Effective Discount", value = 0 )
                   )
                 ),
                 fluidRow(
                   column(6,
                          numericInput("nominalInterest","Nominal Interest", value = 0 )
                   ),
                   column(6,
                          numericInput("nominalDiscount","Nominal Discount", value = 0 )
                   )
                 ),
                 fluidRow(
                   column(6,
                          numericInput("forceInterest","Force", value = 0 )
                   ),
                   column(6,
                          numericInput("periodsConversion","Number of Periods", value = 0 )
                   )
                 ),
                 fluidRow(
                   column(12,
                          radioButtons(inputId = "Type_of_Calculation",
                                       label = "Convert From? Select below, then convert",
                                       choices = list("Effective Interest rate" = 1, "Effective Discount rate" = 2, "Nominal Interest rate" = 3, "Nominal Discount rate" = 4, "Force of Interest" = 5),
                                       selected=1)),
                   
                 ),
                 
                 actionButton("btnCONVERT", "Convert")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

#conversion functions###################################################################


#from effective interest###########

convert_from_effective_interest = function(x,compoundingPeriods){
  results = list("efInterest"=0,"efDiscount"=0,"nomInterest"=0,"nomDiscount"=0,"force"=0)
  
  #make i (the independent variable) the input (x) then calculate the rest
  
  i = x/100
  m = compoundingPeriods
  n = compoundingPeriods
  
  #effective interest
  i = i
  
  #effective discount
  d = 1 - (1+i)**-1
  
  #nominal interest
  i.m = (((1+i)**(1/m))-1)*m
  
  #nominal discount
  d.n = (1-((1+i)**(-1/n)))*n
  
  #force 
  f = log(1+i)
  
  #place calculated values in the object that we will then return
  results[['efInterest']]= i*100
  results[["efDiscount"]]=d*100
  results[["nomInterest"]]=i.m*100
  results[["nomDiscount"]]=d.n*100
  results[["force"]]=f*100
  return (results)
  
}

#from effective interest###########


#from effective discount###########

convert_from_effective_discount = function(x,compoundingPeriods){
  results = list("efInterest"=0,"efDiscount"=0,"nomInterest"=0,"nomDiscount"=0,"force"=0)
  
  #make i (the independent variable) the input (x) then calculate the rest
  
  d = x/100
  m = compoundingPeriods
  n = compoundingPeriods
  
  #effective interest
  i = ((1-d)**-1)-1
  
  #effective discount
  d = d
  
  #nominal interest
  i.m = (((1-d)**-(1/m))-1)*m
  
  #nominal discount
  d.n = (1-((1-d)**(1/m)))*m
  
  #force 
  f = -log(1-d)
  
  #place calculated values in the object that we will then return
  results[['efInterest']]= i*100
  results[["efDiscount"]]=d*100
  results[["nomInterest"]]=i.m*100
  results[["nomDiscount"]]=d.n*100
  results[["force"]]=f*100
  return (results)
  
}

#from effective discount ###########



#from nominal interest ###########

convert_from_nominal_interest = function(x,compoundingPeriods){
  results = list("efInterest"=0,"efDiscount"=0,"nomInterest"=0,"nomDiscount"=0,"force"=0)
  
  #make i (the independent variable) the input (x) then calculate the rest
  
  i.m = x/100
  m = compoundingPeriods
  n = compoundingPeriods
  
  #effective interest
  i = ((1+(i.m/m))**m)-1
  
  #effective discount
  d = 1-((1+(i.m/m))**(-m))
  
  
  #nominal interest
  i.m = i.m
  
  #nominal discount
  d.n = (1-((1+(i.m/m))**(-1)))*m
  
  #force 
  f = (log(1+(i.m/m)))*m
  
  #place calculated values in the object that we will then return
  results[['efInterest']]= i*100
  results[["efDiscount"]]=d*100
  results[["nomInterest"]]=i.m*100
  results[["nomDiscount"]]=d.n*100
  results[["force"]]=f*100
  return (results)
  
}

#from nominal interest ###########




#from nominal discount###########

convert_from_nominal_discount = function(x,compoundingPeriods){
  results = list("efInterest"=0,"efDiscount"=0,"nomInterest"=0,"nomDiscount"=0,"force"=0)
  
  #make i (the independent variable) the input (x) then calculate the rest
  
  d.n = x/100
  m = compoundingPeriods
  n = compoundingPeriods
  
  #effective interest
  i = ((1-(d.n/n))**(-n))-1
  
  #effective discount
  d = 1-(1-(d.n/n))**n
  
  #nominal interest
  i.m = ((1-(d.n/n))**(-1)-1)*m
  
  #nominal discount
  d.n = d.n
  
  #force 
  f = log(1-(d.n/n))*(-m)
  
  #place calculated values in the object that we will then return
  results[['efInterest']]= i*100
  results[["efDiscount"]]=d*100
  results[["nomInterest"]]=i.m*100
  results[["nomDiscount"]]=d.n*100
  results[["force"]]=f*100
  return (results)
  
}

#from nominal discount ###########



#from force ###########

convert_from_force= function(x,compoundingPeriods){
  results = list("efInterest"=0,"efDiscount"=0,"nomInterest"=0,"nomDiscount"=0,"force"=0)
  
  #make i (the independent variable) the input (x) then calculate the rest
  
  f = x/100
  m = compoundingPeriods
  n = compoundingPeriods
  
  #effective interest
  i = exp(f)-1
  
  #effective discount
  d = 1-exp(-f)
  
  #nominal interest
  i.m = (exp(f/m)-1)*m
  
  #nominal discount
  d.n = (1-exp(-f/n))*n
  
  #force 
  f = f
  
  #place calculated values in the object that we will then return
  results[['efInterest']]= i*100
  results[["efDiscount"]]=d*100
  results[["nomInterest"]]=i.m*100
  results[["nomDiscount"]]=d.n*100
  results[["force"]]=f*100
  return (results)
  
}

#from force ###########





#END conversion functions###################################################################




#### Compute functions##################################################
compute_missing_in_simple_interest = function(PV,FV,t,n,r,i){
  
  
  
}
#END Compute functions##################################################


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ##when compute pv button pressed
  observeEvent(input$btnCOMPUTE_PV,{
    result = ''
    
    ##collect all necessary inputs
    PV = input$PVAmt;FV = input$FVAmt;t = input$time;n = input$periods;r = input$rate/100;i = input$interest
    
    if(input$drpMain_Simple_or_Compound==1){
      ##calculate simple interest
      result = FV/(1+ (r*t))
    } else {
      ##calculate compound interest
      result = FV/((1+r)**t)
      
    }
    
    #update the value
    updateNumericInput(session, 'PVAmt',value = result) 
    
  })
  
  
  
  ##when compute fv button pressed
  observeEvent(input$btnCOMPUTE_FV,{
    result = ''
    
    ##collect all necessary inputs
    PV = input$PVAmt;FV = input$FVAmt;t = input$time;n = input$periods;r = input$rate/100;i = input$interest
    
    if(input$drpMain_Simple_or_Compound==1){
      ##calculate simple interest
      result = PV*(1+ (r*t))
    } else {
      ##calculate compound interest
      result = PV*((1+r)**t)
      
    }
    
    #update the value
    updateNumericInput(session, 'FVAmt',value = result) 
    
  })
  
  ##when compute rate button pressed
  observeEvent(input$btnCOMPUTE_RATE,{
    result = ''
    
    ##collect all necessary inputs
    PV = input$PVAmt;FV = input$FVAmt;t = input$time;n = input$periods;r = input$rate/100;i = input$interest
    
    if(input$drpMain_Simple_or_Compound==1){
      ##calculate simple interest
      result = ((FV-PV)/(PV*t))*100
    } else {
      ##calculate compound interest
      result = (((FV/PV)**(1/t))-1)*100
      
    }
    
    #update the value
    updateNumericInput(session, 'rate',value = result) 
    
  }) 
  
  
  ##when compute time button pressed
  observeEvent(input$btnCOMPUTE_TIME,{
    result = ''
    
    ##collect all necessary inputs
    PV = input$PVAmt;FV = input$FVAmt;t = input$time;n = input$periods;r = input$rate/100;i = input$interest
    
    if(input$drpMain_Simple_or_Compound==1){
      ##calculate simple interest
      result = (FV-PV)/(PV*r)
    } else {
      ##calculate compound interest
      result = (log(FV/PV))/(log(1+r))
      
    }
    
    #update the value
    updateNumericInput(session, 'time',value = result) 
    
  })
  
  
  ##when compute int button pressed
  observeEvent(input$btnCOMPUTE_INTEREST_EARNED,{
    result = ''
    
    ##collect all necessary inputs
    PV = input$PVAmt;FV = input$FVAmt;t = input$time;n = input$periods;r = input$rate/100;i = input$interest
    
    if(input$drpMain_Simple_or_Compound==1){
      ##calculate simple interest
      result = PV*r*t
    } else {
      ##calculate compound interest
      result = (PV*(1+r)**t)-PV
      
    }
    
    #update the value
    updateNumericInput(session, 'Int',value = result) 
    
  })
  
  
  #when convert button pressed
  ##check which radio button is selected then run the right function
  
  observeEvent(input$btnCONVERT,{
    
    
    results = ''
    
    if(input$Type_of_Calculation==1){
      ##if effective Interest selected
      
      input_ = input$effectiveInterest
      periods = input$periodsConversion
      results = convert_from_effective_interest(input_,periods)
      
      
      
    } else if(input$Type_of_Calculation==2){
      ##if effective Discount selected
      input_ = input$effectiveDiscount
      periods = input$periodsConversion
      results = convert_from_effective_discount(input_,periods)
      
    } else if(input$Type_of_Calculation==3){
      ##if nominal Interest selected
      input_ = input$nominalInterest
      periods = input$periodsConversion
      results = convert_from_nominal_interest(input_,periods)
      
    } else if(input$Type_of_Calculation==4){
      ##if nominal Discount selected
      input_ = input$nominalDiscount
      periods = input$periodsConversion
      results = convert_from_nominal_discount(input_,periods)
      
    } else if(input$Type_of_Calculation==5){
      ##if force selected
      input_ = input$forceInterest
      periods = input$periodsConversion
      results = convert_from_force(input_,periods)
      
    }
    
    
    #update the input boxes with calculated values
    updateNumericInput(session, "effectiveInterest",value=results$efInterest)
    updateNumericInput(session, "effectiveDiscount",value=results$efDiscount)
    updateNumericInput(session, "nominalDiscount",value=results$nomDiscount)
    updateNumericInput(session, "nominalInterest",value=results$nomInterest)
    updateNumericInput(session, "forceInterest",value=results$force)
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
