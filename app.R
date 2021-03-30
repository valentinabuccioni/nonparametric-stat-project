library(shiny)
library(dplyr)
library(DT) #data table
library(shinydashboard)
library(mgcv) #gam



#options(shiny.error = browser) #for debugging
# load files: we need to have all our .rda files in the same folder
load("insurance.rda")


### Dashboard page #### 

ui <- dashboardPage(
  dashboardHeader(title = "US Health Insurance Calculator"),
  dashboardSidebar( #Dashboard sidebar #####
    sliderInput(
      'age', 'What is your age?', min = 18, max = 64, value = 41 #usa slider
    ),
    selectizeInput(
      'sex', 'What is your sex?', choices = c("Female" = 0,"Male" = 1),
      options = list(
        placeholder = 'Shows 0 if Female, 1 if Male',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),  
    sliderInput(
      'bmi', 'What is your body mass index?', min = 16,max = 53, value = 34.5, step=0.2 
    ),
    sliderInput(
      'steps', 'How many steps do you take daily?', min = 3000, max = 10000, value = 6000,step = 1000 
    ),
    selectizeInput(
      'smoker', 'Do you smoke?', choices = c("Non smoker" = 0,"Smoker" = 1),
      options = list(
        placeholder = 'Type 0 if non smoker, 1 if smoker',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    selectizeInput(
      'insuranceclaim', 'Did you make a insurance claim last year?', choices = c("No" = 0,"Yes" = 1),
      options = list(
        placeholder = 'Shows 0 if no, 1 if yes',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    selectizeInput(
      'coverage', 'Is it a single or a family coverage?', choices = c("Single" = 0,"Family" = 1),
      options = list(
        placeholder = 'Shows 0 if is single, 1 if is family',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    sliderInput(
      'charges', 'What is your insurance premium?', min = 1000,max = 64000,value = 32500, step= 200, #what if it is lower or higher?
    ),
    actionButton("add", "Check your premium"),
    actionButton("remove", "Clear")
  ),
  dashboardBody( #Dashboard body #####
    tags$head( #this part is HTML code in order to style disclaimer text output
      tags$style(
        HTML(
        "
        #disclaimer {
        font-family:  'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif;
        font-size: 14px;
        max-width: 100%;
        padding: 5px 15px; 
        white-space: pre-wrap;
        }

        "
        )
      )
    ),
    HTML('<meta name="viewport" content="width=1024">'), #This HTML code forces desktop view on mobile phones
    fluidRow(
      valueBoxOutput("chargesoutput", width = 12),
    ),
    fluidRow(
      valueBoxOutput("chargespredicted", width = 12),
    ),
    fluidRow(
      valueBoxOutput("comments", width = 12),
      uiOutput("smokercond"), #defines the reactive object that includes a condition. If this condition is met, a ui object is created
      uiOutput("overwcond"),
      uiOutput("obesecond")
    ),
    fluidRow(
      box(title = "Your submitted info",
          solidHeader = T,
          width = 12,
          collapsible = T,
          div(DT::DTOutput("ing_df"), style = "font-size: 70%;")),
    ), 
    fluidRow(
      textOutput("disclaimer"),
    )#last row
  )#body

)#page


### Definition of server logic required #######

server <- function(input, output, session) {
  
  # make reactive to store input
  ing_df <- shiny::reactiveValues() #creates a reactive object with no values
  
  ing_df$df <- data.frame("age" = numeric(), 
                          "sex" = numeric(), 
                          "bmi" = numeric(), 
                          "steps" = numeric(),
                          "smoker" = numeric(),
                          "insuranceclaim" = numeric(),
                          "coverage" = numeric(),
                          "charges" = numeric(),
                          stringsAsFactors = F) 
  ing_df$predict <-data.frame("prediction" = numeric(),
                              "lower" = numeric(),
                              "upper" = numeric(),
                              stringsAsFactors = F)
  

  # removes previously stored data if remove button is clicked
  observeEvent(input$remove, {
    isolate({
      ing_df$df<-ing_df$df[-(nrow(ing_df$df)),]
      ing_df$predict<-ing_df$predict[-(nrow(ing_df$predict)),]})
  })
  
  # stores input data if check premium button is clicked
  observeEvent(input$add, {
    isolate(ing_df$df[nrow(ing_df$df) + 1,] <- c(input$age,
                                                 input$sex,
                                                 input$bmi,
                                                 input$steps,
                                                 input$smoker,
                                                 input$insuranceclaim,
                                                 input$coverage,
                                                 input$charges))
    
  })
  
  #table your info is submitted
  output$ing_df <- DT::renderDataTable(ing_df$df, 
                                       rownames=F, options = list(pageLength = 5))
  
  #disclaimer text
  output$disclaimer <- renderText(paste0("This content is provided for informational purposes only and is not intended as, and should not be construed to be, legal, financial, medical or consulting advice.     "))
  
  # value boxes
  #First value box shows charges#####
  output$chargesoutput <- renderValueBox({
    if(nrow(ing_df$df) > 0){ #shows output only if info is submitted
      valueBox(paste0("Charges: $",ing_df$df[nrow(ing_df$df),8]), 
             "That's how much you pay for your premium", icon = icon("file-invoice-dollar"), color = "yellow")
    } else {
      valueBox("Submit your info", "Open the sidebar on the left", icon = icon("exclamation-triangle"), color = "green")
    }
  }) 
  
  #Second value box shows predicted charges#####
  output$chargespredicted <- renderValueBox({
    
    if(nrow(ing_df$df) > 0){ #shows output only if info is submitted
      attach(insurance)
      #gam model
      insurance$smoker=as.factor(insurance$smoker)
      insurance$coverage=as.factor(insurance$coverage)
      insurance$insuranceclaim=as.factor(insurance$insuranceclaim)
      
      model=gam(charges~s(age,bs='cr')+bmi+steps+smoker+s(bmi,bs='cr',by=smoker)
                +s(steps,bs='cr',by=smoker,k=5)+s(age,bs='cr',by=coverage)+s(bmi,bs='cr',by=coverage))
      
      detach(insurance)
      #user data to make prediction on
      dataneeded <-ing_df$df[nrow(ing_df$df),] %>%
        select(-sex) %>%
        select(-insuranceclaim)
      pr <- t(data.matrix(as.numeric(as.character(dataneeded))))
    
      #prediction gam
      set.seed(100)
      newdata=data.frame(age = pr[1,1],bmi = pr[1,2],steps = pr[1,3],smoker=pr[1,4],coverage =pr[1,5],charges =pr[1,6])
      previsione=predict.gam(model, newdata, se.fit = TRUE)
      alpha=0.05
      lwr=previsione$fit-previsione$se.fit*qt(1-(alpha/2),nrow(insurance))
      upr=previsione$fit+previsione$se.fit*qt(1-(alpha/2),nrow(insurance))
      #storing prediction in a reactive object
      isolate(ing_df$predict[nrow(ing_df$predict) + 1,] <- c(previsione[[1]],
                                                           lwr,
                                                           upr))
      
      
      
      #prints input user in the console (for debugging)
      cat(file=stderr(), "valore age", pr[1,1], "\n")
      cat(file=stderr(), "valore bmi", pr[1,2], "\n")
      cat(file=stderr(), "valore steps", pr[1,3], "\n")
      cat(file=stderr(), "valore smoker", pr[1,4], "\n")
      cat(file=stderr(), "valore coverage", pr[1,5], "\n")
         
      #prints prediction in the console (for debugging)
      cat(file=stderr(), "valore predicted", ing_df$predict[nrow(ing_df$predict),1], "\n") #debugging:il valore di pred lo calcola ing_df$predict[1,1]
      cat(file=stderr(), "valore lower", ing_df$predict[nrow(ing_df$predict),2], "\n")
      cat(file=stderr(), "valore upper", ing_df$predict[nrow(ing_df$predict),3], "\n")

      #Second value box displayed
      valueBox(HTML(paste0("Predicted charges: $", round(ing_df$predict[nrow(ing_df$predict),1],digits = 2))),
               "That's how much you should pay for your premium, based on our data", 
                icon = icon("hand-holding-usd"), 
                ifelse(type.convert(input$charges)>ing_df$predict[nrow(ing_df$predict),2] & type.convert(input$charges)<ing_df$predict[nrow(ing_df$predict),3], #continues if input charges is inside the prediction interval, else red
                        ifelse(type.convert(input$charges)>ing_df$predict[nrow(ing_df$predict),1],
                               "orange",
                        "green"), #green if charges is smaller than predicted, else orange
                "red")) 
     
      } else { #info has not been submitted yet
        valueBox("", "", color = "green")
      }

  }) #end second box
  
  
  
  #Third value box shows comment on the result####
  output$comments <- renderValueBox({
    if(nrow(ing_df$df) > 0){ #shows output only if info is submitted
      valueBox(ifelse(type.convert(input$charges)>ing_df$predict[nrow(ing_df$predict),1], 
                    HTML("You should pay less!"), 
                    HTML( "Congratulations for your premium!")),
               ifelse(type.convert(input$charges)>ing_df$predict[nrow(ing_df$predict),1], 
                      HTML("Maybe your insurance plan is not the most convenient one. In this case, search for a better alternative. Otherwise it may be that your insurance plan has a greater coverage than ours, according to your medical needs. Therefore we cannot investigate further on its price fairness"), 
                      HTML("You may have a lower price because maybe your health insurance plan does not cover you as it should, according to your medical needs. If there is nothing wrong, then congratulations on your great deal!")),
                    icon = icon("piggy-bank"), 
               ifelse(type.convert(input$charges)>ing_df$predict[nrow(ing_df$predict),1], 
                      "red", 
                      "green"))
      
    } else {
      valueBox("", "", color = "green")
    }
    
  })
  
  #Requirements for conditional suggestions####
  output$smokercond <- renderUI({ #if user is a smoker
    req(nrow(ing_df$df) > 0 & ing_df$df[nrow(ing_df$df),5]>0 )
    valueBoxOutput("smokercomment", width = 12)
  })
 
  output$overwcond <- renderUI({ #if user is overweight
    req(nrow(ing_df$df) > 0 & ing_df$df[nrow(ing_df$df),3]>24.9 & ing_df$df[nrow(ing_df$df),3]<30.0  )
    valueBoxOutput("overwcomment", width = 12)
  })
    
  output$obesecond <- renderUI({ #if user is obese
    req(nrow(ing_df$df) > 0 & (ing_df$df[nrow(ing_df$df),3]>=30.0))  
    valueBoxOutput("obesecomment", width = 12)
  })
    
     
  #Value boxes that are displayed if conditions are met####
  
  output$smokercomment <- renderValueBox({ #it is shown only if req smoker is met
    
    #evaluating prediction in case smoker=0
    attach(insurance)
    #gam model
    insurance$smoker=as.factor(insurance$smoker)
    insurance$coverage=as.factor(insurance$coverage)
    insurance$insuranceclaim=as.factor(insurance$insuranceclaim)
    
    model=gam(charges~s(age,bs='cr')+bmi+steps+smoker+s(bmi,bs='cr',by=smoker)
              +s(steps,bs='cr',by=smoker,k=5)+s(age,bs='cr',by=coverage)+s(bmi,bs='cr',by=coverage))
    
    detach(insurance)
    #user data to make prediction on
    dataneeded <-ing_df$df[nrow(ing_df$df),] %>%
      select(-sex) %>%
      select(-insuranceclaim)
    pr <- t(data.matrix(as.numeric(as.character(dataneeded))))
    
    set.seed(100)
    newdata=data.frame(age = pr[1,1],bmi = pr[1,2],steps = pr[1,3],smoker=0,coverage =pr[1,5],charges =pr[1,6])
    previsionesmoker=predict.gam(model, newdata)
    valueBox(HTML("Suggestion"), HTML(paste0("Smoking significantly increases average premium. You would pay $",round(previsionesmoker,digits = 2), " if you did not smoke. Ask for medical advice in case you want to save money and quit smoking ")),icon = icon("smoking"), color = "purple")
  })
    
  output$overwcomment <- renderValueBox({ #it is shown only if req overwcond is met
    #evaluating prediction in case bmi=21.6
    attach(insurance)
    #gam model
    insurance$smoker=as.factor(insurance$smoker)
    insurance$coverage=as.factor(insurance$coverage)
    insurance$insuranceclaim=as.factor(insurance$insuranceclaim)
    
    model=gam(charges~s(age,bs='cr')+bmi+steps+smoker+s(bmi,bs='cr',by=smoker)
              +s(steps,bs='cr',by=smoker,k=5)+s(age,bs='cr',by=coverage)+s(bmi,bs='cr',by=coverage))
    
    detach(insurance)
    #user data to make prediction on
    dataneeded <-ing_df$df[nrow(ing_df$df),] %>%
      select(-sex) %>%
      select(-insuranceclaim)
    pr <- t(data.matrix(as.numeric(as.character(dataneeded))))
    
    set.seed(100)
    newdata=data.frame(age = pr[1,1],bmi = 21.6 ,steps = pr[1,3],smoker=pr[1,4],coverage =pr[1,5],charges =pr[1,6])
    previsioneoverweight=predict.gam(model, newdata)
    valueBox(HTML("Suggestion"), HTML(paste0("Your body mass index indicates you are overweight: you would pay $",round(previsioneoverweight, digits = 2)," if you had a body mass index in the normal weight category. Ask for medical advice in case you want to reduce your premium and body mass index")),icon = icon("user-md"), color = "light-blue")
  })
  
  output$obesecomment <- renderValueBox({ #it is shown only if req obese is met
    #evaluating prediction in case bmi=21.6
    attach(insurance)
    #gam model
    insurance$smoker=as.factor(insurance$smoker)
    insurance$coverage=as.factor(insurance$coverage)
    insurance$insuranceclaim=as.factor(insurance$insuranceclaim)
    
    model=gam(charges~s(age,bs='cr')+bmi+steps+smoker+s(bmi,bs='cr',by=smoker)
              +s(steps,bs='cr',by=smoker,k=5)+s(age,bs='cr',by=coverage)+s(bmi,bs='cr',by=coverage))
    
    detach(insurance)
    #user data to make prediction on
    dataneeded <-ing_df$df[nrow(ing_df$df),] %>%
      select(-sex) %>%
      select(-insuranceclaim)
    pr <- t(data.matrix(as.numeric(as.character(dataneeded))))
    
    set.seed(100)
    newdata=data.frame(age = pr[1,1],bmi = 21.6 ,steps = pr[1,3],smoker=pr[1,4],coverage =pr[1,5],charges =pr[1,6])
    previsioneobese=predict.gam(model, newdata)
    valueBox(HTML("Suggestion"), HTML(paste0("Your body mass index indicates you are obese: you would pay $",round(previsioneobese, digits = 2)," if you had a body mass index in the normal weight category. Ask for medical advice in case you want to reduce your premium and body mass index")),icon = icon("user-md"), color = "light-blue")
  })
    
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)



