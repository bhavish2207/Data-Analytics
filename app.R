#install.packages("shiny")
library("shiny")
library(arules)  
library(arulesViz)
ui <- fluidPage(
  titlePanel("IST 707 HOMEWORK 1"),
  sidebarLayout(
    sidebarPanel(
     sliderInput(inputId =  "Support_val",
                  label =  "Support:",
                 min = 0,
                max = 1,
                  value = 0.03,step = 1/100),
      sliderInput("Confidence_val",
                 "Confidence:",
                  min = 0,
                 max = 1,
                value = 0.4,step = 1/100),
     sliderInput("minlen_val",
                 "Minimum Rule Length:",
                 min = 3,
                 max = 10,
                 value = 3,step = 1),
     sliderInput("no_rules",
                "Number of top Rules selected:",
                min = 5,
                max = 10,
                value = 5, step = 1),
       selectInput("sort_by",
                   "Sort Rules in descending order by Lift/Confidence/Support:",
                   choices = c('lift','confidence','support'),
                   selected = 'lift'
                   )
     ),
      mainPanel(
        tabsetPanel(id = 'rules',
                    tabPanel('Attrition = Yes',value = 'table',verbatimTextOutput("rulesTableYes"),plotOutput("graphPlotYes")),
                    tabPanel('Attrition = No',value = 'table',verbatimTextOutput("rulesTableNo"),plotOutput('graphPlotNo'))
          
    
      )
    )
)
)

server <- function(input,output) {
  
    output$rulesTableYes <- renderPrint({
  
      ar_yes <- apriori(emp_attr_arm_transactions, parameter = list(support = as.numeric(input$Support_val), confidence = as.numeric(input$Confidence_val), 
                                                        minlen = as.numeric(input$minlen_val)),
                        appearance = list(default = "lhs", rhs=("Attrition=Yes")),control=list(verbose = FALSE))
      validate(
        need(length(ar_yes) != 0, "No Rules for this value of support & confidence")
      )
           inspect(head(sort (ar_yes, by=input$sort_by, decreasing=TRUE),input$no_rules))
    
  })
  
  output$rulesTableNo <- renderPrint({
    
    ar_no <- apriori(emp_attr_arm_transactions, parameter = list(support = as.numeric(input$Support_val), confidence = as.numeric(input$Confidence_val), 
                                                     minlen = as.numeric(input$minlen_val)),
                     appearance = list(default = "lhs", rhs=("Attrition=No")),control=list(verbose = FALSE))
    validate(
      need(length(ar_no) != 0, "No Rules for this value of support & confidence")
    )
    
    inspect(head(sort (ar_no, by=input$sort_by, decreasing=TRUE),input$no_rules))
    
  })
  
  output$graphPlotYes <- renderPlot({
    ar_yes <- apriori(emp_attr_arm_transactions, parameter = list(support = as.numeric(input$Support_val), confidence = as.numeric(input$Confidence_val), 
                                                      minlen = as.numeric(input$minlen_val)),
                      appearance = list(default = "lhs", rhs=("Attrition=Yes")))
    plot(head(ar_yes,n = input$no_rules,by=input$sort_by), method='paracoord')
  }, height=800, width=800)
  
  output$graphPlotNo <- renderPlot({
    ar_no <- apriori(emp_attr_arm_transactions, parameter = list(support = as.numeric(input$Support_val), confidence = as.numeric(input$Confidence_val), 
                                                      minlen = as.numeric(input$minlen_val)),
                      appearance = list(default = "lhs", rhs=("Attrition=No")))
    
    plot(head(ar_no,n = input$no_rules,by=input$sort_by), method='paracoord')
  }, height=800, width=800)
  
}
arm_df <- read.csv("employee_attrition_arm.csv",stringsAsFactors = TRUE)
arm_df$Education <-as.factor(arm_df$Education)
arm_df$EnvironmentSatisfaction <- as.factor(arm_df$EnvironmentSatisfaction)
arm_df$JobInvolvement <- as.factor(arm_df$JobInvolvement)
arm_df$JobLevel <- as.factor(arm_df$JobLevel)
arm_df$JobSatisfaction <- as.factor(arm_df$JobSatisfaction)
arm_df$PerformanceRating <- as.factor(arm_df$PerformanceRating)
arm_df$RelationshipSatisfaction <- as.factor(arm_df$RelationshipSatisfaction)
arm_df$StockOptionLevel <- as.factor(arm_df$StockOptionLevel)
arm_df$TrainingTimesLastYear <- as.factor(arm_df$TrainingTimesLastYear)
arm_df$WorkLifeBalance <- as.factor(arm_df$WorkLifeBalance)
emp_attr_arm_transactions <- as(arm_df,"transactions")
shinyApp(ui = ui, server = server)
  