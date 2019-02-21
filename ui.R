library(shiny)
shinyUI(pageWithSidebar(
    # Application title
  headerPanel("Predicting insurance costs for Medical Data"),
  
  # Adding widgets
    sidebarPanel(
    
    helpText("Enter Age, BMI and Smoking values, click on ",strong("Predict!")," to get predicted value of insurance costs."),
    numericInput("age", "age", value = 18, min = 18, max = 64),
    sliderInput("bmi","bmi",value=30,min=15.96,max=53.13,animate=T, round = FALSE, step = 0.01),
    br(),
    radioButtons("smoker","Smoking",list("No" = "no","Yes" = "yes"),"no"),
    actionButton("actionButton","Predict!",align = "center"),
    helpText("Note: For ",strong("Summary, Plot")," and" ,strong("Table"),", whole",strong("Insurance"),"dataset is taken into account and results are displayed respectively")
    
  ),
  
  # Show a tabset that includes mpg prediction, plot, summary, and table view of mtcars dataset
    mainPanel(
    tabsetPanel(
      tabPanel("Prediction",
               h2("Insurance Costs Prediction using Multiple Linear Regression",align="center") ,
               p("Selection of a multiple linear regression (MLR) model that describes a dependent variable term 'charges' by independent variables terms which are significant i.e. Age, Smoker and an interaction product of BMI and Smoker and removing variables which are not much significant such as sex, children and region."), 
               
               
               h3("Prediction for Insurance Cost"),
               
               p("We now apply the predict function on the input data to see the predicted insurance costs."),
               
               p("The predicted",span(" Insurance Cost",style = "color:blue")," value is :"),
               code(textOutput("charges"))
      ),
                # p("The predicted",span(" MPG value",style = "color:blue")," and its",span(" Lower Bound",style="color:blue"),"and",
                #   span("Upper Bound",style="color:blue")," values are :"),
               
               
      tabPanel("Summary", 
               h2("Summary of",strong("Insurance"),"dataset"),
               verbatimTextOutput("summary")), 
      tabPanel("Table", 
               h2("View of Insurance dataset"),
               tableOutput("table")),
      tabPanel("Diagnostic Plots" ,
               h2("Diagnostic plots for Insurance dataset(Pattern, Distribution, Spread & Influence)"),
               plotOutput("myplot")),
      tabPanel("Correlation Plots" ,
               h2("Correlation Plots for Insurance Dataset"),
               plotOutput("myplot1")),
      tabPanel("ScatterPlot Matrix" ,
               h2("Scatterplots for Insurance Dataset"),
               plotOutput("myplot2")),
      tabPanel("k-Fold CV" ,
               h2("K-Fold Cross Validation Insurance Dataset"),
               plotOutput("myplot3")),
      tabPanel("Boxplot" ,
               h2("Boxplot on Insurance Dataset"),
               plotOutput("myplot4")),
      tabPanel("Scatter/Frequency plot" ,
               h2("Scatter and Frequency plots on Insurance Dataset"),
               plotOutput("myplot5")),
      tabPanel("Actual vs Predicted plot" ,
               h2("Actual vs Predicted Medical Charges plot on Insurance Dataset"),
               plotOutput("myplot6"))
    )
  )
))
