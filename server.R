##Shiny Code(server.R)

library(shiny)

mod7 = lm(charges ~ age +smoker + bmi :smoker, data = insurance)
summary(mod7)
#plot(mod7)

# Define server logic for this application
shinyServer(function(input, output) {
  
  # Reactive expression to predict the mpg. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  output$myTrans <- renderText({ input$Trans })
  
  output$charges <- renderText({ 
    input$actionButton
    isolate({
      # wt ,qsec,am
      newdata = data.frame(age=input$age,bmi=input$bmi, smoker=input$smoker)
      myp  <- predict(mod7,newdata)
      #output$myP <- p[1]
    })
  })
  
  # Generate diagnostic plot s
  output$myplot <- renderPlot({
    
    # optional 4 graphs/page
    layout(matrix(c(1,2,3,4),2,2,byrow=T))
    plot(mod7)
    
  })
  
  
  output$myplot1 <- renderPlot({
    library(corrplot)
    # optional 4 graphs/page
    #layout(matrix(c(1,2,3,4),2,2,byrow=T))
    #plot(round(cor(insurancecorr),2))
    corrplot(cor(insurancecorr), method = "circle")
  })
  # we <- reactive({
  #   w <- as.numeric(input$Weight)
  # })
  
  output$myplot2 <- renderPlot({
    
    pairs(charges~.,data=insurance)
  })
  
  
  output$myplot3 <- renderPlot({
    
    cvResults <- suppressWarnings(CVlm(insurance, form.lm=charges ~ age +smoker + bmi :smoker, m=2, dots=TRUE, seed=10, legend.pos="topleft",  printit=TRUE, main="Small symbols are predicted values while bigger ones are actuals."));
    attr(cvResults, 'ms')
  })
  
  output$myplot4 <- renderPlot({
    
    grid.arrange(plot.sexbox, plot.smokerbox,plot.childbox,plot.regionbox , nrow=2)
  })
  
  output$myplot5 <- renderPlot({
    plot.freqcnt<-ggplot(data=insurance, aes(x=charges)) +
      geom_histogram(binwidth=1000, fill="blue") + ggtitle("Frequency plot for Medical Charges") 
    
  grid.arrange(plot.agescatter, plot.bmiscatter,plot.childrenscatter,plot.freqcnt, nrow=2)
})
  output$myplot6 <- renderPlot({
    ggplot(insurance, aes(x=age, y = charges)) + 
      geom_point(aes(color = smoker, size = bmi),alpha=0.5) + #four groups: smoker with high BMI, non-smoker with high BMI, smoker with low BMI and non-smoker with low BMI --> could indicate that there needs to be an interaction term between BMI and Smoker. 
      ggtitle("Charges ~ Age, BMI, Smoker") +geom_smooth(method='lm')
  
      }) 

  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(mod7)
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(insurance)
  })
})
# 
# 
# ############################################################
#   library(shiny)
# 
# # transforming into factor variables
# mtcars$cyl <- as.factor(mtcars$cyl)
# mtcars$vs <- as.factor(mtcars$vs)
# mtcars$am <- as.factor(mtcars$am)
# mtcars$gear <- as.factor(mtcars$gear)
# 
# # mtcars structure after tranformation 
# str(mtcars)
# 
# # fitting linear model
# model <- lm(mpg ~. ,  data=mtcars)
# 
# # Choose a model by AIC in a Stepwise Algorithm
# best_model <- step(model,direction="both")
# 
# # generate model summary
# summary(best_model)
# 
# # Define server logic for this application
# shinyServer(function(input, output) {
#   
#   # Reactive expression to predict the mpg. This is 
#   # called whenever the inputs change. The renderers defined 
#   # below then all use the value computed from this expression
#   output$myTrans <- renderText({ input$Trans })
#   
#   output$myp <- renderText({ 
#     input$actionButton
#     isolate({
#       # wt ,qsec,am
#       newdata = data.frame(wt=we(),qsec=input$Qsec, am=input$Trans)
#       myp  <- predict(best_model,newdata , interval = "predict")
#       #output$myP <- p[1]
#     })
#   })
#   
#   
#   # Generate diagnostic plot s
#   output$myplot <- renderPlot({
#     
#     # optional 4 graphs/page
#     layout(matrix(c(1,2,3,4),2,2,byrow=T))
#     plot(best_model)
#     
#   })
#   we <- reactive({
#     w <- as.numeric(input$Weight)
#   })
#   
#   # Generate a summary of the data
#   output$summary <- renderPrint({
#     summary(mtcars)
#   })
#   
#   # Generate an HTML table view of the data
#   output$table <- renderTable({
#     data.frame(mtcars)
#   })
# })
# 
# 
# 
# 



