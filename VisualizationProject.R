##Medical Costs Personal Datasets

##Link of the dataset for download
##https://www.kaggle.com/mirichoi0218/insurance

##Installing Packages and loading all the libraries

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("randomForest")
install.packages("gridExtra")
install.packages("adegraphics")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("psych")

library(shiny)
library(corrplot)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(randomForest)
library(gridExtra)
library(adegraphics)
library(RColorBrewer)
library(psych)

##Read and View the Dataset
insurance <- read.csv("C:/Users/Abhishek/Desktop/STAT 515/Final Project/insurance.csv")
View(insurance)

##Structure and summary of the dataset
head(insurance)
str(insurance)
summary(insurance)
class(insurance)

##Single column summary
summary(insurance$sex)
summary(insurance$smoker)
class(insurance$sex)
class(insurance$smoker)

##Encoding column values in data-- Converting Factor to numeric data
table(as.numeric(insurance$sex))
table(as.numeric(insurance$smoker))
table(as.numeric(insurance$region))
table(as.numeric(insurance$children))

##Ggplot for Charges vs Age
plot.agescatter <- ggplot(insurance, aes(x = age, y = charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +theme()
plot.agescatter

##Ggplot for Charges vs BMI 
plot.bmiscatter <- ggplot(insurance, aes(x = bmi, y = charges)) +geom_point()
plot.bmiscatter

##Ggplot for Charges vs no of children
plot.childrenscatter <- ggplot(insurance, aes(children, charges)) +
  geom_jitter(aes(color = children), alpha = 0.5) +theme_light()
plot.childrenscatter

##Arrangement of Multiple plots-- (nrow=1) means only 1 row
plot.agebmiscatter <- grid.arrange(plot.agescatter, plot.bmiscatter)
q <- grid.arrange(plot.agescatter, plot.bmiscatter,plot.childrenscatter, nrow=2)

##Boxplot for Sex vs Charges
plot.sexbox <- ggplot(insurance, aes(x = sex, y = charges)) +geom_boxplot(fill = c(2:3)) + ggtitle("Boxplot of Medical Charges per Sex")+theme_classic()
plot.sexbox

##Boxplot for Smoker vs Charges
plot.smokerbox <- ggplot(insurance, aes(x = smoker, y = charges)) +geom_boxplot(fill = c(3:4))+ ggtitle("Boxplot of Medical Charges vs Smoking(y/n)")+theme_classic()
plot.smokerbox

##Boxplot for children vs charges
insurance$children<-as.factor(insurance$children)
plot.childbox <- ggplot(insurance, aes(x = children, y = charges)) +geom_boxplot(fill = c(10:15))+ ggtitle("Boxplot of Medical Charges vs Number of children")
plot.childbox

##Boxplot for region vs charges
plot.regionbox <- ggplot(insurance, aes(x = region, y = charges)) +geom_boxplot(fill = c(4:7))+ ggtitle("Boxplot of Medical Charges per Region")+theme_classic()
plot.regionbox

r <- grid.arrange(plot.sexbox, plot.smokerbox,plot.childbox,plot.regionbox , nrow=2)

##Ggplot--Scatterplot for Age vs Charges + BMI + Smoker
##Also scatterline showing relationship between Age and charges  
ggplot(insurance, aes(x=age, y = charges)) + 
  geom_point(aes(color = smoker, size = bmi),alpha=0.5) + #four groups: smoker with high BMI, non-smoker with high BMI, smoker with low BMI and non-smoker with low BMI --> could indicate that there needs to be an interaction term between BMI and Smoker. 
  ggtitle("Charges ~ Age, BMI, Smoker") +geom_smooth(method='lm')

##Frequency Plot-- Histogram Plot for charges vs count of charges for binwidth=1000
ggplot(data=insurance, aes(x=charges)) +
  geom_histogram(binwidth=1000, fill="blue") + ggtitle("Frequency plot for Medical Charges") 

##Correlation Matrix
cor(insurance, use="pairwise.complete.obs")
insurancecorr <- insurance[,1:length(insurance)]
insurancecorr<-data.matrix(insurancecorr)
round(cor(insurancecorr),2)
corrplot(cor(insurancecorr), method = "circle")
corrplot(cor(insurancecorr), type="upper", order="hclust", 
         col=brewer.pal(n=10, name="RdBu"))

#Scatterplot Matrix
pairs(charges~.,data=insurance, 
      main="Simple Scatterplot Matrix")
pairs.panels(insurance)

##Splitting data into Train and Test Dataset
data <- round(0.7 * nrow(insurance))
traindata <- sample(1:nrow(insurance), data)
insurance_train <- insurance[traindata, ]
insurance_test <- insurance[-traindata, ]

##Linear Regression Model with all variables
mod1 = lm(charges ~ ., data = insurance_train)
summary(mod1)
##R2--0.7402

mod2 = lm(charges ~ age + bmi +children +smoker, data = insurance_train)
summary(mod2)
##R2--0.7397

mod3 = lm(charges ~ age + bmi +smoker, data = insurance_train)
summary(mod3)
##R2--0.7365

mod4 = lm(charges ~ bmi +smoker, data = insurance_train)
summary(mod4)
##R2--0.6474

mod5 = lm(charges ~ age +smoker, data = insurance_train)
summary(mod5)
##R2--0.7136

mod6 = lm(charges ~ age *bmi *smoker, data = insurance_train)
summary(mod6)
##R2--0.8258

mod7 = lm(charges ~ age +smoker + bmi :smoker, data = insurance_train)
summary(mod7)
plot(mod7)

##R2--0.8262--Final Model with only 3 terms and which is better in terms of R sqr value

mod8 <- lm(charges ~ smoker + age + children + region + smoker * bmi, data = insurance_train)
summary(mod8)
##R2--0.8308

mod9 = lm(charges ~ age +smoker+ bmi :smoker +region:bmi, data = insurance_train)
summary(mod9)
##R2==0.837

#Saving R-squared
rsq_train <- summary(mod7)$r.squared
rsq_train

#correlation for mod7 is squareroot of R^2
sqrt(0.8262)
##Correlation value of 0.91

#predict data on test set and review summary stats and plots
insurance_test$prediction <- predict(mod7, newdata = insurance_test)
ggplot(insurance_test, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")
summary(mod7)
AIC(mod7)
##R2--0.8321
##AIC--18596.42

#Prediction Accuracy for the Test Dataset as compared to actual dataset values
cor(x = insurance_test$prediction,y = insurance_test$charges)
# actuals_preds <- data.frame(cbind(actuals=insurance_test$charges, predicteds=predict_ontestdata))
# correlation_accuracy <- cor(actuals_preds) 
# correlation_accuracy
##91.88% correctly predicted

##K-Fold Cross Validation
library(DAAG)
cvResults <- suppressWarnings(CVlm(insurance, form.lm=charges ~ age +smoker + bmi :smoker, m=2, dots=TRUE, seed=10, legend.pos="topleft",  printit=TRUE, main="Small symbols are predicted values while bigger ones are actuals."));
attr(cvResults, 'ms') 
##cv-24367863

##Residual Sum of Squares(Rss)
RSS <- c(crossprod(mod7$residuals))
MSE <- RSS / length(mod7$residuals)
#RMSE <-sqrt(MSE)
RMSE <-sqrt(mean(mod7$residuals^2))
##SSE and SST
SSE<- sum((predict_ontestdata-insurance_test$charges)^2)
SSE
SST<-sum((insurance_test$charges-mean(insurance_test$charges))^2)
SST

#Min Max accuracy and MAPE
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
#77.33%--min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
#31.23%--mean absolute percentage error

#calculating the residuals for test data
residuals_ontestdata <- insurance_test$charges - insurance_test$prediction
plot(residuals_ontestdata)

Data_test$residuals <- Data_test$charges - Data_test$prediction

#calculating Root Mean Squared Error
rmse_ontestdata <- sqrt(mean(residuals_0^2))
##rmse-4713

r_sq_1 <- summary(model_1)$r.squared

prediction_1 <- predict(model_1, newdata = Data_test)

residuals_1 <- Data_test$charges - prediction_1
rmse_1 <- sqrt(mean(residuals_1^2))

# ggplot(data = Data_test, aes(x = prediction, y = residuals)) +
#   geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +
#   geom_hline(yintercept = 0, linetype = 3, color = "red") +
#   ggtitle("Residuals vs. Linear model prediction")
# 
# ggplot(insurance_test, aes(x = residuals)) + 
#   geom_histogram(bins = 15, fill = "blue") +
#   ggtitle("Histogram of residuals")

##ANOVA(Analysis of Variance) Table
anova(mod7)

# ####################################
# ##Random Forest
# Data_train<-data.matrix(Data_train)
# 
# insurance.rftrain=randomForest(charges ~ . , data = Data_train, importance=TRUE)
# insurance.rftrain
# plot(insurance.rftrain)
# 
# insurance.rftest=randomForest(charges ~ . , data = Data_test, ntree= 500, mtry=6, importance=TRUE)
# insurance.rftest
# 
# importance(insurance.rftest)
# varImpPlot(insurance.rftest)
# 
# set.seed(222)
# rdf<-randomForest(insurance$charges~.,data=insurance,na.action = na.roughfix)
# imp_RF <- importance(rdf)
# imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
# imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]
# ggplot(imp_DF[1:6,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")

# #############################################
# ##Ridge and Lasso Regression
# 
# x <- model.matrix(insurance$charges~., insurance)[,-1]
# y <- insurance$charges
# lambda <- 10^seq(10, -2, length = 100)
# 
# set.seed(489)
# #Data_train
# #Data_test
# 
# ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
# summary(ridge.mod)
# coef.glmnet(ridge.mod)

#####################################################

##Shiny Code(server.R)


# # Define server logic for this application
# shinyServer(function(input, output) {
#   
#   # Reactive expression to predict the mpg. This is 
#   # called whenever the inputs change. The renderers defined 
#   # below then all use the value computed from this expression
#   output$myTrans <- renderText({ input$Trans })
#   
#   output$charges <- renderText({ 
#     input$actionButton
#     isolate({
#       # wt ,qsec,am
#       newdata = data.frame(wt=we(),qsec=input$Qsec, am=input$Trans)
#       myp  <- predict(best_model,newdata , interval = "predict")
#       #output$myP <- p[1]
#     })
#   })
#   
#   # Generate diagnostic plot s
#   output$myplot <- renderPlot({
#     
#     # optional 4 graphs/page
#     layout(matrix(c(1,2,3,4),2,2,byrow=T))
#     plot(mod7)
#     
#   })
#   we <- reactive({
#     w <- as.numeric(input$Weight)
#   })
#   
#   # Generate a summary of the data
#   output$summary <- renderPrint({
#     summary(insurance)
#   })
#   
#   # Generate an HTML table view of the data
#   output$table <- renderTable({
#     data.frame(insurance)
#   })
# })
