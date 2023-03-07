#Assignment 3: Earning manipulation by Indian Firms
# Authors: #Nabeel Khan: UIN- ; Nha Nguyen: UIN- ; Razzaq: UIN- 


library(readxl)
library(caret)
library(pROC)
library(ROSE)
library(robustbase)
library(smotefamily)
library(ROCR)
library(rpart)
library(rpart.plot)

#load the excel file 
library(readxl)
databook=read_excel('/Users/nabeel/Downloads/Earnings_main.xlsx')
#Read the 2nd Excel sheet from databook "Manipulator"
manipulator<-read_excel('/Users/nabeel/Downloads/Earnings_main.xlsx',sheet="Manipulator")
#Read the 3rd Excel sheet from databook "Non-manipulator"
nonmanipulator<-read_excel('/Users/nabeel/Downloads/Earnings_main.xlsx',sheet="Non-Manipulator")
#Read the 4th Excel sheet from databook "Complete Data"
complete.data<- read_excel('/Users/nabeel/Downloads/Earnings_main.xlsx',sheet=4)
#Read the 5th Excel sheet from databook "Sample for Model Development"
sample.data<-read_excel('/Users/nabeel/Downloads/Earnings_main.xlsx',sheet=5)


#Replace '-' in the column name 'C-MANIPULATOR' to "C_MANIPULATOR"
names(sample.data)[names(sample.data) == "C-MANIPULATOR"] <- "C_MANIPULATOR"
names(complete.data)[names(complete.data) == "C-MANIPULATOR"] <- "C_MANIPULATOR"
names(complete.data)[names(complete.data) == 'Company ID'] <- 'CID'
names(sample.data)[names(sample.data) == 'Company ID'] <- 'CID'
str(sample.data)

#Check for NA values
colSums(is.na(sample.data)) 

#Change the Manipulator column values to numeric along with its type
sample.data$Manipulator <- ifelse(sample.data$Manipulator == "Yes", 1, 0)
df <- data.frame(sample.data)

#df$C_MANIPULATOR <- as.factor(df$C_MANIPULATOR)
str(df)

table(df$C_MANIPULATOR)
prop.table(table(df$C_MANIPULATOR))

####### Q3- Developing Logistic Regression Model #######

#Divide the data into Training and Testing
set.seed(1234)
SIndx <- sample(2, nrow(sample.data), replace = TRUE, prob = c(0.65,0.35))
TrainIn <- df[SIndx==1, ]
TestIn <- df[SIndx==2, ]
str(TrainIn)
table(TrainIn$C_MANIPULATOR)
prop.table(table(TrainIn$C_MANIPULATOR))

# Variable selection for Logistic Model 

full <- lm(C_MANIPULATOR ~., data = TrainIn, family = "binomial")
null <- lm(C_MANIPULATOR ~1, data = TrainIn, family = "binomial")

step(null, scope = list(lower = null, upper = full), direction = "forward")

#After running either or both (forward & backward) variable selection method, we can see 
#from the output that the important variables are ‘DSRI + SGI + ACCR + AQI + GMI’. 
#Hence, we will run our model using only these important input variables

sampleeemodel <- glm(C_MANIPULATOR~DSRI + SGI + ACCR + AQI + GMI , data= sample.data, family = "binomial")
summary(sampleeemodel)

#Using Treebag Model by Cross Validation, a simple bagging model is created.
#The Treebag Model will provide the Accuracy for training data
optn <- trainControl(method = "cv", number = 5)
tbModel <- train(C_MANIPULATOR ~ ., data = TrainIn, method = "treebag", trControl = optn)
PREDICTTT <- names(TrainIn)[names(TrainIn) != 'C_MANIPULATOR']
pred <- predict(tbModel$finalModel, TestIn[, PREDICTTT])
auc <- roc(TestIn$C_MANIPULATOR, pred)
print(auc)

#Using Treebag Model, the following determines the accuracy on Test Data
str(TestIn)
table(TestIn$C_MANIPULATOR)
prop.table(table(TestIn$C_MANIPULATOR))

optnTest <- trainControl(method = "cv", number = 5)
tbTestModel <- train(C_MANIPULATOR ~ ., data = TestIn, method = "treebag", trControl = optn)
PREDICTTTTest <- names(TestIn)[names(TestIn) != 'C_MANIPULATOR']
predTest <- predict(tbTestModel$finalModel, TestIn[, PREDICTTTTest])
aucTest <- roc(TestIn$C_MANIPULATOR, predTest)
print(aucTest)

#Using SMOTE - will design a new sample.data for the modeling
smote_train <- TrainIn
smote_train$C_MANIPULATOR <- as.factor(smote_train$C_MANIPULATOR)
table(smote_train$C_MANIPULATOR)
prop.table(table(smote_train$C_MANIPULATOR))
str(smote_train)

smote_train <- SMOTE(smote_train[-11], smote_train$C_MANIPULATOR)
smote_new <- smote_train$data
table(smote_new$class)
prop.table(table(smote_new$class))

######## Q4-Performance on Test&Train #########

# Logistic Regression for the data Balanced by Smote
# We use Accuracy measure to evaluate performance. 
# Along AIC. 
smote_main <- glm(as.factor(class) ~ ., data = smote_new, family = "binomial")
summary(smote_main)

smote_predict <- predict(smote_main, type = "response")
smote_predict <- ifelse(smote_predict > 0.50, 1, 0)
confusionMatrix(as.factor(smote_new$class), as.factor(smote_predict), positive = "1")
## The model gives 100% accuracy of Training Data

#Logistic Regression on Test Data
#Using SMOTE - will design a new Test sample.data for the modeling
smote_test <- TestIn
smote_test$C_MANIPULATOR <- as.factor(smote_test$C_MANIPULATOR)
table(smote_test$C_MANIPULATOR)
prop.table(table(smote_test$C_MANIPULATOR))
str(smote_test)

smote_test <- SMOTE(smote_test[-11], smote_test$C_MANIPULATOR)
smote_testNew <- smote_test$data
table(smote_testNew$class)
prop.table(table(smote_testNew$class))

#Logistic Regression for the data Balanced by Smote
smote_testLogistic <- glm(as.factor(class) ~ ., data = smote_testNew, family = "binomial")
summary(smote_testLogistic)

smote_testPredict <- predict(smote_testLogistic, type = "response")
smote_testPredict <- ifelse(smote_testPredict > 0.50, 1, 0)
confusionMatrix(as.factor(smote_testNew$class), as.factor(smote_testPredict), positive = "1")
## The model gives 100% accuracy for Test Data



######### Q5 ############





pr.err <- c()
for(mt in seq(1,ncol(TrainIn)))
{
  rf1 <- randomForest(C_MANIPULATOR~., data = sample.data, ntree = 100,mtry = ifelse(mt == ncol(TrainIn),mt-1, mt))
  
  predicted <- predict(rf1, newdata = TestIn, type = "class")
  pr.err <- c(pr.err,mean(TestIn$C_MANIPULATOR != predicted))
}
bestmtry <- which.min(pr.err)
bestmtry

#rf1 <- randomForest(CMAN ~ ., data =sample_train ,mtry = sqrt(ncol(sample_train)-1), ntree = 300,
#proximity = T, importance = T)

# Identifying the optimal cut-off point from ROC curve using the distance to (0,1) approach



library(ROCR)
pred007 <-prediction(rf1$votes[,2], TrainIn$C_MANIPULATOR)
perf <- performance(pred007, "tpr", "fpr")
plot(perf)

pred007 = prediction(TrainIn, TrainIn$C_MANIPULATOR)
perff = performance(pred007, "tpr", "fpr")
plot(perff, col = "blue")

mydistance <- function(x,y,p){
  d=(x-0)^2+(y-1)^2 # given the points (x, y), compute the distance to the corner point (0,1)
  ind <- which(d==min(d)) # Find the minimum distance and its index
  c(recall = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]]) # return the corresponding tpr, fpr and the cutoff point
}

opt.cut <- function(perf){
  cut.ind <- mapply(FUN = mydistance, 
                    perf@x.values, perf@y.values,perf@alpha.values)
}
Output <- opt.cut(perf)
print(Output[,1])

Threshold <- Output[,1]["cutoff"]

predictedClasss <- as.factor(ifelse(rf1$votes[,2] >= Threshold, "Low", "High"))
CM1 <- table(predictedClasss, TrainIn$C_MANIPULATOR, dnn = c("Predicted","Actual"))
######### Q7- Decision Trees ##########

library(rpart)
library(rpart.plot)


TrainModel111<-rpart( C_MANIPULATOR ~ + DSRI + AQI + GMI + SGI + SGAI + ACCR + DEPI + LEVI + DEPI  , data=sample.data, parms = list(split = "information"), control = rpart.control(minsplit = 5, cp=0.01)) 
rpart.plot(TrainModel111) 
print(TrainModel111)


#Accuracy for the DecisionTree

Train_data_pred1<-predict(TrainModel111, TrainIn)
Train_data_accuracy1<-round(mean(TrainIn$C_MANIPULATOR==Train_data_pred1)*100,2)
Train_data_accuracy1
#Accuracy = 77.78%

Test_data_pred2<-predict(TrainModel111, TestIn)
Test_data_accuracy2<-round(mean(TestIn$C_MANIPULATOR==Test_data_pred2)*100,2)
Test_data_accuracy2
#Accuracy = 74.63%

###### Random Forest Model #######

library(randomForest)
library(ROSE)

# Sampling Test and Train data 
sample.data$`CID`<- NULL
set.seed(1234)
index <- sample(2, nrow(sample.data), replace = TRUE, prob = c(0.65,0.35))
rf_train <- sample.data[index == 1,]
rf_test <- sample.data[index == 2,]

# Balancing the data by Oversampling 
over_sample_rf <- ovun.sample(C_MANIPULATOR~., data = rf_train, method = "over", N= 250)$data
table(over_sample_rf$C_MANIPULATOR)

randomforest1 = randomForest(C_MANIPULATOR~., 
                  data = over_sample_rf, ntree = 100, 
                  proximity = TRUE, replace= TRUE, 
                  importance = TRUE, 
                  mtry = sqrt(ncol(over_sample_rf)))
randomforest2 = randomForest(C_MANIPULATOR~ DSRI + SGI + ACCR, 
                           data = over_sample_rf, ntree = 100, 
                           proximity = TRUE, replace= TRUE, 
                           importance = TRUE, 
                           mtry = sqrt(ncol(over_sample_rf)))

print(randomforest1)
plot(randomforest1)
plot(randomforest2)

rf_test_pred <- predict(randomforest1, newdata = rf_test)
rf_test_pred
rf_table <- table(rf_test_pred, newdata = rf_test$C_MANIPULATOR)


##### Q8-Linear Regression model for Complete data ######

table(complete.data$C_MANIPULATOR)
colSums(is.na(complete.data))

# Change the Manipulator column values to numeric along with its type
complete.data$MANIPULATOR <- ifelse(complete.data$Manipulater == "Yes", 1, 0)
str(complete.data)

# Splitting the complete data into Training and Testing 
set.seed(123)
index1 = sample(2, nrow(complete.data), replace = TRUE, prob = c(0.8,0.2))
Train_complete = complete.data[index1 == 1, ]
nrow(Train_complete)
table(Train_complete$C_MANIPULATOR)
Test_complete = complete.data[index1 == 2,]
nrow(Test_complete)
table(Test_complete$C_MANIPULATOR)

# Building Logistic Regression model 
L_Train_Complete <- glm(C_MANIPULATOR ~ ., 
                       data = Train_complete, 
                       family = "binomial")
summary(L_Train_Complete)

#Using oversampling for predicting 
L_Train_over <- ovun.sample(C_MANIPULATOR~ .,
                           data = Train_complete,
                           method = "over", 
                           N=1986)$data
table(L_Train_over$C_MANIPULATOR)

# Variable selection for Logistic Model 

full1 <- lm(C_MANIPULATOR ~., data = L_Train_over, family = "binomial")
null1 <- lm(C_MANIPULATOR ~1, data = L_Train_over, family = "binomial")

step(null, scope = list(lower = null1, upper = full1), direction = "forward")

#After running either or both (forward & backward) variable selection method, we can see 
#from the output that the important variables are ‘DSRI + SGI + ACCR + AQI + GMI’. 
#Hence, we will run our model using only these important input variables

L_Train_Complete_variable <- glm(C_MANIPULATOR ~  DSRI + SGI + AQI + ACCR + LEVI + GMI,
                                data = L_Train_over, 
                                family = "binomial")
summary(L_Train_Complete_variable)

# Deviance for the model
Lt <- summary(L_Train_Complete_variable)$deviance
Lt

# Predict test data based on model
T_pred_variable = predict.glm(L_Train_Complete_variable, newdata = Test_complete, type="response")
T_pred_variable

#Plotting ROC Curve
roc_pred = prediction(T_pred_variable, Test_complete$C_MANIPULATOR)
roc_perf = performance(roc_pred, "tpr", "fpr")
plot(roc_perf, col = "blue")

#calculating Optimal Cutoff
opt.cut = function(roc_perf, roc_pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], cutoff = p[[ind]])
  }, roc_perf@x.values, roc_perf@y.values, roc_pred@cutoffs)}
print(opt.cut(roc_perf, roc_pred))
#sensitivity 0.9000000
#specificity 0.8534483
#cutoff      0.4188783

#Using the cutoff Point to Plot Confusion Matrix
T_pred_variable$C_MANIPULATOR = ifelse(T_pred_variable> 0.4188783,1,0)

pt<-table(T_pred_variable$C_MANIPULATOR, Test_complete$C_MANIPULATOR, dnn = c("Predicted","Actual"))
pt
confusionMatrix(pt,positive = "1")
# We Get 90% Accuracy

