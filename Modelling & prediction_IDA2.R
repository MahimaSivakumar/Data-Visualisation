install.packages("glmnet")

library(cluster)
library(greybox)
library(dbplyr)
library(corrplot)
library(ggplot2)
library(plotly)
library(reshape2)
library(lattice)
library(arulesViz)
library("viridis")
library("tidyr")
library(scales)
library(caret)
library(themis)
library(randomForest)
library(pROC)
library(glmnet)



airline = read.csv("airlinesData96.csv" , stringsAsFactors = TRUE)
str(airline)
airline[, c("Inflightwifiservice", "DepartureArrivaltimeconvenient", "EaseofOnlinebooking", "Gatelocation", "Foodanddrink", "Onlineboarding", "Seatcomfort", 
         "Inflightentertainment", "Onboardservice", "Legroomservice", "Baggagehandling", "Checkinservice", 
         "Inflightservice", "Cleanliness")] <- lapply(airline[, c("Inflightwifiservice", "DepartureArrivaltimeconvenient", "EaseofOnlinebooking", "Gatelocation", "Foodanddrink", "Onlineboarding", "Seatcomfort", 
                                                               "Inflightentertainment", "Onboardservice", "Legroomservice", "Baggagehandling", "Checkinservice", 
                                                               "Inflightservice", "Cleanliness")], function(x) ifelse(x == 0, NA, x))
airline_data = na.omit(airline)
str(airline_data)
data_airline = airline_data
set.seed(582)

# create a character vector of variable names to convert
vars_to_convert <- c("Gender", "TypeofTravel", "Class", "CustomerType")
# loop through each variable and convert to numeric
for (var in vars_to_convert) {
  airline_data[var] <- as.numeric(factor(airline_data[[var]]))
}

# LASSO Regression 
train_index <- sample(1:nrow(airline_data), size = 0.8*nrow(airline_data), replace = FALSE)
train_data <- airline_data[train_index,-22]
test_data <- airline_data[-train_index,-22]


# Fit a lasso regression model to the training data
x_train <- model.matrix(satisfaction ~ ., data = train_data)[,-1]
y_train <- train_data$satisfaction
lasso_model <- glmnet(x_train, y_train, family = "binomial", alpha = 1)

# Plot the coefficient path to select the optimal lambda
plot(lasso_model, xvar = "lambda", label = TRUE)

cv_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
opt_lambda <- cv_model$lambda.min

# Select the features with non-zero coefficients at the optimal lambda
lasso_coef <- data.frame(as.vector(coef(lasso_model, s = opt_lambda)))
lasso_coef[,2] <- rownames(coef(lasso_model, s = opt_lambda))
lasso_coef <- lasso_coef[-1,]
rownames(lasso_coef) <- 1:(nrow(lasso_coef))
colnames(lasso_coef) <- c('Coefficients','Variable')
lasso_coef <- lasso_coef[c('Variable','Coefficients')]
selected_features <- lasso_coef[lasso_coef[,2]!=0,]
rownames(selected_features) <- 1:(nrow(selected_features))
ranked_features <- selected_features[order(abs(selected_features[,2]), decreasing = TRUE),]



# Evaluate the performance of the model on the test data
x_test <- model.matrix(satisfaction ~ ., data = test_data)[,-1]
y_test <- test_data$satisfaction
ncol(x_test)
predictions <- predict(lasso_model, newx = x_test, s = opt_lambda, type = "response")
threshold <- 0.5
predicted_classes <- as.numeric(predictions > threshold)+1
predicted_classes <-as.factor(predicted_classes)
levels(predicted_classes) = levels(y_test)
test_accuracy <- sum(as.factor(predicted_classes) == y_test)/length(y_test)
test_accuracy 






#Logistic Regression 

logitModel <- glm(satisfaction ~ .,
                  data = airline_data, family = binomial)

summary(logitModel)

probs <- predict(logitModel, newdata = airline_data, type="response")

classPrediction <- 1*(probs > 0.5)
# Verify predictions
prediction <- data.frame(cbind(probs,classPrediction,data_airline$satisfaction))
colnames(prediction)[3] <- 'Actual'

#accuracy
sum(prediction$classPrediction+1==prediction$Actual)*100/nrow(data_airline)

#95% of dissatisfied customers are identified correctly? 
#Sensitivity
sum(prediction$Actual==1 & prediction$classPrediction==0)*100/
  sum(prediction$Actual==1)
#no more than 10% of satisfied customers are mistakenly predicted to be dissatisfied
sum(prediction$Actual==2 & prediction$classPrediction==0)*100/
  sum(prediction$Actual==2)

truthTable <- table(actuals=data_airline$satisfaction, prediction=classPrediction)
truthTable

# Total number of observations in truthTable
N <- sum(truthTable)
# Misclassification error
(truthTable[1,2]+ truthTable[2,1])/N

# Accuracy = Proportion of correct predictions
(truthTable[1,1]+ truthTable[2,2])/N

confusion_matrix <- table(Actual = prediction$Actual, Predicted = prediction$classPrediction)
confusion_matrix
# Calculate specificity (True Negative Rate)
specificity <- confusion_matrix[2, 2] / (confusion_matrix[2, 1] + confusion_matrix[2, 2])
cat("Specificity (True Negative Rate): ", specificity, "\n")

#KNN Modelling

# create a character vector of variable names to convert
vars_to_convert <- c("Gender", "TypeofTravel", "Class", "CustomerType")
# loop through each variable and convert to numeric
for (var in vars_to_convert) {
  airline_data[var] <- as.numeric(factor(airline_data[[var]]))
}
#summary(airline_data)

airline_scale <- cbind(satisfaction = airline_data$satisfaction, as.data.frame(model.matrix(~.-satisfaction,airline_data)[,-1]))

str(airline_scale)
# create a vector of column indices to scale
cols_to_scale <- c(2, 23)

# loop through each column index and scale the column
for (col_index in cols_to_scale) {
  airline_scale[, col_index] <- (airline_scale[, col_index] - min(airline_scale[, col_index])) /
    (max(airline_scale[, col_index]) - min(airline_scale[, col_index]))
}
summary(airline_scale)

obsAll <- nrow(airline_scale)
# Here we split the data to 80%/20%
trainSet <- sample(1:obsAll, 0.80*obsAll)
# Test set. We select values that are not in the train set 
testSet <- (1:obsAll)[!(1:obsAll %in% trainSet)]
table(airline_scale$satisfaction)

#Applying KNN of order 5 
knnModel5 <- knn3(satisfaction~., airline_scale, k=5, subset=trainSet)

knnModel5PredictProb <- predict(knnModel5,
                                newdata=airline_scale[testSet,],
                                type="prob")

knnModel5Predict <- predict(knnModel5,
                                newdata=airline_scale[testSet,],
                                type="class")
confusionMatrix(knnModel5Predict,
                airline_scale$satisfaction[testSet])

#Selecting K for imbalanced data

# Control for cross validation
knnTrainControl <- trainControl(method="repeatedcv", number=5,
                                repeats=3, classProbs=TRUE,
                                summaryFunction=twoClassSummary)
# The training of k-NN
knnTrain <- train(satisfaction~., data=airline_data, method="knn",
                  preProcess="scale", subset=trainSet,
                  trControl = knnTrainControl,
                  metric="ROC", tuneLength=10)
knnTrain
plot(knnTrain)

knnModelPredict <- predict(knnTrain, newdata=airline_data[testSet,],
                           type="raw")

confusionMatrix(knnModelPredict,
                airline_scale$satisfaction[testSet])

#train data confusion matrix
knnModelPredict <- predict(knnTrain, newdata=airline_data[trainSet,],
                           type="raw")

confusionMatrix(knnModelPredict,
                airline_scale$satisfaction[trainSet])

#Fixing data imbalance 

airline_scale_SMOTE <- smote(airline_scale[trainSet,], "satisfaction")

table(airline_scale_SMOTE$satisfaction)
knnTrainBalanced <- train(satisfaction~., data=airline_scale_SMOTE, method="knn",
                          preProcess="scale",
                          trControl = knnTrainControl,
                          metric="ROC", tuneLength=10)
knnTrainBalanced
plot(knnTrainBalanced , main="Selection of K using Cross-Validation")
knnModelBalancedPredict <- predict(knnTrainBalanced, newdata=airline_scale[testSet,],
                                   type="raw")
confusionMatrix(knnModelBalancedPredict,
                airline_scale$satisfaction[testSet])

# JUNK Code

# Define sampling approach
knnTrainControl$sampling <- "smote"
# Retrain k-NN
knnTrainSMOTE <- train(satisfaction~., data=airline_data, method="knn",
                       preProcess="scale", subset=trainSet,
                       trControl = knnTrainControl,
                       metric="ROC", tuneLength=10)
knnTrainSMOTE
plot(knnTrainSMOTE)

#Performance evaluation of logit and KNN
logitModel <- glm(satisfaction~., airline_scale_SMOTE, subset=trainSet, family=binomial)
logitModelPredict <- predict(logitModel,
                             newdata=airline_scale_SMOTE[testSet,],
                             type="response")
# Threshold
threshold <- 0.5
# Classification 
(logitModelPredict>threshold) |>
logitModelPredictClass <- factor(logitModelPredictClass, levels=c("neutral or dissatisfied", "satisfied"),
                                 labels=c("neutralordissatisfied","satisfied"))
confusionMatrix(logitModelPredictClass,
                airline_scale$satisfaction[testSet])
#Decision Tree
DTTrain <- train(satisfaction~., data=airline_scale_SMOTE,
                 method="rpart",
                 trControl=trainControl,
                 metric="ROC", tuneLength=10)

DTTrain
DTPredict <- predict(DTTrain, newdata=airline_scale[testSet,],
                     type="raw")
confusionMatrix(DTPredict,
                airline_scale$satisfaction[testSet])

install.packages("rpart")
library(rpart)




# Fit a decision tree model
treeModel <- rpart(satisfaction ~ ., data = airline_data, method = "class")

# Print the model summary
print(treeModel)

# Plot the decision tree
plot(treeModel)
text(treeModel, use.n = TRUE)

# Predict using the decision tree model
predictions <- predict(treeModel, newdata = airline_data, type = "class")

# Evaluate model performance
confusionMatrix(predictions, airline_data$satisfaction)

#bagging 

DTBagTrain <- train(satisfaction~., data=airline_scale_SMOTE,
                    method="treebag",
                    trControl=trainControl,
                    metric="ROC")
DTBagTrain
DTBagPredict <- predict(DTBagTrain, newdata=airline_scale[testSet,],
                        type="raw")
confusionMatrix(DTBagPredict,
                airline_scale$satisfaction[testSet])

#Random forest 
trainControl <- trainControl(method="repeatedcv", number=5,
                             repeats=3, classProbs=TRUE,
                             summaryFunction=twoClassSummary)

RFTrain <- train(satisfaction~., data=airline_scale_SMOTE,
                 method="rf",
                 trControl=trainControl,
                 metric="ROC")


RFTrain
RFPredict <- predict(RFTrain, newdata=airline_scale[testSet,],
                     type="raw")
confusionMatrix(RFPredict,
                airline_scale$satisfaction[testSet])
varImp(RFTrain) |>
  plot()



# ROC CURVE  - 5 Question 

# k-NN prediction
knnModelProb <- predict(knnTrain, newdata=airline_scale[testSet,],
                        type="prob")
# Logit model
logitModelProb <- predict(logitModel,
                          newdata=airline_scale[testSet,],
                          type="response")
# DT
DTPredict <- predict(DTTrain, newdata=airline_scale[testSet,],
                     type="prob")
# RF
RFPredict <- predict(RFTrain, newdata=airline_scale[testSet,],
                     type="prob")

rocCurves <- vector("list", 4)
# We only need the second column for the purposes of the analysis 
rocCurves[[1]] <- roc(airline_data$satisfaction[testSet] ~ knnModelProb[,2]) 
rocCurves[[2]] <- roc(airline_data$satisfaction[testSet] ~ logitModelProb) 
rocCurves[[3]] <- roc(airline_data$satisfaction[testSet] ~ DTPredict[,2]) 
rocCurves[[4]] <- roc(airline_data$satisfaction[testSet] ~ RFPredict[,2]) 
names(rocCurves) <- c("KNN", "Logit", "DT", "RF")

  par(mfrow=c(2,2)) 
  for(i in 1:4){
    # Plot each of the ROC curves
    plot(rocCurves[[i]], print.auc=TRUE, auc.polygon=TRUE,
         mar=c(4,4,0,0), grid=TRUE)
    # Add titles to plots
    text(1.1, 0.9, names(rocCurves)[i])
  }

  
#ROC curve - 3rd question 
  par(mfrow=c(1,1))   
roc_rf = roc(airline_data$satisfaction[testSet] ~ RFPredict[,2]) 
# Plot the ROC curve
plot(roc_rf, print.thres = c(0.3, 0.5, 0.75), print.auc = TRUE, legacy.axes = TRUE, main = "ROC Curve - Random Forest Model",
     xlab = "Specificity", ylab = "Sensitivity")

# Add labels to the plot
text(0.6, 0.2, paste0("Threshold = ", round(coords(roc_rf, "threshold", ret = "best"), 3)))
text(0.6, 0.15, paste0("Sensitivity = ", round(sensitivity, 3)))
text(0.6, 0.1, paste0("Specificity = ", round(specificity, 3)))
  