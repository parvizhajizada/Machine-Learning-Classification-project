# load necessary packages 
library(readr)
library(dplyr)
library(tibble)
library(corrplot)
library(ggplot2)
library(AER)
library(lmtest)
library(nnet)
library(MASS)
library(caret)
library(verification)
library(e1071)
library(janitor)
library(class)
library(kernlab)
library(verification)
library(tidyverse)
library(gmodels)
library(vcd)


# load data
wdbc <- read_csv("wdbc.csv")

# Rename columns
names(wdbc) <- c("radius1", "texture1", "perimeter1", "area1", "smoothness1", "compactness1", "concavity1", "concave_points1", "symmetry1", "fractal_dimension1",
                 "radius2", "texture2", "perimeter2", "area2", "smoothness2", "compactness2", "concavity2", "concave_points2", "symmetry2", "fractal_dimension2",
                 "radius3", "texture3", "perimeter3", "area3", "smoothness3", "compactness3", "concavity3", "concave_points3", "symmetry3", "fractal_dimension3",
                 "class")

# Factorize target variable with in a more intiutive manner (m is malignent, b is benign)
wdbc$class <- factor(wdbc$class, levels = c(1, 2), labels = c("b", "m"))

# Exploratory data analysis
# Checing for NA
any(is.na(wdbc)) # not any NA

# Checing summary statistics
summary(wdbc) 

# Checking structure of the data
str(wdbc) # all explanatory variables are numeric and response variable is binary

# Checking for potential imbalance in response variable
table(wdbc["class"]) # looks like balanced 
prop.table(table(wdbc$class)) # looks like balanced 

# Modelling
# Data Partitioning
set.seed(1)
which_train <- createDataPartition(wdbc$class, 
                                   p = 0.8, 
                                   list = FALSE) 

# Split data into training and test test 
train <- wdbc[which_train,]
test <- wdbc[-which_train,]

# Comparison of the distribution of the dependent variable in both samples
tabyl(train$class) # looks fine
tabyl(test$class) # looks fine

ctrl_cv5x3a <- trainControl(method = "repeatedcv",
                            number = 5,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary,
                            repeats = 3)

# Logistic Regression
set.seed(1)

logit.train <- 
  train(class ~ ., 
        data = train, 
        method = "glm",
        metric = "ROC",
        family = "binomial",
        trControl = ctrl_cv5x3a)

# Summary
summary(logit.train)

# Fitted values
logit.train_fitted <- predict(logit.train,
                              train,
                              type = "prob")

# Predicted values
logit.train_forecasts <- predict(logit.train,
                                 test,
                                 type = "prob")

# confusion matrix test set
confusionMatrix(data = as.factor(ifelse(logit.train_forecasts["b"] > 0.5, 
                                        "b",
                                        "m")), 
                reference = test$class, 
                positive = "b") 

# ROC test set
roc.area(ifelse(test$class == "b", 1, 0),
         logit.train_forecasts[,"b"])

# confusion matrix train set
confusionMatrix(data = as.factor(ifelse(logit.train_fitted["b"] > 0.5, 
                                        "b",
                                        "m")), 
                reference = train$class, 
                positive = "b") 

# ROC train set
roc.area(ifelse(train$class == "b", 1, 0),
         logit.train_fitted[,"b"])


# Linear Discriminant Analysis
set.seed(1)

lda.train <- 
  train(class ~ ., 
        data = train, 
        method = "lda",
        metric = "ROC",
        trControl = ctrl_cv5x3a)

# Summary
summary(lda.train)

# Fitted values
lda.train_fitted <- predict(lda.train,
                              train,
                              type = "prob")

# Predicted values
lda.train_forecasts <- predict(lda.train,
                                 test,
                                 type = "prob")

# confusion matrix test set
confusionMatrix(data = as.factor(ifelse(lda.train_forecasts["b"] > 0.5, 
                                        "b",
                                        "m")), 
                reference = test$class, 
                positive = "b") 

# ROC test set
roc.area(ifelse(test$class == "b", 1, 0),
         lda.train_forecasts[,"b"])

# confusion matrix train set
confusionMatrix(data = as.factor(ifelse(lda.train_fitted["b"] > 0.5, 
                                        "b",
                                        "m")), 
                reference = train$class, 
                positive = "b") 

# ROC train set
roc.area(ifelse(train$class == "b", 1, 0),
         lda.train_fitted[,"b"])


# Quadratic Discriminant Analysis
set.seed(1)

qda.train <- 
  train(class ~ ., 
        data = train, 
        method = "qda",
        metric = "ROC",
        trControl = ctrl_cv5x3a)

# Summary
summary(qda.train)

# Fitted values
qda.train_fitted <- predict(qda.train,
                            train,
                            type = "prob")

# Predicted values
qda.train_forecasts <- predict(qda.train,
                               test,
                               type = "prob")

# confusion matrix test set
confusionMatrix(data = as.factor(ifelse(qda.train_forecasts["b"] > 0.5, 
                                        "b",
                                        "m")), 
                reference = test$class, 
                positive = "b") 

# ROC test set
roc.area(ifelse(test$class == "b", 1, 0),
         qda.train_forecasts[,"b"])

# confusion matrix train set
confusionMatrix(data = as.factor(ifelse(qda.train_fitted["b"] > 0.5, 
                                        "b",
                                        "m")), 
                reference = train$class, 
                positive = "b") 

# ROC train set
roc.area(ifelse(train$class == "b", 1, 0),
         qda.train_fitted[,"b"])


# KNN
# Save a model formula as a separate object
model_formula <- class ~ .

# create a data frame with the values of parameter k from 1 to 50
different_k <- data.frame(k = 1:50)

ctrl_cv5a <- trainControl(method = "cv",
                          number = 5,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

set.seed(1)

train.knn_cv_scaled <- 
  train(model_formula,
        data = train, 
        method = "knn",
        trControl = ctrl_cv5a,
        tuneGrid = different_k,
        metric = "ROC",
        preProcess = c("range"))

# Summary
summary(train.knn_cv_scaled)

# plot elbow curve
plot(train.knn_cv_scaled) 

# Fitted values
knn.fitted <- predict(train.knn_cv_scaled,
                      train)

# Predicted values
knn.forecasts <- predict(train.knn_cv_scaled,
                         test)

# confusion matrix test set
confusionMatrix(data = knn.forecasts,
                reference = test$class, 
                positive = "b") 

# ROC test set
roc.area(ifelse(test$class == "b", 1, 0),
         ifelse(knn.forecasts == "b", 1, 0))

# confusion matrix train set
confusionMatrix(data = knn.fitted,
                reference = train$class,
                positive = "b")

# ROC train set
roc.area(ifelse(train$class == "b", 1, 0),
         ifelse(knn.fitted == "b", 1, 0))


# SWM
# Setting train control
ctrl_cv5x3 <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3)

# Parameters of svmLinear
modelLookup("svmLinear")

# Grid search
parametersC <- data.frame(C = c(0.001, 0.01, 0.02, 0.05, 
                                0.1, 0.2, 0.5, 1, 2, 5))

# Train data
set.seed(1)
svm_Linear <- train(class ~ ., 
                    data = train, 
                    method = "svmLinear",
                    tuneGrid = parametersC,
                    trControl = ctrl_cv5x3)

# Predicting
svm_Linear_train_forecasts <- predict(svm_Linear, 
                                      newdata = test)

# Confusion Matrix
confusionMatrix(svm_Linear_train_forecasts,
                test$class,
                positive = "b")

# Parameters of svmPoly
modelLookup("svmPoly")

# Grid Search
svm_parametersPoly <- expand.grid(C = c(0.001, 1),
                                  degree = 2:5, 
                                  scale = 1)

# Train data
set.seed(1)
svm_poly <- train(class ~ ., 
                  data = train, 
                  method = "svmPoly",
                  tuneGrid = svm_parametersPoly,
                  trControl = ctrl_cv5x3)

# Predicting
svm_poly_train_forecasts <- predict(svm_poly, 
                                    newdata = test)

# Confusion Matrix
confusionMatrix(svm_poly_train_forecasts,
                test$class,
                positive = "b")

# Parameters of svmRadial
modelLookup("svmRadial")

# Grid Search
parametersC_sigma <- 
  expand.grid(C = c(0.01, 0.05, 0.1, 0.5, 1, 5),
              sigma = c(0.05, 0.1, 0.2, 0.5, 1))

# Train data
set.seed(1)
svm_Radial <- train(class ~ ., 
                    data = train, 
                    method = "svmRadial",
                    tuneGrid = parametersC_sigma,
                    trControl = ctrl_cv5x3)

# Predicting
svm_radial_train_forecasts <- predict(svm_Radial, 
                                      newdata = test)

# Confusion Matrix
confusionMatrix(svm_radial_train_forecasts,
                test$class,
                positive = "b")


# checking for multicollinearity 
correlation <- cor(wdbc[,-c(31)])
mean((correlation > 0.5 | correlation < -0.5))
mean((correlation > 0.7 | correlation < -0.7))
mean((correlation > 0.9 | correlation < -0.9))



