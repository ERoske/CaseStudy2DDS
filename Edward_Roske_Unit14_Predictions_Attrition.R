# MSDS6306 - Unit 14
# Edward Roske
# 2/29/2020
rm(list = ls()); options(prompt="## ") ; shell("cls")       # Cleanup everything before we start
# Tidyverse
library (tidyverse)
library (ggplot2)
library (GGally)
library (ggthemes)
# # Statistics
library (class)
library (caret)
library (e1071) #Naive Bayes
library (rpart) #Decision Tree
library (randomForest) # Random Forest
library (rattle)#fancyRpartPlot
setwd("C:/Users/erosk/Desktop/Dropbox/Personal/SMU/2019 Winter/Unit 14")

# Step 1: Load Data
#empTrain = read.csv("CaseStudy2-data.csv",header = TRUE)
empTrain = read.csv("CaseStudy2-data-with_lm.csv",header = TRUE)            # The file with 300 more observations (monthly income fitted)
empPredict = read.csv("CaseStudy2CompSet No Attrition.csv",header = TRUE)

# Tidy Data
empTrain$OverTime.num <- recode(empTrain$OverTime,
                                'No'=-1,
                                'Yes'=1)
empTrain$SalesBasedOnRole <- recode(empTrain$JobRole,
                                    'Sales Executive'=1,
                                    'Sales Representative'=1,
                                    .default=-1)
empPredict$OverTime.num <- recode(empPredict$OverTime,
                                  'No'=-1,
                                  'Yes'=1)
empPredict$SalesBasedOnRole <- recode(empPredict$JobRole,
                                      'Sales Executive'=1,
                                      'Sales Representative'=1,
                                      .default=-1)

# Keep only helpful columns
trainEmp <- empTrain[,c(
    "Attrition",
    "OverTime.num",
    "MonthlyIncome",
    "Age",
    "JobInvolvement",
    "JobLevel",
    "JobSatisfaction",
    "StockOptionLevel",
    "TotalWorkingYears",
    "YearsAtCompany",
    "YearsInCurrentRole",
    "YearsWithCurrManager",
    "SalesBasedOnRole",
    "ID"
)]
testEmp <- empPredict[,c(
    #"Attrition",
    "OverTime.num",
    "MonthlyIncome",
    "Age",
    "JobInvolvement",
    "JobLevel",
    "JobSatisfaction",
    "StockOptionLevel",
    "TotalWorkingYears",
    "YearsAtCompany",
    "YearsInCurrentRole",
    "YearsWithCurrManager",
    "SalesBasedOnRole",
    "ID"
)]


# Step 2: Use Random Forest
# Tuned settings from hyperparameter tuning
best_maxnodes <- 21
best_mtry <- 4
best_ntree <-500

# Train the best possible Random Forest model
tuneGrid <- expand.grid(.mtry = best_mtry)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
rf.modelEmp <- train(Attrition~.,
                     data=trainEmp,
                     method = "rf",
                     metric = "Accuracy",
                     tuneGrid = tuneGrid,
                     trControl = trControl,
                     importance = TRUE,
                     ntree=best_ntree,
                     #mtry=best_mtry,
                     nodesize=14,
                     maxnodes=best_maxnodes)
rfClassifications <- predict(rf.modelEmp,
                             newdata=testEmp)


# Step 3: Write out results
testEmp$Attrition <- rfClassifications
testEmp %>%
    select(ID, Attrition)  %>%
    write.csv(file="Case2PredictionsRoske Attrition.csv",
              row.names=FALSE,
              quote=FALSE)
