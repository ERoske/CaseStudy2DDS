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
empTrain = read.csv("CaseStudy2-data.csv",header = TRUE)
empPredict = read.csv("CaseStudy2CompSet No Salary.csv",header = TRUE)

# Tidy Data
empTrain$SalesBasedOnRole <- recode(empTrain$JobRole,
                                    'Sales Executive'=1,
                                    'Sales Representative'=1,
                                    .default=-1)
empTrain$SalesBasedOnDept <- recode(empTrain$Department,
                                    'Sales'=1,
                                    .default=-1)
empPredict$SalesBasedOnRole <- recode(empPredict$JobRole,
                                    'Sales Executive'=1,
                                    'Sales Representative'=1,
                                    .default=-1)
empPredict$SalesBasedOnDept <- recode(empPredict$Department,
                                    'Sales'=1,
                                    .default=-1)

# Keep only helpful columns
trainEmp <- empTrain[,c(
    "MonthlyIncome",
    "DistanceFromHome",
    "JobLevel",
    "TotalWorkingYears",
    "YearsWithCurrManager",
    "SalesBasedOnRole",
    "SalesBasedOnDept",
    "ID"
)]
testEmp <- empPredict %>%
    rename(ID=ï..ID) %>%
    select(
           DistanceFromHome,
           JobLevel,
           TotalWorkingYears,
           YearsWithCurrManager,
           SalesBasedOnRole,
           SalesBasedOnDept,
           ID
           )

# Step 2: Use Random Forest
control <- trainControl(method="cv", number=10, search="grid")
rf.modelEmp <- train(MonthlyIncome~.,
                     data=trainEmp,
                     method="rf",
                     trControl=control,
                     tuneLength=5
                     )
rfClassifications <- predict(rf.modelEmp,
                             newdata=testEmp)


# Step 3: Write out results
testEmp$MonthlyIncome <- as.integer(rfClassifications)
testEmp %>%
    select(ID, MonthlyIncome)  %>%
    write.csv(file="Case2PredictionsRoske Salary.csv",
              row.names=FALSE,
              quote=FALSE)
