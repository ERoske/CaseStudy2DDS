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
emp = read.csv("CaseStudy2-data.csv",header = TRUE)
#View(emp)

# Tidy Data
emp$Attrition.num <- recode(emp$Attrition,
                            'No'=-1,
                            'Yes'=1)
emp$BusinessTravel.num <- recode(emp$BusinessTravel,
                                 'Non-Travel'=-1,
                                 'Travel_Rarely'=0,
                                 'Travel_Frequently'=1)
emp$MaritalStatus.num <- recode(emp$MaritalStatus,
                                'Single'=-1,
                                'Married'=0,
                                'Divorced'=1)
emp$Gender.num <- recode(emp$Gender,
                         'Female'=-1,
                         'Male'=1)
emp$OverTime.num <- recode(emp$OverTime,
                           'No'=-1,
                           'Yes'=1)
emp$SalesBasedOnRole <- recode(emp$JobRole,
                               'Sales Executive'=1,
                               'Sales Representative'=1,
                               .default=-1)
emp$SalesBasedOnDept <- recode(emp$Department,
                               'Sales'=1,
                               .default=-1)
emp$EducationField.num <- as.numeric(emp$EducationField)
emp$Department.num <- as.numeric(emp$Department)

# Subset of data (only numeric)
emp.subset <- emp[,-c(1,3:4,6,9,13,17,19,22:23)]    # Get rid of non-numeric (and non-helpful) data
fit <- lm(MonthlyIncome~., emp.subset)
summary(fit)
# Based on the "fit" results, the following columns seem relevant:
# DistanceFromHome
# JobLevel
# TotalWorkingYears
# YearsWithCurrManager
# SalesBasedOnRole
# SalesBasedOnDept


trainEmp <- emp[,c(
                "MonthlyIncome",
                "DistanceFromHome",
                "JobLevel",
                "TotalWorkingYears",
                "YearsWithCurrManager",
                "SalesBasedOnRole",
                "SalesBasedOnDept",
                "ID"
)]


# Step 2: Fit Linear Regression Model
fit <- lm(MonthlyIncome~., trainEmp)
summary(fit)
predictions <- predict(fit, trainEmp)
rmse <- sqrt(mean((trainEmp$MonthlyIncome - predictions)^2))
print(rmse)
# RMSE=$1,330
# Less than $3,500!
# (That was our goal)

# Step 3: Trained Linear Regression Model
# Let's train and see if we can get even better
control <- trainControl(method="repeatedcv", number=40, repeats=3)
model <- train(MonthlyIncome~.,
               data=trainEmp,
               method="lm",
               trControl=control,
               tuneLength=5)
print(model)
# make predictions with trained model
predictions <- predict(model, trainEmp)
rmse <- sqrt(mean((trainEmp$MonthlyIncome - predictions)^2))
print(rmse)
# RMSE=1330
# Didn't improve because "train" only tunes one boolean (intercept)

# Step 4: Lasso Model
# How about Lasso?
library(elasticnet)
control <- trainControl(method="repeatedcv", number=40, repeats=3)
model <- train(MonthlyIncome~.,
               data=trainEmp,
               method="lasso",
               trControl=control,
               tuneLength=5)
print(model)
# make predictions with trained model
predictions <- predict(model, trainEmp)
rmse <- sqrt(mean((trainEmp$MonthlyIncome - predictions)^2))
print(rmse)
# RMSE=1333
# That's worse than linear regression.

# Step 5: Fit Random Forest
# How about Random Forest?
control <- trainControl(method="cv", number=10, search="grid")
model <- train(MonthlyIncome~.,
               data=trainEmp,
               method="rf",
               trControl=control,
               tuneLength=5)
print(model)
# make predictions with trained model
predictions <- predict(model, trainEmp)
rmse <- sqrt(mean((trainEmp$MonthlyIncome - predictions)^2))
print(rmse)
# RMSE=575
# That's impressive. Let's go with Random Forest

# Step 6: Graph RMSE
graphRMSE <- data.frame(
    Model=c(rep('Model',5)),
    Statistic=c('Goal', 'Linear Regression', 'Tuned Linear', 'Lasso', 'Random Forest'),
    Value= c(3000, 1330, 1330, 1333, 575)
)
graphRMSE
ggplot(graphRMSE, aes(x=Statistic, y=Value, fill=Value, label=Value)) +
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Frito-Lay: Salary Prediction by Model") +
    geom_text(vjust=-.15) +
    theme_economist() +
    theme(legend.position = "none") +
    ylab('RMSE') +
    xlab('')
