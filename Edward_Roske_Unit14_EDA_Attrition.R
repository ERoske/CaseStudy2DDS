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
emp = read.csv("CaseStudy2-data-with_lm.csv",header = TRUE)

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
emp.subset <- emp[,-c(1,3:4,6,9,13,17,19,23:24)]    # Get rid of non-numeric (and non-helpful) data


# Step 2: High-Level view of Employee Data
#View(emp)
str(emp)
summary(emp)

# High-Level of Employee Data (only numeric)
#View(emp.subset)
#summary(emp.subset)
#ggpairs(emp.subset)
#cor(emp.subset)
#ggcorr(emp.subset, method=c("everything", "pearson"))
#ggpairs(emp.subset,
#        columns = c(27, 1, 9, 10, 11, 12, 19, 20, 23, 24, 26, 29, 31, 32, 33))
#View(emp)

# Make a dataframe with just these columns
emp.final <- emp[,c(
    #"Attrition",
    "Attrition.num",
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
# Based on these columns, can we get accurate significance indicators?
# Use Linear Regression for a quick snapshot
fit <- lm(Attrition.num~., emp.final)
summary(fit)

# Looks good. Use these for the final columns.
emp.final <- emp[,c(
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



# Step 3: Split the observations
#set.seed(4)
split <- 0.75
trainIndices = sample(seq(1:length(emp.final$Attrition)),
                      round(split*length(emp.final$Attrition)))
trainEmp = emp.final[trainIndices,]
testEmp = emp.final[-trainIndices,]


# Step 4: Train based on split set
(modelEmp <- naiveBayes(Attrition~., data=trainEmp))
nbClassifications <- predict(modelEmp, testEmp)
table(nbClassifications, testEmp$Attrition)
(nbCM <- confusionMatrix(table(nbClassifications, testEmp$Attrition)))
# Results on a random seed
nbAccuracy <- 0.8425
nbSensitivity <- 0.8770
nbSpecificity <- 0.6250
nbSandS <- nbSensitivity + nbSpecificity


# Step 5: Use KNN
k.val <- round(sqrt(nrow(trainEmp)))    # Use sqrt of training set for k
knnClassifications <-
    knn(trainEmp[,-1],
        testEmp[,-1],
        trainEmp$Attrition,
        prob=TRUE,
        k=k.val)
table(knnClassifications, testEmp$Attrition)
(knnCM <- confusionMatrix(table(knnClassifications, testEmp$Attrition)))
# Results on a random seed
knnAccuracy <- 0.8562      # Better!
knnSensitivity <- 0.9921   # Yes!
knnSpecificity <- 0.0000   # Well, that won't work.
knnSandS <- knnSensitivity + knnSpecificity


# Step 6: Create a Regression/Decision Tree
trainEmp.Test <- emp[,c(
    "Attrition",
    "OverTime",
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
    "SalesBasedOnRole"
)]
dt.modelEmp <- rpart(Attrition~.,
                     data=trainEmp.Test,
                     method="class",
                     minbucket=50)
fancyRpartPlot(dt.modelEmp)


# Step 7: Use Random Forest
rf.modelEmp <- randomForest(Attrition~.,
                            data=trainEmp,
                            ntree=500,
                            mtry=6,
                            nodesize=50)
rfClassifications <- predict(rf.modelEmp,
                             newdata=testEmp)
table(testEmp$Attrition, rfClassifications)
(rfCM <- confusionMatrix(table(testEmp$Attrition,
                               rfClassifications)))
round(randomForest::importance(rf.modelEmp), 2)
varImp(rf.modelEmp)
# Results on a random seed
rfAccuracy <- 0.8767      # Better!
rfSensitivity <- 0.8885   # Yes!
rfSpecificity <- 0.6429   # Excellent, let's go with this model
rfSandS <- rfSensitivity + rfSpecificity

# Graph Statistics
graphStats <- data.frame(
    Model=c(rep('nb',4), rep('kNN',4), rep('rf',4)),
    Statistic=c('Accuracy', 'Sensitivity', 'Specificity', 'Sens + Spec'),
    Value= c(
        nbAccuracy,
        nbSensitivity,
        nbSpecificity,
        nbSandS,
        knnAccuracy,
        knnSensitivity,
        knnSpecificity,
        knnSandS,
        rfAccuracy,
        rfSensitivity,
        rfSpecificity,
        rfSandS
    ))
ggplot(graphStats, aes(fill=Model, x=Statistic, y=Value)) +
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Frito-Lay: Salary Prediction by Model") +
    theme_fivethirtyeight() +
    xlab('')


# Step 8: Hyperparameter optimization
# This step of code adapted from:
# https://www.guru99.com/r-random-forest-tutorial.html

# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
set.seed(1234)
# Run the model
rf_default <- train(Attrition~.,
                    data=trainEmp,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)
# Print the results
print(rf_default)

# Finetune the results
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(2:10))
rf_mtry <- train(Attrition~.,
                 data = trainEmp,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)
(best_mtry <- rf_mtry$bestTune$mtry)           # Best mtry seems to be 4
max(rf_mtry$results$Accuracy)
# Looks like mtry=4 wins
best_mtry <- 4

# Tune maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
    set.seed(1234)
    rf_maxnode <- train(Attrition~.,
                        data = trainEmp,
                        method = "rf",
                        metric = "Accuracy",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        nodesize = 14,
                        maxnodes = maxnodes,
                        ntree = 300)
    current_iteration <- toString(maxnodes)
    store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

# So far, maxnode=15 seems to be best. Try 15:25
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(15: 25)) {
    set.seed(1234)
    rf_maxnode <- train(Attrition~.,
                        data = trainEmp,
                        method = "rf",
                        metric = "Accuracy",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        nodesize = 14,
                        maxnodes = maxnodes,
                        ntree = 300)
    current_iteration <- toString(maxnodes)
    store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)
# Looks like maxnodes=21 wins
best_maxnodes <- 21

# Tune ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
    set.seed(1234)
    rf_maxtrees <- train(Attrition~.,
                         data = trainEmp,
                         method = "rf",
                         metric = "Accuracy",
                         tuneGrid = tuneGrid,
                         trControl = trControl,
                         importance = TRUE,
                         nodesize = 14,
                         maxnodes = best_maxnodes,
                         ntree = ntree)
    key <- toString(ntree)
    store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)
# Looks like ntree=500 wins
best_ntree <- 500

# Run again with tuned values
tuneGrid <- expand.grid(.mtry = best_mtry)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
rf.modelEmp <- train(Attrition~.,
                        #data=trainEmp[,-14],   # If we want to leave out ID
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
                             #newdata=testEmp[,-14])   # If we want to leave out ID
                             newdata=testEmp)
table(testEmp$Attrition, rfClassifications)
(rfCM <- confusionMatrix(table(testEmp$Attrition,
                               rfClassifications)))
round(randomForest::importance(rf.modelEmp), 2)
plot(varImp(rf.modelEmp))
